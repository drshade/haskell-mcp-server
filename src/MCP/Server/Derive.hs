{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Template Haskell utilities for deriving MCP handlers from data types.
--
-- Tool arguments are decoded from full JSON values: records may contain
-- primitive fields ('Data.Text.Text', 'Int', 'Integer', 'Double', 'Float',
-- 'Bool'), optional fields ('Maybe'), lists, enumerations (data types whose
-- constructors are all nullary), and nested records — the generated
-- @inputSchema@ mirrors the same structure. Prompt arguments are
-- string-valued per the MCP specification, so prompt records are limited to
-- primitive and enumeration fields.
module MCP.Server.Derive
  ( -- * Template Haskell Derivation
    derivePromptHandler
  , derivePromptHandlerWithDescription
  , deriveResourceHandler
  , deriveResourceHandlerWithDescription
  , deriveToolHandler
  , deriveToolHandlerWithDescription
  ) where

import           Data.Aeson          (Value (..))
import           Data.List           (intercalate)
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import           Language.Haskell.TH
import qualified Data.Char           as Char

import           MCP.Server.Derive.Internal
import           MCP.Server.Types

-------------------------------------------------------------------------------
-- Small helpers
-------------------------------------------------------------------------------

-- Helper function to convert PascalCase/camelCase to snake_case
--
-- >>> toSnakeCase "GetValue"
-- "get_value"
toSnakeCase :: String -> String
toSnakeCase [] = []
toSnakeCase (x:xs) = Char.toLower x : go xs
  where
    go [] = []
    go (c:cs)
      | Char.isUpper c = '_' : Char.toLower c : go cs
      | otherwise = c : go cs

-- Snake-cased name from a TH Name
snakeName :: Name -> String
snakeName = toSnakeCase . nameBase

-- Check if a type is Maybe, and unwrap it
unwrapMaybe :: Type -> (Bool, Type)
unwrapMaybe (AppT (ConT n) inner) | nameBase n == "Maybe" = (True, inner)
unwrapMaybe other = (False, other)

-- Look up description with a default
descriptionFor :: [(String, String)] -> String -> String -> String
descriptionFor descriptions key fallback =
  fromMaybe fallback (lookup key descriptions)

-- Helper function to convert Clause to Match
clauseToMatch :: Clause -> Match
clauseToMatch (Clause ps b ds) = Match (case ps of [p] -> p; _ -> TupP ps) b ds

-- The constructors of a named data (or newtype) declaration, if it is one
reifyCons :: Name -> Q (Maybe [Con])
reifyCons n = do
  info <- reify n
  pure $ case info of
    TyConI (DataD _ _ _ _ cs _)    -> Just cs
    TyConI (NewtypeD _ _ _ _ c _)  -> Just [c]
    _                              -> Nothing

isNullary :: Con -> Bool
isNullary (NormalC _ []) = True
isNullary _              = False

-- Get fields from a constructor: [] for nullary, record fields for RecC,
-- extracted fields for single-param NormalC
getConFields :: Con -> Q [(Name, Bang, Type)]
getConFields (NormalC _ [])                    = pure []
getConFields (RecC _ fields)                   = pure fields
getConFields (NormalC _ [(_bang, paramType)])  = extractFieldsFromParamType paramType
getConFields _                                 = fail "Unsupported constructor type"

-- Get the constructor name from a Con
conName :: Con -> Name
conName (NormalC n _) = n
conName (RecC n _)    = n
conName (InfixC _ n _) = n
conName (ForallC _ _ c) = conName c
conName (GadtC (n:_) _ _) = n
conName (RecGadtC (n:_) _ _) = n
conName _ = error "conName: empty GADT constructor list"

-- Compute required field names (non-Maybe fields)
requiredFieldNames :: [(Name, Bang, Type)] -> [String]
requiredFieldNames fields =
  [ nameBase fn | (fn, _, ft) <- fields, not (fst (unwrapMaybe ft)) ]

-- Extract field information from a (possibly wrapped) parameter record type
extractFieldsFromParamType :: Type -> Q [(Name, Bang, Type)]
extractFieldsFromParamType paramType = do
  case paramType of
    ConT typeName -> do
      cons <- reifyCons typeName
      case cons of
        Just [RecC _ fields] -> return fields
        Just [NormalC _ [(_bang, innerType)]] -> extractFieldsFromParamType innerType
        _ -> fail $ "Parameter type " ++ show typeName ++ " must be a record type or single-parameter constructor"
    _ -> fail $ "Parameter type must be a concrete type, got: " ++ show paramType

-- Names used to communicate between the generated lambda and its cases
ctxName, argNameN, argsName :: Name
ctxName  = mkName "ctx"
argNameN = mkName "name"
argsName = mkName "args"

-------------------------------------------------------------------------------
-- Value parsers (tool arguments)
-------------------------------------------------------------------------------

-- Build a parser expression of type @Value -> Either Text a@ for a field type
mkValueParser :: Type -> Q Exp
mkValueParser ty = case ty of
  ConT n | nameBase n == "Int"     -> [| valueInt |]
         | nameBase n == "Integer" -> [| valueInteger |]
         | nameBase n == "Double"  -> [| valueDouble |]
         | nameBase n == "Float"   -> [| valueFloat |]
         | nameBase n == "Bool"    -> [| valueBool |]
         | nameBase n == "Text"    -> [| valueText |]
  AppT ListT inner -> [| valueArray $(mkValueParser inner) |]
  ConT n -> do
    cons <- reifyCons n
    case cons of
      Just cs | not (null cs), all isNullary cs -> mkEnumValueParser n cs
      Just [RecC icon ifields] -> mkObjectParser n icon ifields
      Just [NormalC icon [(_bang, inner)]] -> [| fmap $(conE icon) . $(mkValueParser inner) |]
      _ -> fail $ "Unsupported tool argument type: " ++ show n
  _ -> fail $ "Unsupported tool argument type: " ++ show ty

-- Parser for an enumeration: a JSON string matching a snake_cased constructor
mkEnumValueParser :: Name -> [Con] -> Q Exp
mkEnumValueParser tyName cons = do
  textParser <- mkEnumTextParser tyName cons
  [| \v -> case v of
       String s -> $(return textParser) s
       _        -> Left ("Failed to parse " <> $(litE $ stringL $ nameBase tyName) <> " from: " <> renderValue v) |]

-- Parser of type @Text -> Either Text a@ for an enumeration
mkEnumTextParser :: Name -> [Con] -> Q Exp
mkEnumTextParser tyName cons = do
  let expected = intercalate ", " [snakeName (conName c) | c <- cons]
  sVar <- newName "s"
  matches <- mapM (enumMatch . conName) cons
  fallback <- [| Left ("Failed to parse " <> $(litE $ stringL $ nameBase tyName)
                       <> " from: " <> $(varE sVar)
                       <> " (expected one of: " <> $(litE $ stringL expected) <> ")") |]
  let defaultMatch = Match WildP (NormalB fallback) []
  return $ LamE [VarP sVar] $
    CaseE (AppE (VarE 'T.unpack) (VarE sVar)) (matches ++ [defaultMatch])
  where
    enumMatch cn = do
      body <- [| Right $(conE cn) |]
      return $ Match (LitP $ StringL $ snakeName cn) (NormalB body) []

-- Parser for a nested record: a JSON object decoded field by field
mkObjectParser :: Name -> Name -> [(Name, Bang, Type)] -> Q Exp
mkObjectParser tyName icon ifields = do
  vVar <- newName "v"
  oVar <- newName "o"
  chain <- foldl (\acc f -> [| $acc <*> $(fieldExtractor oVar f) |])
                 [| pure $(conE icon) |]
                 ifields
  fallback <- [| Left ("Failed to parse " <> $(litE $ stringL $ nameBase tyName)
                       <> " object from: " <> renderValue $(varE vVar)) |]
  objP <- conP 'Object [varP oVar]
  return $ LamE [VarP vVar] $
    CaseE (VarE vVar)
      [ Match objP (NormalB chain) []
      , Match WildP (NormalB fallback) []
      ]
  where
    fieldExtractor oVar (fieldName, _, fieldType) = do
      let (isOptional, innerType) = unwrapMaybe fieldType
      let fname = litE $ stringL $ nameBase fieldName
      if isOptional
        then [| optionalField $fname $(mkValueParser innerType) $(varE oVar) |]
        else [| requiredField $fname $(mkValueParser innerType) $(varE oVar) |]

-------------------------------------------------------------------------------
-- Text parsers (prompt arguments)
-------------------------------------------------------------------------------

-- Build a parser expression of type @Text -> Either Text a@ for a prompt field
mkTextParser :: Type -> Q Exp
mkTextParser ty = case ty of
  ConT n | nameBase n == "Int"     -> [| parseInt |]
         | nameBase n == "Integer" -> [| parseInteger |]
         | nameBase n == "Double"  -> [| parseDouble |]
         | nameBase n == "Float"   -> [| parseFloat |]
         | nameBase n == "Bool"    -> [| parseBool |]
         | nameBase n == "Text"    -> [| parseText |]
  ConT n -> do
    cons <- reifyCons n
    case cons of
      Just cs | not (null cs), all isNullary cs -> mkEnumTextParser n cs
      _ -> fail $ "Unsupported prompt argument type (prompt arguments are strings): " ++ show n
  _ -> fail $ "Unsupported prompt argument type (prompt arguments are strings): " ++ show ty

-------------------------------------------------------------------------------
-- Argument decoding for a constructor
-------------------------------------------------------------------------------

data ArgStyle = ToolArgs | PromptArgs

-- Build a decoder expression of type @Either Error <ConType>@ reading from
-- the in-scope arguments map bound to 'argsName'.
mkConDecoder :: ArgStyle -> Con -> Q Exp
mkConDecoder _ (NormalC cn []) = [| Right $(conE cn) |]
mkConDecoder style (RecC cn fields) = mkFieldsDecoder style (conE cn) fields
mkConDecoder style (NormalC cn [(_bang, paramType)]) =
  wrapParam paramType
  where
    wrapParam ty = case ty of
      ConT typeName -> do
        cons <- reifyCons typeName
        case cons of
          Just [RecC icon ifields] -> do
            inner <- mkFieldsDecoder style (conE icon) ifields
            [| $(conE cn) <$> $(return inner) |]
          Just [NormalC icon [(_b, innerTy)]] -> do
            innerCon <- mkConDecoder style (NormalC icon [(_b, innerTy)])
            [| $(conE cn) <$> $(return innerCon) |]
          _ -> fail $ "Parameter type " ++ show typeName ++ " must be a record type or single-parameter constructor"
      _ -> fail $ "Parameter type must be a concrete type, got: " ++ show ty
mkConDecoder _ con = fail $ "Unsupported constructor type: " ++ show (conName con)

-- Applicative chain over the constructor's fields, reading from 'argsName'
mkFieldsDecoder :: ArgStyle -> Q Exp -> [(Name, Bang, Type)] -> Q Exp
mkFieldsDecoder style con fields =
  foldl (\acc f -> [| $acc <*> $(argExtractor f) |]) [| pure $con |] fields
  where
    argExtractor (fieldName, _, fieldType) = do
      let (isOptional, innerType) = unwrapMaybe fieldType
      let fname = litE $ stringL $ nameBase fieldName
      case style of
        ToolArgs
          | isOptional -> [| optionalArg $fname $(mkValueParser innerType) $(varE argsName) |]
          | otherwise  -> [| requiredArg $fname $(mkValueParser innerType) $(varE argsName) |]
        PromptArgs
          | isOptional -> [| optionalTextArg $fname $(mkTextParser innerType) $(varE argsName) |]
          | otherwise  -> [| requiredTextArg $fname $(mkTextParser innerType) $(varE argsName) |]

-------------------------------------------------------------------------------
-- Schema generation
-------------------------------------------------------------------------------

-- Build a 'SchemaType' expression for a field type
mkSchemaShape :: [(String, String)] -> Type -> Q Exp
mkSchemaShape descriptions ty = case ty of
  ConT n | nameBase n == "Int"     -> [| SchemaInteger |]
         | nameBase n == "Integer" -> [| SchemaInteger |]
         | nameBase n == "Double"  -> [| SchemaNumber |]
         | nameBase n == "Float"   -> [| SchemaNumber |]
         | nameBase n == "Bool"    -> [| SchemaBoolean |]
         | nameBase n == "Text"    -> [| SchemaString Nothing |]
  AppT ListT inner ->
    [| SchemaArray (Schema Nothing $(mkSchemaShape descriptions inner)) |]
  ConT n -> do
    cons <- reifyCons n
    case cons of
      Just cs | not (null cs), all isNullary cs -> do
        let vals = listE [litE $ stringL $ snakeName (conName c) | c <- cs]
        [| SchemaString (Just $vals) |]
      Just [RecC _ ifields] -> mkObjectShape descriptions ifields
      Just [NormalC _ [(_bang, inner)]] -> mkSchemaShape descriptions inner
      _ -> fail $ "Unsupported tool argument type: " ++ show n
  _ -> fail $ "Unsupported tool argument type: " ++ show ty

-- Build a 'SchemaType' object expression from record fields
mkObjectShape :: [(String, String)] -> [(Name, Bang, Type)] -> Q Exp
mkObjectShape descriptions fields = do
  props <- mapM prop fields
  let reqd = listE [litE $ stringL fn | fn <- requiredFieldNames fields]
  [| SchemaObject $(return $ ListE props) $reqd |]
  where
    prop (fieldName, _, fieldType) = do
      let fieldStr = nameBase fieldName
      let description = descriptionFor descriptions fieldStr fieldStr
      let (_, innerType) = unwrapMaybe fieldType
      [| ( $(litE $ stringL fieldStr)
         , Schema (Just $(litE $ stringL description)) $(mkSchemaShape descriptions innerType)
         ) |]

-------------------------------------------------------------------------------
-- Prompt derivation
-------------------------------------------------------------------------------

-- | Derive prompt handlers from a data type with custom descriptions.
-- Usage:
--
-- > $(derivePromptHandlerWithDescription ''MyPrompt 'handlePrompt [("Constructor", "Description")])
derivePromptHandlerWithDescription :: Name -> Name -> [(String, String)] -> Q Exp
derivePromptHandlerWithDescription typeName handlerName descriptions = do
  cons <- reifyCons typeName
  case cons of
    Just constructors -> do
      -- Generate prompt definitions
      promptDefs <- traverse (mkPromptDefWithDescription descriptions) constructors

      -- Generate list handler
      listHandlerExp <- [| \_ctx -> pure $(return $ ListE promptDefs) |]

      -- Generate get handler with cases
      cases <- traverse (mkDispatchCase PromptArgs handlerName) constructors
      defaultCase <- [| pure $ Left $ InvalidPromptName $ "Unknown prompt: " <> $(varE argNameN) |]
      let defaultMatch = Match WildP (NormalB defaultCase) []
      let getHandlerExp = LamE [VarP ctxName, VarP argNameN, VarP argsName] $
            CaseE (AppE (VarE 'T.unpack) (VarE argNameN))
              (map clauseToMatch cases ++ [defaultMatch])

      return $ TupE [Just listHandlerExp, Just getHandlerExp]
    Nothing -> fail $ "derivePromptHandlerWithDescription: " ++ show typeName ++ " is not a data type"

-- | Derive prompt handlers from a data type.
-- Usage:
--
-- > $(derivePromptHandler ''MyPrompt 'handlePrompt)
derivePromptHandler :: Name -> Name -> Q Exp
derivePromptHandler typeName handlerName =
  derivePromptHandlerWithDescription typeName handlerName []

mkPromptDefWithDescription :: [(String, String)] -> Con -> Q Exp
mkPromptDefWithDescription descriptions con = do
  let name = conName con
  let sname = snakeName name
  let description = descriptionFor descriptions (nameBase name) ("Handle " ++ nameBase name)
  fields <- getConFields con
  args <- traverse (mkArgDef descriptions) fields
  [| PromptDefinition
      { promptDefinitionName = $(litE $ stringL sname)
      , promptDefinitionDescription = $(litE $ stringL description)
      , promptDefinitionArguments = $(return $ ListE args)
      , promptDefinitionTitle = Nothing
      } |]

mkArgDef :: [(String, String)] -> (Name, Bang, Type) -> Q Exp
mkArgDef descriptions (fieldName, _, fieldType) = do
  let isOptional = fst (unwrapMaybe fieldType)
  let fieldNameStr = nameBase fieldName
  let description = descriptionFor descriptions fieldNameStr fieldNameStr
  [| ArgumentDefinition
      { argumentDefinitionName = $(litE $ stringL fieldNameStr)
      , argumentDefinitionDescription = $(litE $ stringL description)
      , argumentDefinitionRequired = $(if isOptional then [| False |] else [| True |])
      } |]

-------------------------------------------------------------------------------
-- Dispatch case generation (shared by prompt and tool)
-------------------------------------------------------------------------------

mkDispatchCase :: ArgStyle -> Name -> Con -> Q Clause
mkDispatchCase style handlerName con = do
  let sname = snakeName (conName con)
  decoder <- mkConDecoder style con
  body <- case style of
    ToolArgs ->
      [| case $(return decoder) of
           Left err -> pure (Left err)
           Right v  -> do
             r <- $(varE handlerName) $(varE ctxName) v
             pure (Right (toToolResult r)) |]
    PromptArgs ->
      [| case $(return decoder) of
           Left err -> pure (Left err)
           Right v  -> do
             r <- $(varE handlerName) $(varE ctxName) v
             pure (Right (toPromptResult r)) |]
  clause [litP $ stringL sname] (normalB (return body)) []

-------------------------------------------------------------------------------
-- Resource derivation
-------------------------------------------------------------------------------

-- | Derive resource handlers from a data type with custom descriptions.
-- Usage:
--
-- > $(deriveResourceHandlerWithDescription ''MyResource 'handleResource [("Constructor", "Description")])
deriveResourceHandlerWithDescription :: Name -> Name -> [(String, String)] -> Q Exp
deriveResourceHandlerWithDescription typeName handlerName descriptions = do
  cons <- reifyCons typeName
  case cons of
    Just constructors -> do
      -- Generate resource definitions
      resourceDefs <- traverse (mkResourceDefWithDescription descriptions) constructors
      listHandlerExp <- [| \_ctx -> pure $(return $ ListE resourceDefs) |]

      -- Generate read handler with cases
      cases <- traverse (mkResourceCase handlerName) constructors
      defaultCase <- [| pure $ Left $ ResourceNotFound $ "Resource not found: " <> T.pack unknown |]
      let defaultMatch = Match (VarP (mkName "unknown")) (NormalB defaultCase) []

      let readHandlerExp = LamE [VarP ctxName, VarP (mkName "uri")] $
            CaseE (AppE (VarE 'show) (VarE (mkName "uri")))
              (map clauseToMatch cases ++ [defaultMatch])

      return $ TupE [Just listHandlerExp, Just readHandlerExp]
    Nothing -> fail $ "deriveResourceHandlerWithDescription: " ++ show typeName ++ " is not a data type"

-- | Derive resource handlers from a data type.
-- Usage:
--
-- > $(deriveResourceHandler ''MyResource 'handleResource)
deriveResourceHandler :: Name -> Name -> Q Exp
deriveResourceHandler typeName handlerName =
  deriveResourceHandlerWithDescription typeName handlerName []

mkResourceDefWithDescription :: [(String, String)] -> Con -> Q Exp
mkResourceDefWithDescription descriptions (NormalC name []) = do
  let resourceName = T.pack . snakeName $ name
  let resourceURI = "resource://" <> T.unpack resourceName
  let constructorName = nameBase name
  let description = case lookup constructorName descriptions of
        Just desc -> Just desc
        Nothing   -> Just constructorName
  [| ResourceDefinition
      { resourceDefinitionURI = $(litE $ stringL resourceURI)
      , resourceDefinitionName = $(litE $ stringL $ T.unpack resourceName)
      , resourceDefinitionDescription = $(case description of
          Just desc -> [| Just $(litE $ stringL desc) |]
          Nothing   -> [| Nothing |])
      , resourceDefinitionMimeType = Just "text/plain"
      , resourceDefinitionTitle = Nothing
      } |]
mkResourceDefWithDescription _ _ = fail "Unsupported constructor type for resources"


mkResourceCase :: Name -> Con -> Q Clause
mkResourceCase handlerName (NormalC name []) = do
  let resourceName = T.pack . snakeName $ name
  let resourceURI = "resource://" <> T.unpack resourceName
  clause [litP $ stringL resourceURI]
    (normalB [| Right <$> $(varE handlerName) $(varE ctxName) $(varE (mkName "uri")) $(conE name) |])
    []
mkResourceCase _ _ = fail "Unsupported constructor type for resources"

-------------------------------------------------------------------------------
-- Tool derivation
-------------------------------------------------------------------------------

-- | Derive tool handlers from a data type with custom descriptions.
-- Usage:
--
-- > $(deriveToolHandlerWithDescription ''MyTool 'handleTool [("Constructor", "Description")])
deriveToolHandlerWithDescription :: Name -> Name -> [(String, String)] -> Q Exp
deriveToolHandlerWithDescription typeName handlerName descriptions = do
  cons <- reifyCons typeName
  case cons of
    Just constructors -> do
      -- Generate tool definitions
      toolDefs <- traverse (mkToolDefWithDescription descriptions) constructors

      listHandlerExp <- [| \_ctx -> pure $(return $ ListE toolDefs) |]

      -- Generate call handler with cases
      cases <- traverse (mkDispatchCase ToolArgs handlerName) constructors
      defaultCase <- [| pure $ Left $ UnknownTool $ "Unknown tool: " <> $(varE argNameN) |]
      let defaultMatch = Match WildP (NormalB defaultCase) []
      let callHandlerExp = LamE [VarP ctxName, VarP argNameN, VarP argsName] $
            CaseE (AppE (VarE 'T.unpack) (VarE argNameN))
              (map clauseToMatch cases ++ [defaultMatch])

      return $ TupE [Just listHandlerExp, Just callHandlerExp]
    Nothing -> fail $ "deriveToolHandlerWithDescription: " ++ show typeName ++ " is not a data type"

-- | Derive tool handlers from a data type.
-- Usage:
--
-- > $(deriveToolHandler ''MyTool 'handleTool)
deriveToolHandler :: Name -> Name -> Q Exp
deriveToolHandler typeName handlerName =
  deriveToolHandlerWithDescription typeName handlerName []

mkToolDefWithDescription :: [(String, String)] -> Con -> Q Exp
mkToolDefWithDescription descriptions con = do
  let name = conName con
  let sname = snakeName name
  let description = descriptionFor descriptions (nameBase name) (nameBase name)
  fields <- getConFields con
  shapeExp <- mkObjectShape descriptions fields
  [| ToolDefinition
      { toolDefinitionName = $(litE $ stringL sname)
      , toolDefinitionDescription = $(litE $ stringL description)
      , toolDefinitionInputSchema = Schema Nothing $(return shapeExp)
      , toolDefinitionOutputSchema = Nothing
      , toolDefinitionTitle = Nothing
      } |]
