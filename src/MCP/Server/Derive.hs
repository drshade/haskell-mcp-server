{-# LANGUAGE DeriveLift        #-}
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
  , derivePromptHandlerWithOptions
  , deriveResourceHandler
  , deriveResourceHandlerWithDescription
  , deriveResourceHandlerWithOptions
  , deriveResourceTemplates
  , deriveResourceTemplatesWithDescription
  , deriveResourceTemplatesWithOptions
  , deriveToolHandler
  , deriveToolHandlerWithDescription
  , deriveToolHandlerWithOptions
  , deriveToolHandlerWithOutput
  , deriveToolHandlerWithOutputDescription
  , deriveToolHandlerWithOutputOptions

    -- * Per-constructor customization
  , DefinitionOptions(..)
  , defaultDefinitionOptions
  ) where

import           Control.Monad       (zipWithM)
import           Data.Aeson          (Value (..))
import           Data.List           (intercalate)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (Lift, lift)
import qualified Data.Char           as Char

import           MCP.Server.Derive.Internal
import           MCP.Server.Types

-- | Per-constructor customization for the @WithOptions@ derivations: a
-- single mechanism for everything a definition can carry beyond its shape.
-- Entries are looked up by constructor name; for output-typed tool
-- derivations, an entry keyed by the /output type's/ name supplies its
-- field descriptions.
data DefinitionOptions = DefinitionOptions
  { optDescription       :: Maybe Text
    -- ^ The definition's description (falls back to the constructor name)
  , optTitle             :: Maybe Text
  , optIcons             :: [Icon]
  , optToolAnnotations   :: Maybe ToolAnnotations
    -- ^ Behavioral hints; meaningful for tools, ignored elsewhere
  , optFieldDescriptions :: [(Text, Text)]
    -- ^ Field\/argument descriptions, scoped to this constructor — two
    -- constructors may describe a same-named field differently
  } deriving (Show, Eq, Lift)

-- | Nothing customized; record-update what you need.
defaultDefinitionOptions :: DefinitionOptions
defaultDefinitionOptions = DefinitionOptions
  { optDescription = Nothing
  , optTitle = Nothing
  , optIcons = []
  , optToolAnnotations = Nothing
  , optFieldDescriptions = []
  }

-- | Look up a constructor's options.
optionsFor :: [(String, DefinitionOptions)] -> String -> DefinitionOptions
optionsFor opts name = fromMaybe defaultDefinitionOptions (lookup name opts)

-- | Adapt the legacy flat description list to per-constructor options:
-- constructor entries become 'optDescription'; the whole list also serves
-- as the (unscoped) field-description namespace, preserving the old
-- behavior exactly.
optionsFromDescriptions :: [(String, String)] -> String -> DefinitionOptions
optionsFromDescriptions descriptions name = defaultDefinitionOptions
  { optDescription = T.pack <$> lookup name descriptions
  }

-- The merged field-description namespace for one constructor: its scoped
-- entries shadow the legacy global list.
fieldDescsFor :: DefinitionOptions -> [(String, String)] -> [(String, String)]
fieldDescsFor opts globalDescs =
  [(T.unpack k, T.unpack v) | (k, v) <- optFieldDescriptions opts] ++ globalDescs

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
ctxName, argNameN, argsName, uriName :: Name
ctxName  = mkName "ctx"
argNameN = mkName "name"
argsName = mkName "args"
uriName  = mkName "uri"

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
      Just [RecC icn ifields] -> mkObjectParser n icn ifields
      Just [NormalC icn [(_bang, inner)]] -> [| fmap $(conE icn) . $(mkValueParser inner) |]
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
mkObjectParser tyName icn ifields = do
  vVar <- newName "v"
  oVar <- newName "o"
  chain <- foldl (\acc f -> [| $acc <*> $(fieldExtractor oVar f) |])
                 [| pure $(conE icn) |]
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
-- Value serializers (structured output)
-------------------------------------------------------------------------------

-- Build a serializer expression of type @a -> Value@ that mirrors
-- 'mkSchemaShape' exactly: what the schema promises is what the value
-- contains (snake_cased enums, 'Maybe' fields omitted when 'Nothing').
mkValueBuilder :: Type -> Q Exp
mkValueBuilder ty = case ty of
  ConT n | nameBase n `elem` ["Int", "Integer", "Double", "Float", "Bool", "Text"] ->
    [| toJSON |]
  AppT ListT inner ->
    [| \xs -> toJSON (map $(mkValueBuilder inner) xs) |]
  ConT n -> do
    cons <- reifyCons n
    case cons of
      Just cs | not (null cs), all isNullary cs -> do
        vVar <- newName "v"
        matches <- mapM enumMatch cs
        return $ LamE [VarP vVar] $ CaseE (VarE vVar) matches
      Just [RecC icn ifields] -> mkRecordBuilder icn ifields
      Just [NormalC icn [(_bang, inner)]] -> do
        xVar <- newName "x"
        wrapped <- conP icn [varP xVar]
        body <- [| $(mkValueBuilder inner) $(varE xVar) |]
        return $ LamE [wrapped] body
      _ -> fail $ "Unsupported output field type: " ++ show n
  _ -> fail $ "Unsupported output field type: " ++ show ty
  where
    enumMatch c = do
      let cn = conName c
      body <- [| String $(litE $ stringL $ snakeName cn) |]
      pat <- conP cn []
      return $ Match pat (NormalB body) []

-- Serializer for a record: an object with one entry per field, omitting
-- 'Maybe' fields that are 'Nothing'. Fields are bound by pattern-matching
-- the constructor — never by accessor application, which would be ambiguous
-- for users with DuplicateRecordFields enabled.
mkRecordBuilder :: Name -> [(Name, Bang, Type)] -> Q Exp
mkRecordBuilder con fields = do
  vars <- mapM (const $ newName "f") fields
  pat <- conP con (map varP vars)
  fieldExps <- zipWithM fieldPairs vars fields
  body <- [| object (concat $(return $ ListE fieldExps)) |]
  return $ LamE [pat] body
  where
    fieldPairs var (fieldName, _, fieldType) = do
      let (isOptional, innerType) = unwrapMaybe fieldType
      let fname = litE $ stringL $ nameBase fieldName
      if isOptional
        then [| omitNothing $fname $(mkValueBuilder innerType) $(varE var) |]
        else [| [ $fname .= $(mkValueBuilder innerType) $(varE var) ] |]

-- The output type must (possibly through single-constructor wrappers)
-- resolve to a record: outputSchema is an object schema per the spec.
requireOutputRecord :: Name -> Q ()
requireOutputRecord n = do
  cons <- reifyCons n
  case cons of
    Just [RecC _ (_:_)] -> pure ()
    Just [NormalC _ [(_bang, ConT inner)]] -> requireOutputRecord inner
    _ -> fail $ "Output type " ++ show n
           ++ " must be a record (outputSchema is an object schema)"

-------------------------------------------------------------------------------
-- Argument decoding for a constructor
-------------------------------------------------------------------------------

data ArgStyle = ToolArgs | ToolArgsOutput Exp | PromptArgs

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
          Just [RecC icn ifields] -> do
            inner <- mkFieldsDecoder style (conE icn) ifields
            [| $(conE cn) <$> $(return inner) |]
          Just [NormalC icn [(_b, innerTy)]] -> do
            innerCon <- mkConDecoder style (NormalC icn [(_b, innerTy)])
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
        PromptArgs
          | isOptional -> [| optionalTextArg $fname $(mkTextParser innerType) $(varE argsName) |]
          | otherwise  -> [| requiredTextArg $fname $(mkTextParser innerType) $(varE argsName) |]
        _ -- tool styles share the Value-based argument decoding
          | isOptional -> [| optionalArg $fname $(mkValueParser innerType) $(varE argsName) |]
          | otherwise  -> [| requiredArg $fname $(mkValueParser innerType) $(varE argsName) |]

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
derivePromptHandlerWithDescription typeName handlerName descriptions =
  derivePromptHandlerGeneric typeName handlerName (optionsFromDescriptions descriptions) descriptions

-- | Derive prompt handlers with per-constructor 'DefinitionOptions'
-- (title, icons, scoped argument descriptions). Usage:
--
-- > $(derivePromptHandlerWithOptions ''MyPrompt 'handlePrompt
-- >     [("Recipe", defaultDefinitionOptions { optDescription = Just "…" })])
derivePromptHandlerWithOptions :: Name -> Name -> [(String, DefinitionOptions)] -> Q Exp
derivePromptHandlerWithOptions typeName handlerName opts =
  derivePromptHandlerGeneric typeName handlerName (optionsFor opts) []

derivePromptHandlerGeneric :: Name -> Name -> (String -> DefinitionOptions) -> [(String, String)] -> Q Exp
derivePromptHandlerGeneric typeName handlerName conOpts globalDescs = do
  cons <- reifyCons typeName
  case cons of
    Just constructors -> do
      -- Generate prompt definitions
      promptDefs <- traverse (mkPromptDef conOpts globalDescs) constructors

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
    Nothing -> fail $ "derivePromptHandler: " ++ show typeName ++ " is not a data type"

-- | Derive prompt handlers from a data type.
-- Usage:
--
-- > $(derivePromptHandler ''MyPrompt 'handlePrompt)
derivePromptHandler :: Name -> Name -> Q Exp
derivePromptHandler typeName handlerName =
  derivePromptHandlerWithDescription typeName handlerName []

mkPromptDef :: (String -> DefinitionOptions) -> [(String, String)] -> Con -> Q Exp
mkPromptDef conOpts globalDescs con = do
  let name = conName con
  let sname = snakeName name
  let opts = conOpts (nameBase name)
  let description = fromMaybe (T.pack ("Handle " ++ nameBase name)) (optDescription opts)
  fields <- getConFields con
  args <- traverse (mkArgDef (fieldDescsFor opts globalDescs)) fields
  [| PromptDefinition
      { promptDefinitionName = $(litE $ stringL sname)
      , promptDefinitionDescription = $(lift description)
      , promptDefinitionArguments = $(return $ ListE args)
      , promptDefinitionTitle = $(lift (optTitle opts))
      , promptDefinitionIcons = $(lift (optIcons opts))
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
    ToolArgsOutput builder ->
      [| case $(return decoder) of
           Left err -> pure (Left err)
           Right v  -> do
             r <- $(varE handlerName) $(varE ctxName) v
             pure (Right (resolveToolOutput $(return builder) r)) |]
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
--
-- Nullary constructors become static resources
-- (@Menu -> resource:\/\/menu@). Record constructors become resource
-- /templates/ (@UserProfile { userId :: Text } ->
-- resource:\/\/user_profile\/{userId}@): they are excluded from
-- @resources\/list@ (advertise them with 'deriveResourceTemplates'), and the
-- read handler matches their URIs by splitting the path into percent-decoded
-- segments, one per field, in declaration order.
--
-- Usage:
--
-- > $(deriveResourceHandlerWithDescription ''MyResource 'handleResource [("Constructor", "Description")])
deriveResourceHandlerWithDescription :: Name -> Name -> [(String, String)] -> Q Exp
deriveResourceHandlerWithDescription typeName handlerName descriptions =
  deriveResourceHandlerGeneric typeName handlerName (optionsFromDescriptions descriptions)

-- | Derive resource handlers with per-constructor 'DefinitionOptions'
-- (description, title, icons).
deriveResourceHandlerWithOptions :: Name -> Name -> [(String, DefinitionOptions)] -> Q Exp
deriveResourceHandlerWithOptions typeName handlerName opts =
  deriveResourceHandlerGeneric typeName handlerName (optionsFor opts)

deriveResourceHandlerGeneric :: Name -> Name -> (String -> DefinitionOptions) -> Q Exp
deriveResourceHandlerGeneric typeName handlerName conOpts = do
  cons <- reifyCons typeName
  case cons of
    Just constructors -> do
      -- Static resource definitions: nullary constructors only
      resourceDefs <- traverse (mkResourceDef conOpts)
                               (filter isNullary constructors)
      listHandlerExp <- [| \_ctx -> pure $(return $ ListE resourceDefs) |]

      -- Read handler: a chain of alternatives over every constructor,
      -- falling through to ResourceNotFound
      let fallbackQ = [| pure $ Left $ ResourceNotFound $
                           "Resource not found: " <> T.pack (show $(varE uriName)) |]
      readBody <- foldr (mkResourceAlt handlerName) fallbackQ constructors
      let readHandlerExp = LamE [VarP ctxName, VarP uriName] readBody

      return $ TupE [Just listHandlerExp, Just readHandlerExp]
    Nothing -> fail $ "deriveResourceHandler: " ++ show typeName ++ " is not a data type"

-- | Derive resource handlers from a data type.
-- Usage:
--
-- > $(deriveResourceHandler ''MyResource 'handleResource)
deriveResourceHandler :: Name -> Name -> Q Exp
deriveResourceHandler typeName handlerName =
  deriveResourceHandlerWithDescription typeName handlerName []

-- | Derive a 'ResourceTemplateListHandler' advertising the record
-- constructors of a resource type as URI templates, with custom
-- descriptions. Usage:
--
-- > $(deriveResourceTemplatesWithDescription ''MyResource [("Constructor", "Description")])
deriveResourceTemplatesWithDescription :: Name -> [(String, String)] -> Q Exp
deriveResourceTemplatesWithDescription typeName descriptions =
  deriveResourceTemplatesGeneric typeName (optionsFromDescriptions descriptions)

-- | Derive a 'ResourceTemplateListHandler' with per-constructor
-- 'DefinitionOptions' (description, title, icons).
deriveResourceTemplatesWithOptions :: Name -> [(String, DefinitionOptions)] -> Q Exp
deriveResourceTemplatesWithOptions typeName opts =
  deriveResourceTemplatesGeneric typeName (optionsFor opts)

deriveResourceTemplatesGeneric :: Name -> (String -> DefinitionOptions) -> Q Exp
deriveResourceTemplatesGeneric typeName conOpts = do
  cons <- reifyCons typeName
  case cons of
    Just constructors -> do
      templateDefs <- traverse (mkTemplateDef conOpts)
                               [c | c@(RecC _ _) <- constructors]
      [| \_ctx -> pure $(return $ ListE templateDefs) |]
    Nothing -> fail $ "deriveResourceTemplates: " ++ show typeName ++ " is not a data type"

-- | Derive a 'ResourceTemplateListHandler' from a resource type's record
-- constructors. Usage:
--
-- > $(deriveResourceTemplates ''MyResource)
deriveResourceTemplates :: Name -> Q Exp
deriveResourceTemplates typeName =
  deriveResourceTemplatesWithDescription typeName []

-- The URI-template string for a record constructor:
-- resource://user_profile/{userId}/{...}
templateURI :: Name -> [(Name, Bang, Type)] -> String
templateURI name fields =
  "resource://" <> snakeName name
    <> concat ["/{" <> nameBase fn <> "}" | (fn, _, _) <- fields]

mkTemplateDef :: (String -> DefinitionOptions) -> Con -> Q Exp
mkTemplateDef _ (RecC name []) =
  fail $ "Resource template constructors need at least one field: " ++ nameBase name
mkTemplateDef conOpts (RecC name fields) = do
  let opts = conOpts (nameBase name)
  let description = fromMaybe (T.pack (nameBase name)) (optDescription opts)
  [| ResourceTemplateDefinition
      { resourceTemplateURITemplate = $(litE $ stringL $ templateURI name fields)
      , resourceTemplateName = $(litE $ stringL $ snakeName name)
      , resourceTemplateDescription = Just $(lift description)
      , resourceTemplateMimeType = Just "text/plain"
      , resourceTemplateTitle = $(lift (optTitle opts))
      , resourceTemplateIcons = $(lift (optIcons opts))
      } |]
mkTemplateDef _ _ = fail "Resource templates require record constructors"

mkResourceDef :: (String -> DefinitionOptions) -> Con -> Q Exp
mkResourceDef conOpts (NormalC name []) = do
  let resourceName = T.pack . snakeName $ name
  let resourceURI = "resource://" <> T.unpack resourceName
  let opts = conOpts (nameBase name)
  let description = fromMaybe (T.pack (nameBase name)) (optDescription opts)
  [| ResourceDefinition
      { resourceDefinitionURI = $(litE $ stringL resourceURI)
      , resourceDefinitionName = $(litE $ stringL $ T.unpack resourceName)
      , resourceDefinitionDescription = Just $(lift description)
      , resourceDefinitionMimeType = Just "text/plain"
      , resourceDefinitionTitle = $(lift (optTitle opts))
      , resourceDefinitionIcons = $(lift (optIcons opts))
      } |]
mkResourceDef _ _ = fail "Unsupported constructor type for resources"

-- One alternative of the read handler: try this constructor, else fall
-- through to the rest of the chain.
mkResourceAlt :: Name -> Con -> Q Exp -> Q Exp
mkResourceAlt handlerName con restQ = case con of
  NormalC name [] -> do
    let resourceURI = "resource://" <> snakeName name
    [| if show $(varE uriName) == $(litE $ stringL resourceURI)
         then Right <$> $(varE handlerName) $(varE ctxName) $(varE uriName) $(conE name)
         else $restQ |]
  RecC name [] ->
    fail $ "Resource template constructors need at least one field: " ++ nameBase name
  RecC name fields -> do
    let prefix = "resource://" <> snakeName name <> "/"
        segsName = mkName "segs"
    decoder <- mkSegmentsDecoder segsName name fields
    handled <- [| case $(return decoder) of
                    Left err -> pure (Left err)
                    Right v  -> Right <$> $(varE handlerName) $(varE ctxName) $(varE uriName) v |]
    rest <- restQ
    matchExp <- [| matchTemplateSegments $(litE $ stringL prefix)
                     $(litE $ integerL $ fromIntegral $ length fields)
                     (show $(varE uriName)) |]
    justP <- conP 'Just [varP segsName]
    nothingP <- conP 'Nothing []
    return $ CaseE matchExp
      [ Match justP (NormalB handled) []
      , Match nothingP (NormalB rest) []
      ]
  _ -> fail "Unsupported constructor type for resources"

-- Applicative decode of the matched template segments into the constructor
mkSegmentsDecoder :: Name -> Name -> [(Name, Bang, Type)] -> Q Exp
mkSegmentsDecoder segsName con fields =
  foldl step [| pure $(conE con) |] (zip [0 :: Integer ..] fields)
  where
    step acc (i, (fieldName, _, fieldType)) = do
      let (isOptional, innerType) = unwrapMaybe fieldType
      if isOptional
        then fail $ "Maybe fields are not supported in resource templates: " ++ nameBase fieldName
        else [| $acc <*> templateField $(litE $ stringL $ nameBase fieldName)
                        $(mkTextParser innerType)
                        $(varE segsName)
                        $(litE $ integerL i) |]

-------------------------------------------------------------------------------
-- Tool derivation
-------------------------------------------------------------------------------

-- | Derive tool handlers from a data type with custom descriptions.
-- Usage:
--
-- > $(deriveToolHandlerWithDescription ''MyTool 'handleTool [("Constructor", "Description")])
deriveToolHandlerWithDescription :: Name -> Name -> [(String, String)] -> Q Exp
deriveToolHandlerWithDescription typeName handlerName descriptions =
  deriveToolHandlerGeneric typeName handlerName (optionsFromDescriptions descriptions) descriptions Nothing

-- | Derive tool handlers with per-constructor 'DefinitionOptions'
-- (description, title, icons, behavioral annotations, scoped argument
-- descriptions). Usage:
--
-- > $(deriveToolHandlerWithOptions ''MyTool 'handleTool
-- >     [ ("Search", defaultDefinitionOptions
-- >         { optDescription = Just "Search the catalog"
-- >         , optToolAnnotations = Just defaultToolAnnotations { toolReadOnlyHint = Just True }
-- >         })
-- >     ])
deriveToolHandlerWithOptions :: Name -> Name -> [(String, DefinitionOptions)] -> Q Exp
deriveToolHandlerWithOptions typeName handlerName opts =
  deriveToolHandlerGeneric typeName handlerName (optionsFor opts) [] Nothing

-- | Derive tool handlers from a data type.
-- Usage:
--
-- > $(deriveToolHandler ''MyTool 'handleTool)
deriveToolHandler :: Name -> Name -> Q Exp
deriveToolHandler typeName handlerName =
  deriveToolHandlerWithDescription typeName handlerName []

-- | Derive tool handlers with typed, structured output: the third name is a
-- record type describing the tools' results. Its shape becomes the derived
-- @outputSchema@ (same field rules as input derivation: primitives,
-- 'Maybe', lists, all-nullary enums, nested records), and the handler
-- returns @'ToolOutput' \<output\>@, whose typed values the derivation
-- serializes into @structuredContent@ — guaranteed to match the schema.
--
-- > handleTool :: ClientContext -> MyTool -> IO (ToolOutput WeatherReport)
-- > $(deriveToolHandlerWithOutput ''MyTool 'handleTool ''WeatherReport)
deriveToolHandlerWithOutput :: Name -> Name -> Name -> Q Exp
deriveToolHandlerWithOutput typeName handlerName outputName =
  deriveToolHandlerWithOutputDescription typeName handlerName outputName []

-- | 'deriveToolHandlerWithOutput' with custom descriptions. The
-- description list is shared with the output type: entries matching output
-- field names describe the @outputSchema@ properties.
deriveToolHandlerWithOutputDescription :: Name -> Name -> Name -> [(String, String)] -> Q Exp
deriveToolHandlerWithOutputDescription typeName handlerName outputName descriptions =
  deriveToolHandlerGeneric typeName handlerName (optionsFromDescriptions descriptions) descriptions (Just outputName)

-- | 'deriveToolHandlerWithOutput' with per-constructor
-- 'DefinitionOptions'. An entry keyed by the output type's name supplies
-- the @outputSchema@ field descriptions via 'optFieldDescriptions'.
deriveToolHandlerWithOutputOptions :: Name -> Name -> Name -> [(String, DefinitionOptions)] -> Q Exp
deriveToolHandlerWithOutputOptions typeName handlerName outputName opts =
  deriveToolHandlerGeneric typeName handlerName (optionsFor opts) [] (Just outputName)

deriveToolHandlerGeneric :: Name -> Name -> (String -> DefinitionOptions) -> [(String, String)] -> Maybe Name -> Q Exp
deriveToolHandlerGeneric typeName handlerName conOpts globalDescs outputName = do
  cons <- reifyCons typeName
  case cons of
    Just constructors -> do
      -- The output type's schema and its matching serializer, when typed
      -- output is requested. Its field descriptions come from an options
      -- entry keyed by the output type's own name, merged with the legacy
      -- global list.
      output <- traverse
        (\o -> do
          requireOutputRecord o
          let outDescs = fieldDescsFor (conOpts (nameBase o)) globalDescs
          (,) <$> mkSchemaShape outDescs (ConT o)
              <*> mkValueBuilder (ConT o))
        outputName

      -- Generate tool definitions
      toolDefs <- traverse (mkToolDef conOpts globalDescs (fst <$> output)) constructors

      listHandlerExp <- [| \_ctx -> pure $(return $ ListE toolDefs) |]

      -- Generate call handler with cases
      let style = maybe ToolArgs (ToolArgsOutput . snd) output
      cases <- traverse (mkDispatchCase style handlerName) constructors
      defaultCase <- [| pure $ Left $ UnknownTool $ "Unknown tool: " <> $(varE argNameN) |]
      let defaultMatch = Match WildP (NormalB defaultCase) []
      let callHandlerExp = LamE [VarP ctxName, VarP argNameN, VarP argsName] $
            CaseE (AppE (VarE 'T.unpack) (VarE argNameN))
              (map clauseToMatch cases ++ [defaultMatch])

      return $ TupE [Just listHandlerExp, Just callHandlerExp]
    Nothing -> fail $ "deriveToolHandler: " ++ show typeName ++ " is not a data type"

mkToolDef :: (String -> DefinitionOptions) -> [(String, String)] -> Maybe Exp -> Con -> Q Exp
mkToolDef conOpts globalDescs outputShape con = do
  let name = conName con
  let sname = snakeName name
  let opts = conOpts (nameBase name)
  let description = fromMaybe (T.pack (nameBase name)) (optDescription opts)
  fields <- getConFields con
  shapeExp <- mkObjectShape (fieldDescsFor opts globalDescs) fields
  [| ToolDefinition
      { toolDefinitionName = $(litE $ stringL sname)
      , toolDefinitionDescription = $(lift description)
      , toolDefinitionInputSchema = Schema Nothing $(return shapeExp)
      , toolDefinitionOutputSchema = $(case outputShape of
          Just shape -> [| Just (Schema Nothing $(return shape)) |]
          Nothing    -> [| Nothing |])
      , toolDefinitionTitle = $(lift (optTitle opts))
      , toolDefinitionAnnotations = $(lift (optToolAnnotations opts))
      , toolDefinitionIcons = $(lift (optIcons opts))
      } |]
