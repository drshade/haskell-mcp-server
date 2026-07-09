{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Template Haskell utilities for deriving MCP handlers from data types.
module MCP.Server.Derive
  ( -- * Template Haskell Derivation
    derivePromptHandler
  , derivePromptHandlerWithDescription
  , deriveResourceHandler
  , deriveResourceHandlerWithDescription
  , deriveToolHandler
  , deriveToolHandlerWithDescription
  ) where

import qualified Data.Map            as Map
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

-- Map a Haskell type to its JSON schema type string
jsonTypeFor :: Type -> String
jsonTypeFor (ConT n)
  | nameBase n == "Int"     = "integer"
  | nameBase n == "Integer" = "integer"
  | nameBase n == "Double"  = "number"
  | nameBase n == "Float"   = "number"
  | nameBase n == "Bool"    = "boolean"
jsonTypeFor _ = "string"

-- Helper function to convert Clause to Match
clauseToMatch :: Clause -> Match
clauseToMatch (Clause ps b ds) = Match (case ps of [p] -> p; _ -> TupP ps) b ds

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

-------------------------------------------------------------------------------
-- Prompt derivation
-------------------------------------------------------------------------------

-- | Derive prompt handlers from a data type with custom descriptions.
-- Usage:
--
-- > $(derivePromptHandlerWithDescription ''MyPrompt 'handlePrompt [("Constructor", "Description")])
derivePromptHandlerWithDescription :: Name -> Name -> [(String, String)] -> Q Exp
derivePromptHandlerWithDescription typeName handlerName descriptions = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ constructors _) -> do
      -- Generate prompt definitions
      promptDefs <- traverse (mkPromptDefWithDescription descriptions) constructors

      -- Generate list handler
      listHandlerExp <- [| \_ctx -> pure $(return $ ListE promptDefs) |]

      -- Generate get handler with cases
      cases <- traverse (mkDispatchCase handlerName) constructors
      defaultCase <- [| pure $ Left $ InvalidPromptName $ "Unknown prompt: " <> name |]
      let defaultMatch = Match WildP (NormalB defaultCase) []
      let getHandlerExp = LamE [VarP (mkName "ctx"), VarP (mkName "name"), VarP (mkName "args")] $
            CaseE (AppE (VarE 'T.unpack) (VarE (mkName "name")))
              (map clauseToMatch cases ++ [defaultMatch])

      return $ TupE [Just listHandlerExp, Just getHandlerExp]
    _ -> fail $ "derivePromptHandlerWithDescription: " ++ show typeName ++ " is not a data type"

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
  args <- case con of
    NormalC _ []                   -> pure []
    RecC _ fields                  -> traverse (mkArgDef descriptions) fields
    NormalC _ [(_bang, paramType)] -> extractFieldsFromType descriptions paramType
    _                              -> fail "Unsupported constructor type"
  [| PromptDefinition
      { promptDefinitionName = $(litE $ stringL sname)
      , promptDefinitionDescription = $(litE $ stringL description)
      , promptDefinitionArguments = $(return $ ListE args)
      , promptDefinitionTitle = Nothing
      } |]

-- Extract field definitions from a parameter type recursively
extractFieldsFromType :: [(String, String)] -> Type -> Q [Exp]
extractFieldsFromType descriptions paramType = do
  case paramType of
    ConT typeName -> do
      info <- reify typeName
      case info of
        TyConI (DataD _ _ _ _ [RecC _ fields] _) ->
          -- Parameter type is a record with fields
          traverse (mkArgDef descriptions) fields
        TyConI (DataD _ _ _ _ [NormalC _ [(_bang, innerType)]] _) ->
          -- Parameter type has a single parameter - recurse
          extractFieldsFromType descriptions innerType
        _ -> fail $ "Parameter type " ++ show typeName ++ " must be a record type or single-parameter constructor"
    _ -> fail $ "Parameter type must be a concrete type, got: " ++ show paramType

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

mkDispatchCase :: Name -> Con -> Q Clause
mkDispatchCase handlerName con = do
  let name = conName con
  let sname = snakeName name
  body <- case con of
    NormalC _ [] ->
      [| do
          content <- $(varE handlerName) $(varE (mkName "ctx")) $(conE name)
          pure $ Right content |]
    RecC _ fields ->
      mkRecordCase name handlerName fields
    NormalC _ [(_bang, paramType)] ->
      mkSeparateParamsCase name handlerName paramType
    _ -> fail "Unsupported constructor type"
  clause [litP $ stringL sname] (normalB (return body)) []

-------------------------------------------------------------------------------
-- Field validation
-------------------------------------------------------------------------------

mkSeparateParamsCase :: Name -> Name -> Type -> Q Exp
mkSeparateParamsCase outerConName handlerName paramType = do
  fields <- extractFieldsFromParamType paramType
  let argMapName = mkName "argMap"
  let mkBaseExp fieldVars = do
        paramConstructorApp <- buildParameterConstructor paramType fieldVars
        let outerConstructorApp = AppE (ConE outerConName) paramConstructorApp
        [| do
            content <- $(varE handlerName) $(varE (mkName "ctx")) $(return outerConstructorApp)
            pure $ Right content |]
  inner <- buildFieldValidation argMapName handlerName mkBaseExp fields 0
  [| let $(varP argMapName) = Map.fromList args in $(return inner) |]

mkRecordCase :: Name -> Name -> [(Name, Bang, Type)] -> Q Exp
mkRecordCase recConName handlerName fields = do
  case fields of
    [] -> [| do
        content <- $(varE handlerName) $(varE (mkName "ctx")) $(conE recConName)
        pure $ Right content |]
    _ -> do
      let argMapName = mkName "argMap"
      let mkBaseExp fieldVars = do
            let constructorApp = foldl AppE (ConE recConName) (map VarE fieldVars)
            [| do
                content <- $(varE handlerName) $(varE (mkName "ctx")) $(return constructorApp)
                pure $ Right content |]
      inner <- buildFieldValidation argMapName handlerName mkBaseExp fields 0
      [| let $(varP argMapName) = Map.fromList args in $(return inner) |]

-- Build nested case expressions for field validation, supporting any number of fields.
-- The mkBaseExp callback receives field variable names and builds the final expression.
buildFieldValidation :: Name -> Name -> ([Name] -> Q Exp) -> [(Name, Bang, Type)] -> Int -> Q Exp
buildFieldValidation _argMap _handlerName mkBaseExp [] depth = do
  let fieldVars = [mkName ("field" ++ show i) | i <- [0..depth-1]]
  mkBaseExp fieldVars

buildFieldValidation argMap handlerName mkBaseExp ((fieldName, _, fieldType):remainingFields) depth = do
  let fieldStr = nameBase fieldName
  let (isOptional, innerType) = unwrapMaybe fieldType
  let fieldVar = mkName ("field" ++ show depth)

  continuation <- buildFieldValidation argMap handlerName mkBaseExp remainingFields (depth + 1)

  let parseFunc = mkParseFunc innerType

  let fieldPrefix = litE $ stringL $ "field '" <> fieldStr <> "': "

  if isOptional
    then do
      [| case Map.lookup $(litE $ stringL fieldStr) $(varE argMap) of
            Nothing -> do
              let $(varP fieldVar) = Nothing
              $(return continuation)
            Just raw -> case $(parseFunc) raw of
              Left err -> pure $ Left $ InvalidParams ($fieldPrefix <> err)
              Right parsed -> do
                let $(varP fieldVar) = Just parsed
                $(return continuation) |]
    else do
      [| case Map.lookup $(litE $ stringL fieldStr) $(varE argMap) of
            Just raw -> case $(parseFunc) raw of
              Left err -> pure $ Left $ InvalidParams ($fieldPrefix <> err)
              Right $(varP fieldVar) -> $(return continuation)
            Nothing -> pure $ Left $ MissingRequiredParams $(litE $ stringL $ "field '" <> fieldStr <> "' is missing") |]

-- Extract field information from parameter type
extractFieldsFromParamType :: Type -> Q [(Name, Bang, Type)]
extractFieldsFromParamType paramType = do
  case paramType of
    ConT typeName -> do
      info <- reify typeName
      case info of
        TyConI (DataD _ _ _ _ [RecC _ fields] _) ->
          return fields
        TyConI (DataD _ _ _ _ [NormalC _ [(_bang, innerType)]] _) ->
          extractFieldsFromParamType innerType
        _ -> fail $ "Parameter type " ++ show typeName ++ " must be a record type or single-parameter constructor"
    _ -> fail $ "Parameter type must be a concrete type, got: " ++ show paramType

-- Build the parameter constructor application recursively
buildParameterConstructor :: Type -> [Name] -> Q Exp
buildParameterConstructor paramType fieldVars = do
  case paramType of
    ConT typeName -> do
      info <- reify typeName
      case info of
        TyConI (DataD _ _ _ _ [RecC cn _] _) ->
          -- Record constructor - apply all field variables
          return $ foldl AppE (ConE cn) (map VarE fieldVars)
        TyConI (DataD _ _ _ _ [NormalC cn [(_bang, innerType)]] _) -> do
          -- Single parameter constructor - recurse and wrap
          innerConstructor <- buildParameterConstructor innerType fieldVars
          return $ AppE (ConE cn) innerConstructor
        _ -> fail $ "Parameter type " ++ show typeName ++ " must be a record type or single-parameter constructor"
    _ -> fail $ "Parameter type must be a concrete type, got: " ++ show paramType

-- Select the appropriate parse helper for a given type
mkParseFunc :: Type -> Q Exp
mkParseFunc innerType = case innerType of
  ConT n | nameBase n == "Int"     -> [| parseInt |]
  ConT n | nameBase n == "Integer" -> [| parseInteger |]
  ConT n | nameBase n == "Double"  -> [| parseDouble |]
  ConT n | nameBase n == "Float"   -> [| parseFloat |]
  ConT n | nameBase n == "Bool"    -> [| parseBool |]
  _                                -> [| parseText |]

-------------------------------------------------------------------------------
-- Resource derivation
-------------------------------------------------------------------------------

-- | Derive resource handlers from a data type with custom descriptions.
-- Usage:
--
-- > $(deriveResourceHandlerWithDescription ''MyResource 'handleResource [("Constructor", "Description")])
deriveResourceHandlerWithDescription :: Name -> Name -> [(String, String)] -> Q Exp
deriveResourceHandlerWithDescription typeName handlerName descriptions = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ constructors _) -> do
      -- Generate resource definitions
      resourceDefs <- traverse (mkResourceDefWithDescription descriptions) constructors
      listHandlerExp <- [| \_ctx -> pure $(return $ ListE resourceDefs) |]

      -- Generate read handler with cases
      cases <- traverse (mkResourceCase handlerName) constructors
      defaultCase <- [| pure $ Left $ ResourceNotFound $ "Resource not found: " <> T.pack unknown |]
      let defaultMatch = Match (VarP (mkName "unknown")) (NormalB defaultCase) []

      let readHandlerExp = LamE [VarP (mkName "ctx"), VarP (mkName "uri")] $
            CaseE (AppE (VarE 'show) (VarE (mkName "uri")))
              (map clauseToMatch cases ++ [defaultMatch])

      return $ TupE [Just listHandlerExp, Just readHandlerExp]
    _ -> fail $ "deriveResourceHandlerWithDescription: " ++ show typeName ++ " is not a data type"

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
      , resourceDefinitionTitle = Nothing  -- 2025-06-18: New title field
      } |]
mkResourceDefWithDescription _ _ = fail "Unsupported constructor type for resources"


mkResourceCase :: Name -> Con -> Q Clause
mkResourceCase handlerName (NormalC name []) = do
  let resourceName = T.pack . snakeName $ name
  let resourceURI = "resource://" <> T.unpack resourceName
  clause [litP $ stringL resourceURI]
    (normalB [| Right <$> $(varE handlerName) $(varE (mkName "ctx")) $(varE (mkName "uri")) $(conE name) |])
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
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ constructors _) -> do
      -- Generate tool definitions
      toolDefs <- traverse (mkToolDefWithDescription descriptions) constructors

      listHandlerExp <- [| \_ctx -> pure $(return $ ListE toolDefs) |]

      -- Generate call handler with cases
      cases <- traverse (mkDispatchCase handlerName) constructors
      defaultCase <- [| pure $ Left $ UnknownTool $ "Unknown tool: " <> name |]
      let defaultMatch = Match WildP (NormalB defaultCase) []
      let callHandlerExp = LamE [VarP (mkName "ctx"), VarP (mkName "name"), VarP (mkName "args")] $
            CaseE (AppE (VarE 'T.unpack) (VarE (mkName "name")))
              (map clauseToMatch cases ++ [defaultMatch])

      return $ TupE [Just listHandlerExp, Just callHandlerExp]
    _ -> fail $ "deriveToolHandlerWithDescription: " ++ show typeName ++ " is not a data type"

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
  props <- traverse (mkProperty descriptions) fields
  let required = requiredFieldNames fields
  [| ToolDefinition
      { toolDefinitionName = $(litE $ stringL sname)
      , toolDefinitionDescription = $(litE $ stringL description)
      , toolDefinitionInputSchema = InputSchemaDefinitionObject
          { properties = $(return $ ListE props)
          , required = $(return $ ListE $ map (LitE . StringL) required)
          }
      , toolDefinitionTitle = Nothing
      } |]

mkProperty :: [(String, String)] -> (Name, Bang, Type) -> Q Exp
mkProperty descriptions (fieldName, _, fieldType) = do
  let fieldStr = nameBase fieldName
  let description = descriptionFor descriptions fieldStr fieldStr
  let (_, innerType) = unwrapMaybe fieldType
  let jsonType = jsonTypeFor innerType
  [| ($(litE $ stringL fieldStr), InputSchemaDefinitionProperty
      { propertyType = $(litE $ stringL jsonType)
      , propertyDescription = $(litE $ stringL description)
      }) |]
