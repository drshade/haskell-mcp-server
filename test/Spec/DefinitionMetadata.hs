{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Coverage for definition metadata (ADR 0006): tool annotations, icons,
-- content annotations, and the options-based derive customization.
module Spec.DefinitionMetadata (spec) where

import Data.Aeson
import Data.Text (Text)
import MCP.Server
import MCP.Server.Derive
import Test.Hspec
import TestTypes

annotatedToolHandlers :: (ToolListHandler, ToolCallHandler)
annotatedToolHandlers = $(deriveToolHandlerWithOptions ''TestTool 'handleTestTool
  [ ("Echo", defaultDefinitionOptions
      { optDescription = Just "Echoes the input"
      , optTitle = Just "Echo"
      , optIcons = [icon "https://example.com/echo.png"]
      , optToolAnnotations = Just defaultToolAnnotations
          { toolReadOnlyHint = Just True
          , toolIdempotentHint = Just True
          }
      , optFieldDescriptions = [("text", "What to echo")]
      })
  , ("Calculate", defaultDefinitionOptions
      { optFieldDescriptions = [("x", "Calculate's first operand")]
      })
  ])

toolNamed :: Text -> [ToolDefinition] -> ToolDefinition
toolNamed n defs = case [d | d <- defs, toolDefinitionName d == n] of
  (d:_) -> d
  []    -> error $ "tool not found: " ++ show n

propDescription :: Text -> ToolDefinition -> Maybe Text
propDescription name def = case schemaShape (toolDefinitionInputSchema def) of
  SchemaObject props _ -> lookup name props >>= schemaDescription
  _                    -> Nothing

spec :: Spec
spec = describe "Definition metadata" $ do

  describe "JSON shapes" $ do
    it "serializes tool annotations with only the set hints" $ do
      toJSON defaultToolAnnotations { toolReadOnlyHint = Just True } `shouldBe`
        object ["readOnlyHint" .= True]

    it "serializes icons, omitting empty optional fields" $ do
      toJSON (icon "https://example.com/i.png") `shouldBe`
        object ["src" .= ("https://example.com/i.png" :: Text)]
      toJSON (Icon "u" (Just "image/png") ["48x48"]) `shouldBe`
        object ["src" .= ("u" :: Text), "mimeType" .= ("image/png" :: Text), "sizes" .= (["48x48"] :: [Text])]

    it "merges content annotations into the inner block" $ do
      let anns = defaultAnnotations { annotationsAudience = [RoleUser], annotationsPriority = Just 0.8 }
      toJSON (ContentAnnotated anns (ContentText "hi")) `shouldBe`
        object [ "type" .= ("text" :: Text), "text" .= ("hi" :: Text)
               , "annotations" .= object ["audience" .= (["user"] :: [Text]), "priority" .= (0.8 :: Double)]
               ]

    it "round-trips annotated content through FromJSON" $ do
      let anns = defaultAnnotations { annotationsAudience = [RoleAssistant] }
          c = ContentAnnotated anns (ContentText "hello")
      decode (encode c) `shouldBe` Just c

  describe "Options-based derivation" $ do
    it "carries description, title, icons and annotations on the definition" $ do
      defs <- fst annotatedToolHandlers anonCtx
      let echoDef = toolNamed "echo" defs
      toolDefinitionDescription echoDef `shouldBe` "Echoes the input"
      toolDefinitionTitle echoDef `shouldBe` Just "Echo"
      toolDefinitionIcons echoDef `shouldBe` [icon "https://example.com/echo.png"]
      toolDefinitionAnnotations echoDef `shouldBe` Just defaultToolAnnotations
        { toolReadOnlyHint = Just True
        , toolIdempotentHint = Just True
        }

    it "leaves uncustomized constructors bare (constructor-name description)" $ do
      defs <- fst annotatedToolHandlers anonCtx
      let toggleDef = toolNamed "toggle" defs
      toolDefinitionDescription toggleDef `shouldBe` "Toggle"
      toolDefinitionAnnotations toggleDef `shouldBe` Nothing
      toolDefinitionIcons toggleDef `shouldBe` []

    it "scopes field descriptions to their constructor" $ do
      defs <- fst annotatedToolHandlers anonCtx
      -- 'text' described only on Echo; 'x' described only on Calculate
      propDescription "text" (toolNamed "echo" defs) `shouldBe` Just "What to echo"
      propDescription "x" (toolNamed "calculate" defs) `shouldBe` Just "Calculate's first operand"
      -- Echo's options must not leak onto Calculate's same-named args
      propDescription "operation" (toolNamed "calculate" defs) `shouldBe` Just "operation"

    it "still dispatches calls unchanged" $ do
      result <- snd annotatedToolHandlers anonCtx "echo" mempty
      case result of
        Left (MissingRequiredParams msg) -> show msg `shouldContain` "text"
        other -> expectationFailure $ "expected missing-params error, got: " ++ show other