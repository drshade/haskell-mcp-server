{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Coverage for raw JSON Schema keywords (ADR 0014).
module Spec.RawSchema (spec) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import MCP.Server
import Test.Hspec

-- | Keywords none of the other shapes can express: a nullable union and a
-- recursive definition.
treeKeywords :: Object
treeKeywords = KM.fromList
  [ "$defs" .= object
      [ "tree" .= object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "label" .= object ["type" .= ("string" :: Text)]
              , "children" .= object ["type" .= ("array" :: Text), "items" .= object ["$ref" .= ("#/$defs/tree" :: Text)]]
              ]
          , "required" .= (["label", "children"] :: [Text])
          ]
      ]
  , "type" .= ("object" :: Text)
  , "properties" .= object
      [ "root" .= object ["anyOf" .= [object ["type" .= ("null" :: Text)], object ["$ref" .= ("#/$defs/tree" :: Text)]]]
      ]
  , "required" .= (["root"] :: [Text])
  ]

spec :: Spec
spec = describe "Raw schemas" $ do
  it "serializes raw keywords as given" $
    toJSON (schema (SchemaRaw treeKeywords)) `shouldBe` Object treeKeywords

  it "adds a description, replacing one in the keywords" $
    toJSON (describedSchema "Outer" (SchemaRaw (KM.fromList ["type" .= ("integer" :: Text), "description" .= ("Inner" :: Text)])))
      `shouldBe` object ["type" .= ("integer" :: Text), "description" .= ("Outer" :: Text)]

  it "nests inside the other shapes" $
    toJSON (schema (SchemaObject [("count", schema (SchemaRaw (KM.fromList ["type" .= ("integer" :: Text), "minimum" .= (0 :: Int)])))] ["count"]))
      `shouldBe` object
        [ "type" .= ("object" :: Text)
        , "properties" .= object ["count" .= object ["type" .= ("integer" :: Text), "minimum" .= (0 :: Int)]]
        , "required" .= (["count"] :: [Text])
        ]

  it "reaches the wire as a tool's input schema" $ do
    let definition = mkToolDefinition "plant" "Plant a tree" (schema (SchemaRaw treeKeywords))
    toJSON definition `shouldSatisfy` \case
      Object fields -> KM.lookup "inputSchema" fields == Just (Object treeKeywords)
      _ -> False
