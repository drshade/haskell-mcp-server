{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Text (Text)

-- High-level data type definitions from SPEC.md

data MyPrompt
    = Recipe { idea :: Text }
    | Shopping { description :: Text }
    deriving (Show, Eq)

data MyResource
    = ProductCategories
    | SaleItems
    | HeadlineBannerAd
    -- A record constructor becomes a resource template:
    -- resource://product_detail/{productSku}
    | ProductDetail { productSku :: Text }
    deriving (Show, Eq)

-- An enumeration argument: all-nullary data types become string enums in
-- the generated schema ("standard", "express", "overnight")
data ShippingSpeed
    = Standard
    | Express
    | Overnight
    deriving (Show, Eq)

-- A nested record argument: appears as a JSON object in the schema
data Address = Address
    { street :: Text
    , city   :: Text
    , zipCode :: Maybe Text
    } deriving (Show, Eq)

data MyTool
    = SearchForProduct { q :: Text, category :: Maybe Text }
    | AddToCart { sku :: Text, quantities :: [Int] }
    | Checkout { speed :: ShippingSpeed, shipTo :: Address }
    | ComplexTool { field1 :: Text, field2 :: Text, field3 :: Maybe Text, field4 :: Text, field5 :: Maybe Text }
    deriving (Show, Eq)
