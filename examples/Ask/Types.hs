{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Data.Text         (Text)
import           MCP.Server.Derive

-- The tools. Declared above the deriveForm splices below: a splice only
-- sees the types declared above the last declaration splice.
data BookingTool
    = BookTable { restaurant :: Text }
    | CancelBooking { reference :: Text }
    | OpeningHours { restaurant :: Text }
    deriving (Show, Eq)

-- The forms: plain records whose answers the handlers want back.

-- An enumeration is offered as a choice ("lunch", "early_dinner", ...)
data Slot = Lunch | EarlyDinner | LateDinner
    deriving (Show, Eq)

data Diet = Vegetarian | Vegan | GlutenFree | NutAllergy
    deriving (Show, Eq)

data BookingDetails = BookingDetails
    { partySize :: Int
    , slot      :: Slot
    , dietary   :: [Diet]       -- a list of an enumeration: pick several
    , highChair :: Maybe Bool   -- Maybe: the user may leave it out
    } deriving (Show, Eq)

-- A yes/no question is a one-field form
newtype Confirm = Confirm { confirmed :: Bool }
    deriving (Show, Eq)

-- Labels, help text and starting values for the form's fields. A default
-- that does not fit its field is a compile-time error.
$(deriveFormWithOptions ''BookingDetails
    [ ("partySize", defaultFormFieldOptions
        { formFieldTitle = Just "Party size", formFieldDefault = Just (Number 2) })
    , ("slot", defaultFormFieldOptions
        { formFieldTitle = Just "Time", formFieldDefault = Just "early_dinner" })
    , ("dietary", defaultFormFieldOptions
        { formFieldTitle = Just "Dietary requirements"
        , formFieldDescription = Just "Tick any that apply to your party" })
    , ("highChair", defaultFormFieldOptions { formFieldTitle = Just "Need a high chair?" })
    ])

$(deriveFormWithDescription ''Confirm [("confirmed", "Tick to confirm")])
