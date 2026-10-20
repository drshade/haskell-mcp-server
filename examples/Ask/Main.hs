{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Tools that ask the user for input part-way through (multi round-trip
-- requests, MCP 2026-07-28). Run it, then watch stderr while a client
-- books a table: the handler replays from the top on every round, but each
-- 'step' runs once in a normal booking, its result replayed afterwards.
--
-- 'step' is at-least-once, though: a client that retries a round with an
-- earlier state (say, after losing a reply) runs the steps after that point
-- again. A real 'confirmBooking' must therefore be idempotent, for example
-- by keying the booking on something the retry repeats.
module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import           MCP.Server
import           MCP.Server.Derive
import           System.IO              (hPutStrLn, hSetEncoding, stderr,
                                         stdout, utf8)
import           Types

-- Returning 'Ask' instead of 'IO' is all it takes to ask questions.
handleTool :: ClientContext -> BookingTool -> Ask Content
handleTool _ (BookTable place) = do
    -- Ask for the details. 'elicit' picks the form from the type the
    -- answer is used at: BookingDetails.
    answer <- elicit ("Booking a table at " <> place <> ". What do you need?")
    case answer of
        Accepted details -> do
            -- Work between questions goes in 'step': it runs once, and
            -- its result is replayed on later rounds.
            offer <- step (findTable place details)
            ok <- elicit ("Book " <> offer <> "?")
            case ok of
                Accepted (Confirm True) -> do
                    ref <- step (confirmBooking offer)
                    pure $ ContentText $ "Booked " <> offer <> ". Your reference is " <> ref <> "."
                _ -> pure $ ContentText "No problem, nothing was booked."
        Declined  -> pure $ ContentText "No problem, nothing was booked."
        Cancelled -> pure $ ContentText "Booking cancelled."
        -- The client cannot show forms (an older protocol revision, or no
        -- elicitation capability): no round trip happens, so say what to do.
        Unavailable -> pure $ ContentText $
            "I can't ask for booking details in this client. Please call "
                <> place <> " to book."

handleTool _ (CancelBooking ref) = do
    ok <- elicit ("Cancel booking " <> ref <> "? This cannot be undone.")
    case ok of
        Accepted (Confirm True) -> do
            _ <- step (cancelBooking ref)
            pure $ ContentText $ "Booking " <> ref <> " is cancelled."
        -- Cancelling without confirmation would be destructive: refuse.
        Unavailable -> pure $ ContentText "Cancelling needs confirmation, which this client can't ask for."
        _ -> pure $ ContentText $ "Booking " <> ref <> " is unchanged."

-- A tool that never asks finishes in one round; plain IO goes through liftIO.
handleTool _ (OpeningHours place) =
    ContentText <$> liftIO (lookupHours place)

-- The "back end". Each logs to stderr so you can see when it runs.

findTable :: Text -> BookingDetails -> IO Text
findTable place details = do
    logLine $ "finding a table at " <> place <> " for " <> T.pack (show details)
    pure $ "a table for " <> T.pack (show (partySize details)) <> " at " <> place
        <> ", " <> slotTime (slot details)
        <> (if null (dietary details) then "" else " (kitchen notified: "
              <> T.intercalate ", " (map dietLabel (dietary details)) <> ")")
        <> (if highChair details == Just True then " with a high chair" else "")

-- Not idempotent: a retried final round would book again under a new
-- reference. Fine for a demo; see the note at the top of the module.
confirmBooking :: Text -> IO Text
confirmBooking offer = do
    now <- getPOSIXTime
    let ref = "BK-" <> T.pack (show (floor now `mod` (100000 :: Integer)))
    logLine $ "booked " <> offer <> " as " <> ref
    pure ref

cancelBooking :: Text -> IO ()
cancelBooking ref = logLine $ "cancelled " <> ref

lookupHours :: Text -> IO Text
lookupHours place = pure $ place <> " is open 12:00-15:00 and 17:30-23:00, Tuesday to Sunday."

slotTime :: Slot -> Text
slotTime Lunch       = "13:00"
slotTime EarlyDinner = "18:00"
slotTime LateDinner  = "20:30"

dietLabel :: Diet -> Text
dietLabel Vegetarian = "vegetarian"
dietLabel Vegan      = "vegan"
dietLabel GlutenFree = "gluten-free"
dietLabel NutAllergy = "nut allergy"

logLine :: Text -> IO ()
logLine = hPutStrLn stderr . ("[bookings] " <>) . T.unpack

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    runMcpServerStdio
        McpServerInfo
            { serverName = "Table Booking Example"
            , serverVersion = "0.1.0"
            , serverInstructions = "Books restaurant tables, asking the user for the details."
            }
        noHandlers
            { tools = Just $(deriveToolHandlerWithOptions ''BookingTool 'handleTool
                [ ("BookTable", defaultDefinitionOptions
                    { optDescription = Just "Book a table at a restaurant; asks the user for the details" })
                , ("CancelBooking", defaultDefinitionOptions
                    { optDescription = Just "Cancel a booking by its reference, after confirming with the user"
                    , optToolAnnotations = Just defaultToolAnnotations { toolDestructiveHint = Just True } })
                , ("OpeningHours", defaultDefinitionOptions
                    { optDescription = Just "A restaurant's opening hours"
                    , optToolAnnotations = Just defaultToolAnnotations { toolReadOnlyHint = Just True } })
                ])
            }
