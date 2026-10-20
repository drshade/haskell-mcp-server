{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Forms and handlers for the derived-interface tests of 'Ask' (ADR 0009,
-- phase 3). Splices that derive handlers live in "Spec.DerivedAsk".
module AskTypes where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Language.Haskell.TH    (recover)
import           MCP.Server
import           MCP.Server.Derive

data Environment = Staging | Production
  deriving (Show, Eq)

data Tag = Urgent | Routine
  deriving (Show, Eq)

newtype Ticket = Ticket Text
  deriving (Show, Eq)

data DeployTarget = DeployTarget
  { environment :: Environment
  , reason      :: Text
  , notify      :: Maybe Bool
  , replicas    :: Int
  , tags        :: [Tag]
  , ticket      :: Maybe Ticket
  } deriving (Show, Eq)

newtype Confirm = Confirm { proceed :: Bool }
  deriving (Show, Eq)

data Styled = Styled { size :: Double, label :: Text, mode :: Environment }
  deriving (Show, Eq)

data Counted = Counted { count :: Int, greeting :: Text }
  deriving (Show, Eq)

$(deriveFormWithDescription ''DeployTarget [("reason", "Why deploy now?")])
$(deriveForm ''Confirm)
$(deriveFormWithOptions ''Styled
    [ ("size", defaultFormFieldOptions { formFieldTitle = Just "Size", formFieldDefault = Just (Number 1.5) })
    , ("mode", defaultFormFieldOptions { formFieldDefault = Just "production", formFieldDescription = Just "Where" })
    ])

$(deriveFormWithOptions ''Counted
    [ ("count", defaultFormFieldOptions { formFieldDefault = Just (Number 3) })
    , ("greeting", defaultFormFieldOptions { formFieldDefault = Just "café 東京" })
    ])

-- Types a form cannot hold, for the compile-time rejection tests.
data Inner = Inner { innerName :: Text } deriving (Show, Eq)
data Nested = Nested { inner :: Inner } deriving (Show, Eq)
data TextList = TextList { names :: [Text] } deriving (Show, Eq)
data Unit = Unit deriving (Show, Eq)

-- | Which derivations were rejected at compile time.
rejectedNested, rejectedTextList, rejectedUnit, rejectedUnknownField, rejectedBadDefault, rejectedBadChoice,
  rejectedFractionalInt, rejectedOutOfRangeInt :: Bool
rejectedNested = $(recover [| True |] (deriveForm ''Nested >> [| False |]))
rejectedTextList = $(recover [| True |] (deriveForm ''TextList >> [| False |]))
rejectedUnit = $(recover [| True |] (deriveForm ''Unit >> [| False |]))
rejectedUnknownField = $(recover [| True |]
  (deriveFormWithDescription ''Confirm [("procede", "typo")] >> [| False |]))
rejectedBadDefault = $(recover [| True |]
  (deriveFormWithOptions ''Confirm [("proceed", defaultFormFieldOptions { formFieldDefault = Just "yes" })] >> [| False |]))
rejectedFractionalInt = $(recover [| True |]
  (deriveFormWithOptions ''Counted [("count", defaultFormFieldOptions { formFieldDefault = Just (Number 1.5) })] >> [| False |]))
rejectedOutOfRangeInt = $(recover [| True |]
  (deriveFormWithOptions ''Counted [("count", defaultFormFieldOptions { formFieldDefault = Just (Number 1e30) })] >> [| False |]))
rejectedBadChoice = $(recover [| True |]
  (deriveFormWithOptions ''Styled [("mode", defaultFormFieldOptions { formFieldDefault = Just "prod" })] >> [| False |]))

-- Tools: one asks, one never does.
data OpsTool
  = Deploy { service :: Text }
  | Status { service :: Text }
  deriving (Show, Eq)

handleOps :: ClientContext -> OpsTool -> Ask Content
handleOps _ (Deploy svc) = do
  target <- elicit ("Where should " <> svc <> " deploy to?")
  case target of
    Accepted t -> do
      plan <- step (pure (T.pack (show (environment t)) <> " x" <> T.pack (show (replicas t))))
      ok <- elicit ("Apply " <> plan <> "?")
      pure $ ContentText $ case ok of
        Accepted (Confirm True) -> "deployed " <> svc <> ": " <> plan
        _                       -> "cancelled"
    other -> pure (ContentText ("no target: " <> T.pack (show (() <$ other))))
handleOps _ (Status svc) = ContentText <$> liftIO (pure (svc <> " is up"))

-- An IO handler still derives unchanged.
handleOpsIO :: ClientContext -> OpsTool -> IO Content
handleOpsIO _ (Deploy svc) = pure (ContentText ("deployed " <> svc))
handleOpsIO _ (Status svc) = pure (ContentText (svc <> " is up"))

-- Structured output from an asking handler.
newtype DeployReport = DeployReport { deployed :: Bool }

handleOpsOutput :: ClientContext -> OpsTool -> Ask (ToolOutput DeployReport)
handleOpsOutput _ _ = do
  ok <- elicit "Go?"
  pure $ ToolOutput $ DeployReport $ case ok of
    Accepted (Confirm b) -> b
    _                    -> False

-- Derived here, where only 'Value' is imported from aeson: the generated
-- serializer must not rely on names from the user's imports.
$(pure [])

outputTools :: (ToolListHandler, ToolCallHandler)
outputTools = $(deriveToolHandlerWithOutput ''OpsTool 'handleOpsOutput ''DeployReport)

-- Prompts and resources can ask too.
newtype OpsPrompt = Briefing { team :: Text }

handlePrompt :: ClientContext -> OpsPrompt -> Ask Content
handlePrompt _ (Briefing t) = do
  ok <- elicit ("Include incidents for " <> t <> "?")
  pure $ ContentText $ case ok of
    Accepted (Confirm True) -> "briefing with incidents"
    _                       -> "briefing"

data OpsResource = Secrets

handleResource :: ClientContext -> URI -> OpsResource -> Ask ResourceContent
handleResource _ uri Secrets = do
  ok <- elicit "Reveal secrets?"
  pure $ ResourceText uri "text/plain" $ case ok of
    Accepted (Confirm True) -> "s3cret"
    _                       -> "hidden"
