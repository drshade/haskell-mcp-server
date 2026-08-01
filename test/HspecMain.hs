{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import qualified Spec.JSONConversion
import qualified Spec.BasicDerivation
import qualified Spec.SchemaValidation
import qualified Spec.AdvancedDerivation
import qualified Spec.Cancellation
import qualified Spec.UnicodeHandling
import qualified Spec.DefinitionMetadata
import qualified Spec.DerivedOutput
import qualified Spec.GoldenWire
import qualified Spec.ModernEra
import qualified Spec.Progress
import qualified Spec.ProtocolVersionNegotiation
import qualified Spec.Subscriptions
import qualified Spec.TemplatesCompletions
import qualified Spec.ToolCallParsing
import qualified Spec.TypedArgs

main :: IO ()
main = hspec $ do
  describe "MCP Server" $ do
    Spec.JSONConversion.spec
    Spec.BasicDerivation.spec
    Spec.SchemaValidation.spec
    Spec.AdvancedDerivation.spec
    Spec.Cancellation.spec
    Spec.UnicodeHandling.spec
    Spec.DefinitionMetadata.spec
    Spec.DerivedOutput.spec
    Spec.GoldenWire.spec
    Spec.ModernEra.spec
    Spec.Progress.spec
    Spec.ProtocolVersionNegotiation.spec
    Spec.Subscriptions.spec
    Spec.TemplatesCompletions.spec
    Spec.ToolCallParsing.spec
    Spec.TypedArgs.spec
