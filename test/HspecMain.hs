{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import qualified Spec.JSONConversion
import qualified Spec.BasicDerivation
import qualified Spec.SchemaValidation
import qualified Spec.AdvancedDerivation
import qualified Spec.UnicodeHandling
import qualified Spec.GoldenWire
import qualified Spec.ModernEra
import qualified Spec.ProtocolVersionNegotiation
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
    Spec.UnicodeHandling.spec
    Spec.GoldenWire.spec
    Spec.ModernEra.spec
    Spec.ProtocolVersionNegotiation.spec
    Spec.TemplatesCompletions.spec
    Spec.ToolCallParsing.spec
    Spec.TypedArgs.spec
