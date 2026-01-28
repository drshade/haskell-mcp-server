{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import qualified Spec.JSONConversion
import qualified Spec.BasicDerivation
import qualified Spec.SchemaValidation
import qualified Spec.AdvancedDerivation
import qualified Spec.UnicodeHandling

main :: IO ()
main = hspec $ do
  describe "MCP Server" $ do
    Spec.JSONConversion.spec
    Spec.BasicDerivation.spec
    Spec.SchemaValidation.spec
    Spec.AdvancedDerivation.spec
    Spec.UnicodeHandling.spec
