{-# LANGUAGE OverloadedStrings #-}

-- | Deterministic JSON encoding, for JSON that ends up inside a string or a
-- digest, where structural comparison cannot see through the bytes.
module MCP.Server.Internal.Json
  ( canonicalJson
  , canonicalJsonText
  ) where

import           Data.Aeson           (ToJSON (..), Value (..), encode)
import qualified Data.Aeson.Key       as Key
import qualified Data.Aeson.KeyMap    as KM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map             as Map
import           Data.Text            (Text)
import qualified Data.Text.Encoding   as TE

-- | Compact JSON with objects in sorted-key order, independent of the
-- aeson\/hashable pair in the build plan.
canonicalJson :: Value -> BSL.ByteString
canonicalJson = encode . Canonical

-- | 'canonicalJson' as text. Used wherever JSON ends up /inside a string/
-- (the structured-output text block): structural comparison cannot see
-- through a string, so those bytes must be deterministic — which also
-- keeps repeated tool results stable for client prompt caching.
canonicalJsonText :: Value -> Text
canonicalJsonText = TE.decodeUtf8 . BSL.toStrict . canonicalJson

-- Encoding-only wrapper: objects are emitted via a sorted 'Map', arrays
-- and atoms recurse. 'encode' uses 'toEncoding', so no KeyMap is rebuilt.
newtype Canonical = Canonical Value

instance ToJSON Canonical where
  toJSON (Canonical v) = v
  toEncoding (Canonical (Object o)) =
    toEncoding (Map.fromList [(Key.toText k, Canonical v) | (k, v) <- KM.toList o])
  toEncoding (Canonical (Array xs)) = toEncoding (fmap Canonical xs)
  toEncoding (Canonical v) = toEncoding v
