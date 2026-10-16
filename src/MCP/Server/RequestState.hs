{-# LANGUAGE OverloadedStrings #-}

-- | Protection for the @requestState@ of multi round-trip requests.
--
-- When a handler answers with 'MCP.Server.Types.InputRequired', the
-- client must echo the request state back on its retry. The specification
-- treats that state as attacker-controlled, so the library never hands it
-- to the client in the clear: it binds the state to the caller, the
-- original request and an expiry, then seals it with a t'StateCodec'.
--
-- The default codec, 'ephemeralStateCodec', encrypts with a random key
-- created when the transport starts. That is right for stdio and for a
-- single HTTP instance. Deployments with several instances behind a load
-- balancer must share a key, with 'aeadStateCodec':
--
-- > Just key = stateKey "2026-10" keyBytes   -- 32 secret bytes
-- > config = defaultHttpConfig { httpStateCodec = Just (aeadStateCodec key []) }
--
-- To rotate keys, seal with the new key and keep accepting the old one
-- until its states have expired: @aeadStateCodec newKey [oldKey]@.
module MCP.Server.RequestState
  ( -- * Codecs
    StateCodec(..)
  , StateError(..)
  , defaultStateLifetime
  , aeadStateCodec
  , ephemeralStateCodec
    -- * Keys
  , StateKey
  , stateKey
  , stateKeyId
  ) where

import           Crypto.Cipher.ChaChaPoly1305 (aeadChacha20poly1305Init)
import           Crypto.Cipher.Types          (AuthTag (..), aeadSimpleDecrypt,
                                               aeadSimpleEncrypt)
import           Crypto.Error                 (maybeCryptoError)
import           Crypto.Random                (getRandomBytes)
import qualified Data.ByteArray               as BA
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Base64.URL   as B64
import           Data.Char                    (isAsciiLower, isAsciiUpper,
                                               isDigit)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Time.Clock              (NominalDiffTime)
import           Numeric                      (showHex)

-- | How sealed request state is produced and checked. The library builds
-- the plaintext (the caller, the request, the expiry and the handler's
-- state) and verifies it after 'openState'; a codec only has to keep it
-- confidential and tamper-evident.
--
-- Replace the codec to manage keys elsewhere, or to keep states on the
-- server and hand the client only a reference.
data StateCodec = StateCodec
  { sealState :: ByteString -> IO Text
    -- ^ Seal plaintext into the opaque string sent to the client.
  , openState :: Text -> IO (Either StateError ByteString)
    -- ^ Recover the plaintext from a string the client sent back.
  , stateLifetime :: NominalDiffTime
    -- ^ How long a state stays valid after it is issued.
  }

-- | Why a state could not be opened.
data StateError
  = StateUnknownKey
    -- ^ Sealed under a key this codec does not have: issued by another
    --   server instance, or under a key that has since been retired.
  | StateInvalid
    -- ^ Malformed, or altered after it was sealed.
  deriving (Show, Eq)

-- | 60 minutes: long enough for a user to fill in a form.
defaultStateLifetime :: NominalDiffTime
defaultStateLifetime = 3600

-- | A 256-bit key for 'aeadStateCodec', with an id that is stored next to
-- each sealed state so the right key is used to open it.
data StateKey = StateKey Text ByteString

-- | A key from its id and 32 secret bytes. The id is 1–32 characters of
-- ASCII letters, digits, @-@ and @_@; it is visible to clients, so it
-- should not reveal anything. 'Nothing' if either is invalid.
stateKey :: Text -> ByteString -> Maybe StateKey
stateKey kid secret
  | validId && BS.length secret == 32 = Just (StateKey kid secret)
  | otherwise = Nothing
  where
    validId = not (T.null kid) && T.length kid <= 32 && T.all idChar kid
    idChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '-' || c == '_'

-- | The id of a key.
stateKeyId :: StateKey -> Text
stateKeyId (StateKey kid _) = kid

-- | Seal with ChaCha20-Poly1305 under the first key, with a random nonce
-- for every state; open with that key or any of the others. States last
-- 'defaultStateLifetime'; set 'stateLifetime' to change that.
aeadStateCodec :: StateKey -> [StateKey] -> StateCodec
aeadStateCodec current older = StateCodec
  { sealState = sealWith current
  , openState = pure . openWith (current : older)
  , stateLifetime = defaultStateLifetime
  }

-- | 'aeadStateCodec' with a random key, created afresh by each call. States
-- cannot be opened by another process, or after a restart.
ephemeralStateCodec :: IO StateCodec
ephemeralStateCodec = do
  secret <- getRandomBytes 32
  idBytes <- getRandomBytes 6 :: IO ByteString
  let kid = "ephemeral-" <> T.pack (concatMap hex (BS.unpack idBytes))
      hex b = let s = showHex b "" in if length s < 2 then '0' : s else s
  case stateKey kid secret of
    Just key -> pure (aeadStateCodec key [])
    Nothing  -> error "ephemeralStateCodec: generated an invalid key"

-- Wire format: @<key id>.<base64url(nonce || ciphertext || tag)>@. The key
-- id and a format version are authenticated as associated data.

nonceSize, tagSize :: Int
nonceSize = 12
tagSize = 16

associatedData :: Text -> ByteString
associatedData kid = "mcp-server/requestState/v1:" <> TE.encodeUtf8 kid

sealWith :: StateKey -> ByteString -> IO Text
sealWith (StateKey kid secret) plaintext = do
  nonce <- getRandomBytes nonceSize :: IO ByteString
  case maybeCryptoError (aeadChacha20poly1305Init secret nonce) of
    Nothing   -> error "aeadStateCodec: invalid key or nonce"
    Just aead -> do
      let (AuthTag tag, ciphertext) =
            aeadSimpleEncrypt aead (associatedData kid) plaintext tagSize
          blob = nonce <> ciphertext <> BA.convert tag
      pure (kid <> "." <> TE.decodeUtf8 (B64.encode blob))

openWith :: [StateKey] -> Text -> Either StateError ByteString
openWith keys sealed = do
  let (kid, rest) = T.breakOn "." sealed
  StateKey _ secret <- maybe (Left StateUnknownKey) Right $
    lookup kid [(k, key) | key@(StateKey k _) <- keys]
  blob <- either (const (Left StateInvalid)) Right $
    B64.decode (TE.encodeUtf8 (T.drop 1 rest))
  if BS.length blob < nonceSize + tagSize
    then Left StateInvalid
    else do
      let (nonce, body) = BS.splitAt nonceSize blob
          (ciphertext, tag) = BS.splitAt (BS.length body - tagSize) body
      aead <- maybe (Left StateInvalid) Right $
        maybeCryptoError (aeadChacha20poly1305Init secret nonce)
      maybe (Left StateInvalid) Right $
        aeadSimpleDecrypt aead (associatedData kid) ciphertext (AuthTag (BA.convert tag))
