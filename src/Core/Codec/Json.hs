-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Core.Codec.Json (
    encodeField
  , decodeField
  , Decodable(..)
  , Encodeable(..)
  , Null(..)
) where


import Core

import qualified Prelude                      as Pre
import qualified Control.Lens                 as L
import qualified Control.Monad                as M
import qualified Control.Monad.IO.Class       as M
import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import qualified Data.Maybe                   as Maybe
import qualified Data.Either                  as Either
import qualified Data.Char                    as Char
import qualified Data.String                  as String
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Char8        as Char8
import qualified Data.HashMap.Strict          as HashMap

-- + Serialization
import Data.UUID.Types (UUID)
import Data.Aeson
  ( ToJSON(..)
  , FromJSON(..)
  , (.:)
  , (.:?)
  , (.=)
  )
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Types           as A
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID
import qualified Data.ByteString.Base64     as Base64

-- + Type Utils
import GHC.TypeLits as TL
import GHC.OverloadedLabels (IsLabel(..))
import Data.Type.Bool as Bool


-- + Local Deps
import Core.Record as R
import Core.Class.Newtype as New

-- + Local
import Core.Codec.Json.Record (encodeField, decodeField)


class Decodable a where
  decode :: A.FromJSON b => a -> Maybe b

class Encodeable o where
  encode :: A.ToJSON i => i -> o

instance Decodable BS.ByteString where
  decode = A.decode' . LBS.fromStrict

instance Decodable Text where
  decode = A.decode' . LBS.fromStrict . encodeUtf8


instance Encodeable Text where
  encode x = A.encode x
    & LBS.toStrict
    & decodeUtf8

instance Encodeable ByteString where
  encode x = A.encode x
    & LBS.toStrict

instance Encodeable LBS.ByteString where
  encode = A.encode

instance A.ToJSON BS.ByteString where
  toJSON = A.String . decodeUtf8 . Base64.encode

instance A.FromJSON BS.ByteString where
  parseJSON = A.withText "Base64 Encoded ByteString"
    (return . Base64.decodeLenient . encodeUtf8)



data Null = Null
  deriving (Eq, Ord, Show)

instance A.ToJSON Null where
  toJSON _ = A.Null

instance A.FromJSON Null where
  parseJSON _ = return Null



