module Core.Codec.DynamoDb (
    EncodeableValue(..)
  , DecodeableValue(..)
  , EncodeableFields(..)
  , DecodeableFields(..)
) where


import Core
import Data.Time (UTCTime)
import Control.Lens ((^.), (.~), (?~))

import qualified Prelude                      as Pre
import qualified Control.Lens                 as L
import qualified Control.Exception.Lens       as EL
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
import qualified Data.Time                    as Time

-- + OS Apis
import qualified System.IO      as Sys
import qualified System.Exit    as Sys
import qualified System.Process as SP

-- + Debugging
import qualified Text.Show.Prettyprint as PP

-- + Serialization
import Data.UUID.Types (UUID)
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Types           as A
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID
import qualified Crypto.KDF.BCrypt          as BSC

-- + Type Utils
import GHC.TypeLits as TL
import GHC.OverloadedLabels (IsLabel(..))

-- + AWS
import qualified Network.AWS              as Aws
import qualified Network.AWS.S3           as S3
import qualified Network.AWS.DynamoDB     as Dynamo

-- + Local Deps
import           Core.Codec.Text.Utils    as TextCodec (camlToSnakeCase, encodeUtcTime, decodeUtcTime)
import           Core.Record              as R
import           Core.Class.Newtype       as New
import qualified Core.Class.Renderable    as Render
import qualified Core.Codec.Json          as Json

-- + Local
import Core.Codec.DynamoDb.Internal
  ( EncodeableValue(..)
  , DecodeableValue(..)
  , EncodeableFields(..)
  , DecodeableFields(..)
  )


-------------------------------------------------------------------------------
-- Basic - Coders
-------------------------------------------------------------------------------

instance EncodeableValue Dynamo.AttributeValue where
  encodeValue = id
instance DecodeableValue Dynamo.AttributeValue where
  decodeValue = id


-------------------------------------------------------------------------------
-- Value Encoders
-------------------------------------------------------------------------------
instance EncodeableValue Bool where
  encodeValue x = Dynamo.attributeValue & Dynamo.avBOOL ?~ x

instance EncodeableValue Int where
  encodeValue x = Dynamo.attributeValue & Dynamo.avN ?~ Text.pack (show x)

instance EncodeableValue BS.ByteString where
  encodeValue x = Dynamo.attributeValue & Dynamo.avB ?~ x

instance EncodeableValue Text where
  encodeValue x
    | Text.null x = Dynamo.attributeValue & Dynamo.avNULL ?~ True
    | otherwise   = Dynamo.attributeValue & Dynamo.avS ?~ x

instance EncodeableValue UTCTime where
  encodeValue x = Dynamo.attributeValue & Dynamo.avS ?~ encodeUtcTime x

instance EncodeableValue UUID where
  encodeValue x = Dynamo.attributeValue & Dynamo.avS ?~ UUID.toText x

instance EncodeableValue (Maybe UUID) where
  encodeValue Nothing  = Dynamo.attributeValue & Dynamo.avNULL ?~ True
  encodeValue (Just x) = Dynamo.attributeValue & Dynamo.avS ?~ UUID.toText x

instance EncodeableValue a => EncodeableValue [a] where
  encodeValue x = Dynamo.attributeValue & Dynamo.avL .~ map encodeValue x

instance EncodeableValue a => EncodeableValue (HashMap.HashMap Text a) where
  encodeValue x = Dynamo.attributeValue & Dynamo.avM .~ HashMap.map encodeValue x


-------------------------------------------------------------------------------
-- Value Decoders
-------------------------------------------------------------------------------
instance DecodeableValue Bool where
  decodeValue av | Just v <- av ^. Dynamo.avBOOL = v

instance DecodeableValue Int where
  decodeValue av | Just v <- av ^. Dynamo.avN = read (Text.unpack v)

instance DecodeableValue BS.ByteString where
  decodeValue av | Just v <- av ^. Dynamo.avB = v

instance DecodeableValue Text where
  decodeValue av
    | Just v <- av ^. Dynamo.avS = v
    | Just True <- av ^. Dynamo.avNULL = Text.empty


instance DecodeableValue UTCTime where
  decodeValue av | Just v <- av ^. Dynamo.avS = decodeUtcTime v

instance DecodeableValue UUID where
  decodeValue av | Just v <- av ^. Dynamo.avS = UUID.fromText v & \case
    Just x -> x

instance DecodeableValue (Maybe UUID) where
  decodeValue av
    | Just v <- av ^. Dynamo.avS = UUID.fromText v
    | Just True <- av ^. Dynamo.avNULL = Nothing

instance DecodeableValue a => DecodeableValue [a] where
  decodeValue av = map decodeValue (av ^. Dynamo.avL)

instance DecodeableValue a => DecodeableValue (HashMap.HashMap Text a) where
  decodeValue av = HashMap.map decodeValue (av ^. Dynamo.avM)


