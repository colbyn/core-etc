{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
module Core.Codec.DynamoDb.Internal (
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
import           Core.Codec.Text.Utils    as TextCodec (camlToSnakeCase, encodeUtcTime)
import           Core.Record              as R
import           Core.Class.Newtype       as New
import qualified Core.Class.Renderable    as Render
import qualified Core.Codec.Json          as Json


{-# ANN module ("HLint: ignore" :: Pre.String) #-}


class EncodeableValue a where
  encodeValue :: a -> Dynamo.AttributeValue
  
  default encodeValue :: A.ToJSON a => a -> Dynamo.AttributeValue
  encodeValue = A.toJSON >>> \case
    A.String x -> Dynamo.attributeValue & Dynamo.avS ?~ x

class DecodeableValue a where
  decodeValue :: Dynamo.AttributeValue -> a
  
  default decodeValue :: A.FromJSON a => Dynamo.AttributeValue -> a
  decodeValue av
    | Just v <- (av ^. Dynamo.avS) = A.fromJSON (A.String v) & \case
      A.Success x -> x


class EncodeableFields a where
  encodeFields :: a -> HashMap.HashMap Text Dynamo.AttributeValue

class DecodeableFields a where
  decodeFields :: HashMap.HashMap Text Dynamo.AttributeValue -> a


-------------------------------------------------------------------------------
-- Values Encoders
-------------------------------------------------------------------------------

instance (EncodeableValue v1) => EncodeableValue (k1 := v1) where
  encodeValue f1 = Dynamo.attributeValue & Dynamo.avM .~ (encodeFields f1)

instance (EncodeableValue v1, EncodeableValue v2) => EncodeableValue (k1 := v1, k2 := v2) where
  encodeValue x = Dynamo.attributeValue & Dynamo.avM .~ (encodeFields x)
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3) => EncodeableValue (k1 := v1, k2 := v2, k3 := v3) where
  encodeValue x = Dynamo.attributeValue & Dynamo.avM .~ (encodeFields x)
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4) => EncodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4) where
  encodeValue x = Dynamo.attributeValue & Dynamo.avM .~ (encodeFields x)
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5) => EncodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5) where
  encodeValue x = Dynamo.attributeValue & Dynamo.avM .~ (encodeFields x)
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6) => EncodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6) where
  encodeValue x = Dynamo.attributeValue & Dynamo.avM .~ (encodeFields x)
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7) => EncodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7) where
  encodeValue x = Dynamo.attributeValue & Dynamo.avM .~ (encodeFields x)
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7, EncodeableValue v8) => EncodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8) where
  encodeValue x = Dynamo.attributeValue & Dynamo.avM .~ (encodeFields x)
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7, EncodeableValue v8, EncodeableValue v9) => EncodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9) where
  encodeValue x = Dynamo.attributeValue & Dynamo.avM .~ (encodeFields x)
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7, EncodeableValue v8, EncodeableValue v9, EncodeableValue v10) => EncodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10) where
  encodeValue x = Dynamo.attributeValue & Dynamo.avM .~ (encodeFields x)
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7, EncodeableValue v8, EncodeableValue v9, EncodeableValue v10, EncodeableValue v11) => EncodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11) where
  encodeValue x = Dynamo.attributeValue & Dynamo.avM .~ (encodeFields x)
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7, EncodeableValue v8, EncodeableValue v9, EncodeableValue v10, EncodeableValue v11, EncodeableValue v12) => EncodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12) where
  encodeValue x = Dynamo.attributeValue & Dynamo.avM .~ (encodeFields x)
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7, EncodeableValue v8, EncodeableValue v9, EncodeableValue v10, EncodeableValue v11, EncodeableValue v12, EncodeableValue v13) => EncodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13) where
  encodeValue x = Dynamo.attributeValue & Dynamo.avM .~ (encodeFields x)
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7, EncodeableValue v8, EncodeableValue v9, EncodeableValue v10, EncodeableValue v11, EncodeableValue v12, EncodeableValue v13, EncodeableValue v14) => EncodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14) where
  encodeValue x = Dynamo.attributeValue & Dynamo.avM .~ (encodeFields x)
instance
  (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7, EncodeableValue v8, EncodeableValue v9, EncodeableValue v10, EncodeableValue v11, EncodeableValue v12, EncodeableValue v13, EncodeableValue v14, EncodeableValue v15) => EncodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14, k15 := v15) where
  encodeValue x = Dynamo.attributeValue & Dynamo.avM .~ (encodeFields x)



-------------------------------------------------------------------------------
-- Values Decoders
-------------------------------------------------------------------------------

instance (KnownSymbol k1, DecodeableValue v1) => DecodeableValue (k1 := v1) where
  decodeValue av | not $ null (av ^. Dynamo.avM) = decodeFields (av ^. Dynamo.avM)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2) => DecodeableValue (k1 := v1, k2 := v2) where
  decodeValue av | not $ null (av ^. Dynamo.avM) = decodeFields (av ^. Dynamo.avM)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3) => DecodeableValue (k1 := v1, k2 := v2, k3 := v3) where
  decodeValue av | not $ null (av ^. Dynamo.avM) = decodeFields (av ^. Dynamo.avM)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4) => DecodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4) where
  decodeValue av | not $ null (av ^. Dynamo.avM) = decodeFields (av ^. Dynamo.avM)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5) => DecodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5) where
  decodeValue av | not $ null (av ^. Dynamo.avM) = decodeFields (av ^. Dynamo.avM)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6) => DecodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6) where
  decodeValue av | not $ null (av ^. Dynamo.avM) = decodeFields (av ^. Dynamo.avM)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7) => DecodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7) where
  decodeValue av | not $ null (av ^. Dynamo.avM) = decodeFields (av ^. Dynamo.avM)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7, KnownSymbol k8, DecodeableValue v8) => DecodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8) where
  decodeValue av | not $ null (av ^. Dynamo.avM) = decodeFields (av ^. Dynamo.avM)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7, KnownSymbol k8, DecodeableValue v8, KnownSymbol k9, DecodeableValue v9) => DecodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9) where
  decodeValue av | not $ null (av ^. Dynamo.avM) = decodeFields (av ^. Dynamo.avM)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7, KnownSymbol k8, DecodeableValue v8, KnownSymbol k9, DecodeableValue v9, KnownSymbol k10, DecodeableValue v10) => DecodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10) where
  decodeValue av | not $ null (av ^. Dynamo.avM) = decodeFields (av ^. Dynamo.avM)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7, KnownSymbol k8, DecodeableValue v8, KnownSymbol k9, DecodeableValue v9, KnownSymbol k10, DecodeableValue v10, KnownSymbol k11, DecodeableValue v11) => DecodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11) where
  decodeValue av | not $ null (av ^. Dynamo.avM) = decodeFields (av ^. Dynamo.avM)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7, KnownSymbol k8, DecodeableValue v8, KnownSymbol k9, DecodeableValue v9, KnownSymbol k10, DecodeableValue v10, KnownSymbol k11, DecodeableValue v11, KnownSymbol k12, DecodeableValue v12) => DecodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12) where
  decodeValue av | not $ null (av ^. Dynamo.avM) = decodeFields (av ^. Dynamo.avM)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7, KnownSymbol k8, DecodeableValue v8, KnownSymbol k9, DecodeableValue v9, KnownSymbol k10, DecodeableValue v10, KnownSymbol k11, DecodeableValue v11, KnownSymbol k12, DecodeableValue v12, KnownSymbol k13, DecodeableValue v13) => DecodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13) where
  decodeValue av | not $ null (av ^. Dynamo.avM) = decodeFields (av ^. Dynamo.avM)


instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7, KnownSymbol k8, DecodeableValue v8, KnownSymbol k9, DecodeableValue v9, KnownSymbol k10, DecodeableValue v10, KnownSymbol k11, DecodeableValue v11, KnownSymbol k12, DecodeableValue v12, KnownSymbol k13, DecodeableValue v13, KnownSymbol k14, DecodeableValue v14) => DecodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14) where
  decodeValue av | not $ null (av ^. Dynamo.avM) = decodeFields (av ^. Dynamo.avM)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7, KnownSymbol k8, DecodeableValue v8, KnownSymbol k9, DecodeableValue v9, KnownSymbol k10, DecodeableValue v10, KnownSymbol k11, DecodeableValue v11, KnownSymbol k12, DecodeableValue v12, KnownSymbol k13, DecodeableValue v13, KnownSymbol k14, DecodeableValue v14, KnownSymbol k15, DecodeableValue v15) => DecodeableValue (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14, k15 := v15) where
  decodeValue av | not $ null (av ^. Dynamo.avM) = decodeFields (av ^. Dynamo.avM)




-------------------------------------------------------------------------------
-- Field Encoders
-------------------------------------------------------------------------------

instance (EncodeableValue v1) => EncodeableFields (k1 := v1) where
  encodeFields = HashMap.fromList . pure . encodeRecordField

instance (EncodeableValue v1, EncodeableValue v2) => EncodeableFields (k1 := v1, k2 := v2) where
  encodeFields (f1, f2) = HashMap.fromList
    [ encodeRecordField f1
    , encodeRecordField f2
    ]
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3) => EncodeableFields (k1 := v1, k2 := v2, k3 := v3) where
  encodeFields (f1, f2, f3) = HashMap.fromList
    [ encodeRecordField f1
    , encodeRecordField f2
    , encodeRecordField f3
    ]
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4) => EncodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4) where
  encodeFields (f1, f2, f3, f4) = HashMap.fromList
    [ encodeRecordField f1
    , encodeRecordField f2
    , encodeRecordField f3
    , encodeRecordField f4
    ]
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5) => EncodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5) where
  encodeFields (f1, f2, f3, f4, f5) = HashMap.fromList
    [ encodeRecordField f1
    , encodeRecordField f2
    , encodeRecordField f3
    , encodeRecordField f4
    , encodeRecordField f5
    ]
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6) => EncodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6) where
  encodeFields (f1, f2, f3, f4, f5, f6) = HashMap.fromList
    [ encodeRecordField f1
    , encodeRecordField f2
    , encodeRecordField f3
    , encodeRecordField f4
    , encodeRecordField f5
    , encodeRecordField f6
    ]
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7) => EncodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7) where
  encodeFields (f1, f2, f3, f4, f5, f6, f7) = HashMap.fromList
    [ encodeRecordField f1
    , encodeRecordField f2
    , encodeRecordField f3
    , encodeRecordField f4
    , encodeRecordField f5
    , encodeRecordField f6
    , encodeRecordField f7
    ]
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7, EncodeableValue v8) => EncodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8) where
  encodeFields (f1, f2, f3, f4, f5, f6, f7, f8) = HashMap.fromList
    [ encodeRecordField f1
    , encodeRecordField f2
    , encodeRecordField f3
    , encodeRecordField f4
    , encodeRecordField f5
    , encodeRecordField f6
    , encodeRecordField f7
    , encodeRecordField f8
    ]
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7, EncodeableValue v8, EncodeableValue v9) => EncodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9) where
  encodeFields (f1, f2, f3, f4, f5, f6, f7, f8, f9) = HashMap.fromList
    [ encodeRecordField f1
    , encodeRecordField f2
    , encodeRecordField f3
    , encodeRecordField f4
    , encodeRecordField f5
    , encodeRecordField f6
    , encodeRecordField f7
    , encodeRecordField f8
    , encodeRecordField f9
    ]
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7, EncodeableValue v8, EncodeableValue v9, EncodeableValue v10) => EncodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10) where
  encodeFields (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10) = HashMap.fromList
    [ encodeRecordField f1
    , encodeRecordField f2
    , encodeRecordField f3
    , encodeRecordField f4
    , encodeRecordField f5
    , encodeRecordField f6
    , encodeRecordField f7
    , encodeRecordField f8
    , encodeRecordField f9
    , encodeRecordField f10
    ]
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7, EncodeableValue v8, EncodeableValue v9, EncodeableValue v10, EncodeableValue v11) => EncodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11) where
  encodeFields (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11) = HashMap.fromList
    [ encodeRecordField f1
    , encodeRecordField f2
    , encodeRecordField f3
    , encodeRecordField f4
    , encodeRecordField f5
    , encodeRecordField f6
    , encodeRecordField f7
    , encodeRecordField f8
    , encodeRecordField f9
    , encodeRecordField f10
    , encodeRecordField f11
    ]
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7, EncodeableValue v8, EncodeableValue v9, EncodeableValue v10, EncodeableValue v11, EncodeableValue v12) => EncodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12) where
  encodeFields (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12) = HashMap.fromList
    [ encodeRecordField f1
    , encodeRecordField f2
    , encodeRecordField f3
    , encodeRecordField f4
    , encodeRecordField f5
    , encodeRecordField f6
    , encodeRecordField f7
    , encodeRecordField f8
    , encodeRecordField f9
    , encodeRecordField f10
    , encodeRecordField f11
    , encodeRecordField f12
    ]
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7, EncodeableValue v8, EncodeableValue v9, EncodeableValue v10, EncodeableValue v11, EncodeableValue v12, EncodeableValue v13) => EncodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13) where
  encodeFields (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13) = HashMap.fromList
    [ encodeRecordField f1
    , encodeRecordField f2
    , encodeRecordField f3
    , encodeRecordField f4
    , encodeRecordField f5
    , encodeRecordField f6
    , encodeRecordField f7
    , encodeRecordField f8
    , encodeRecordField f9
    , encodeRecordField f10
    , encodeRecordField f11
    , encodeRecordField f12
    , encodeRecordField f13
    ]
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7, EncodeableValue v8, EncodeableValue v9, EncodeableValue v10, EncodeableValue v11, EncodeableValue v12, EncodeableValue v13, EncodeableValue v14) => EncodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14) where
  encodeFields (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14) = HashMap.fromList
    [ encodeRecordField f1
    , encodeRecordField f2
    , encodeRecordField f3
    , encodeRecordField f4
    , encodeRecordField f5
    , encodeRecordField f6
    , encodeRecordField f7
    , encodeRecordField f8
    , encodeRecordField f9
    , encodeRecordField f10
    , encodeRecordField f11
    , encodeRecordField f12
    , encodeRecordField f13
    , encodeRecordField f14
    ]
instance (EncodeableValue v1, EncodeableValue v2, EncodeableValue v3, EncodeableValue v4, EncodeableValue v5, EncodeableValue v6, EncodeableValue v7, EncodeableValue v8, EncodeableValue v9, EncodeableValue v10, EncodeableValue v11, EncodeableValue v12, EncodeableValue v13, EncodeableValue v14, EncodeableValue v15) => EncodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14, k15 := v15) where
  encodeFields (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15) = HashMap.fromList
    [ encodeRecordField f1
    , encodeRecordField f2
    , encodeRecordField f3
    , encodeRecordField f4
    , encodeRecordField f5
    , encodeRecordField f6
    , encodeRecordField f7
    , encodeRecordField f8
    , encodeRecordField f9
    , encodeRecordField f10
    , encodeRecordField f11
    , encodeRecordField f12
    , encodeRecordField f13
    , encodeRecordField f14
    , encodeRecordField f15
    ]


-------------------------------------------------------------------------------
-- Field Decoders
-------------------------------------------------------------------------------
instance (KnownSymbol k1, DecodeableValue v1) => DecodeableFields (k1 := v1) where
  decodeFields = decodeRecordField

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2) => DecodeableFields (k1 := v1, k2 := v2) where
  decodeFields x =
    (decodeRecordField x, decodeRecordField x)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3) => DecodeableFields (k1 := v1, k2 := v2, k3 := v3) where
  decodeFields x =
    (decodeRecordField x, decodeRecordField x, decodeRecordField x)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4) => DecodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4) where
  decodeFields x =
    (decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5) => DecodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5) where
  decodeFields x =
    (decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6) => DecodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6) where
  decodeFields x =
    (decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7) => DecodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7) where
  decodeFields x =
    (decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7, KnownSymbol k8, DecodeableValue v8) => DecodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8) where
  decodeFields x =
    (decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7, KnownSymbol k8, DecodeableValue v8, KnownSymbol k9, DecodeableValue v9) => DecodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9) where
  decodeFields x =
    (decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7, KnownSymbol k8, DecodeableValue v8, KnownSymbol k9, DecodeableValue v9, KnownSymbol k10, DecodeableValue v10) => DecodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10) where
  decodeFields x =
    (decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7, KnownSymbol k8, DecodeableValue v8, KnownSymbol k9, DecodeableValue v9, KnownSymbol k10, DecodeableValue v10, KnownSymbol k11, DecodeableValue v11) => DecodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11) where
  decodeFields x =
    (decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7, KnownSymbol k8, DecodeableValue v8, KnownSymbol k9, DecodeableValue v9, KnownSymbol k10, DecodeableValue v10, KnownSymbol k11, DecodeableValue v11, KnownSymbol k12, DecodeableValue v12) => DecodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12) where
  decodeFields x =
    (decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7, KnownSymbol k8, DecodeableValue v8, KnownSymbol k9, DecodeableValue v9, KnownSymbol k10, DecodeableValue v10, KnownSymbol k11, DecodeableValue v11, KnownSymbol k12, DecodeableValue v12, KnownSymbol k13, DecodeableValue v13) => DecodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13) where
  decodeFields x =
    (decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7, KnownSymbol k8, DecodeableValue v8, KnownSymbol k9, DecodeableValue v9, KnownSymbol k10, DecodeableValue v10, KnownSymbol k11, DecodeableValue v11, KnownSymbol k12, DecodeableValue v12, KnownSymbol k13, DecodeableValue v13, KnownSymbol k14, DecodeableValue v14) => DecodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14) where
  decodeFields x =
    (decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x)

instance (KnownSymbol k1, DecodeableValue v1, KnownSymbol k2, DecodeableValue v2, KnownSymbol k3, DecodeableValue v3, KnownSymbol k4, DecodeableValue v4, KnownSymbol k5, DecodeableValue v5, KnownSymbol k6, DecodeableValue v6, KnownSymbol k7, DecodeableValue v7, KnownSymbol k8, DecodeableValue v8, KnownSymbol k9, DecodeableValue v9, KnownSymbol k10, DecodeableValue v10, KnownSymbol k11, DecodeableValue v11, KnownSymbol k12, DecodeableValue v12, KnownSymbol k13, DecodeableValue v13, KnownSymbol k14, DecodeableValue v14, KnownSymbol k15, DecodeableValue v15) => DecodeableFields (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14, k15 := v15) where
  decodeFields x =
    (decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x, decodeRecordField x)



-------------------------------------------------------------------------------
-- Base
-------------------------------------------------------------------------------


encodeRecordField
  :: forall k v. (EncodeableValue v) => (k := v) -> (Text, Dynamo.AttributeValue)
encodeRecordField (_ := v) = (,) key (encodeValue v)
  where
    key :: Text
    key = camlToSnakeCase $ Text.pack $ symbolVal (Proxy :: Proxy k)

decodeRecordField
  :: forall k v res. (KnownSymbol k, DecodeableValue v) => HashMap.HashMap Text Dynamo.AttributeValue -> (k := v)
decodeRecordField o = (Proxy @k) := (decodeValue value)
  where
    value :: Dynamo.AttributeValue
    value = getNormalField label o
    
    label :: Text
    label = Text.pack $ symbolVal (Proxy @k)


getNormalField :: Text -> HashMap.HashMap Text Dynamo.AttributeValue -> Dynamo.AttributeValue
getNormalField key o = (extract . HashMap.toList . HashMap.filterWithKey (\k _ -> normalize key == normalize k)) o
  where
    extract :: [(Text, Dynamo.AttributeValue)] -> Dynamo.AttributeValue
    extract [(_, val)] = val
    -- TODO: find closest match
    -- extract xs = error $ show $ (,,) key (HashMap.keys o) xs
    
    normalize :: Text -> Text
    normalize = Text.filter (not . f) . Text.toCaseFold
      where
        f '_' = True
        f '-' = True
        f _ = False

