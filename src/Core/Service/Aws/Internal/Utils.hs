-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE Rank2Types #-}
module Core.Service.Aws.Internal.Utils where

import Core
import NeatInterpolation (text)
import Data.Time (UTCTime)
import Control.Lens ((^.), (.~), (?~))
import Data.HashMap.Strict (HashMap)
import Numeric.Natural (Natural)
import GHC.Natural as Nat
import Core.Service.Aws (Aws)


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
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteArray.Encoding    as ByteArray


-- + Type Utils
import GHC.TypeLits as TL
import GHC.OverloadedLabels (IsLabel(..))

-- + AWS
import qualified Network.AWS              as AwsSdk
import qualified Network.AWS.DynamoDB     as AwsDyn

-- + Local Deps
import           Core.Record              as R
import qualified Core.Class.Renderable    as Render
import qualified Core.Codec.Json          as Json
import qualified Core.Codec.DynamoDb      as CoreDynamo
import qualified Core.Service.Aws         as Aws

-- {-# ANN module ("HLint: ignore" :: Pre.String) #-}





intToNatural :: Int -> Natural
intToNatural = Pre.fromInteger . Pre.fromIntegral


mapRec
  :: forall l v1 v2 r1 r2. (KnownSymbol l, Has l v1 r1, Has l v2 r2, r2 ~ (l := v2))
  => Proxy l -> (v1 -> v2) -> r1 -> r2
mapRec l f rec = l := v2
  where
    v2 :: v2
    v2 = f v1
    
    v1 :: v1
    v1 = R.get l rec


convert
  :: forall label value record endpoint.
    (R.Has label value record)
  => Proxy label
  -> record
  -> L.Lens' endpoint value
  -> endpoint
  -> endpoint
convert label rec l payload = payload & (l .~ v)
  where
    v :: value
    v = R.get label rec


convertIfJust
  :: forall label value value' record endpoint.
    (R.Has label value record, value ~ (Maybe value'))
  => Proxy label
  -> record
  -> L.Lens' endpoint value
  -> endpoint
  -> endpoint
convertIfJust label rec l payload = case value of
  Nothing -> payload
  Just v -> payload & (l ?~ v)
  where
    value :: Maybe value'
    value = R.get label rec


convertIfJust'
  :: forall label value value' record endpoint.
    (R.Has label value record, value ~ (Maybe value'))
  => Proxy label
  -> record
  -> L.Lens' endpoint value'
  -> endpoint
  -> endpoint
convertIfJust' label rec l payload = case value of
  Nothing -> payload
  Just v -> payload & (l .~ v)
  where
    value :: Maybe value'
    value = R.get label rec





