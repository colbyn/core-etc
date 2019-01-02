module Core.Service.Aws (
    Aws
  , initAws
  , exec
) where

import Core
import NeatInterpolation (text)
import Data.Time (UTCTime)
import Control.Lens ((^.), (.~), (?~))
import Data.HashMap.Strict (HashMap)
import Numeric.Natural (Natural)
import GHC.Natural as Nat


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

-- + Processing
import qualified Control.Monad.Trans.Resource as Resource

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

-- + Type Utils
import GHC.TypeLits as TL
import GHC.OverloadedLabels (IsLabel(..))

-- + AWS
import qualified Network.AWS              as AwsSdk
import qualified Control.Monad.Trans.AWS  as AwsMonad
import qualified Network.AWS.DynamoDB     as AwsDyn

-- + Local Deps
import           Core.Record              as R
import qualified Core.Class.Renderable    as Render
import qualified Core.Codec.Json          as Json


newtype Aws = Aws AwsInternal

type AwsInternal =
  ( "env" := AwsSdk.Env
  , "region" := AwsSdk.Region
  )


initAws :: IO Aws
initAws = do
  env <- defaultEnv
  return $ Aws
    ( #env := env
    , #region := region
    )
  where
    region :: AwsSdk.Region
    region = AwsSdk.NorthVirginia
    
    defaultEnv :: IO AwsSdk.Env
    defaultEnv = AwsSdk.newEnv AwsSdk.Discover
      <&> L.set AwsSdk.envRegion region


exec :: forall a b. (AwsSdk.AWSRequest a, b ~ AwsSdk.Rs a) => Aws -> a -> IO b
exec (Aws config) x = AwsSdk.runResourceT ctx
  where
    env :: AwsSdk.Env
    env = config \- #env
    
    ctx :: Resource.ResourceT IO (AwsSdk.Rs a)
    ctx = AwsSdk.runAWS env $ AwsSdk.send x



