-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE Rank2Types #-}
module Core.Service.Aws.Lambda (
    InvokeInput
  , InvokeOutput
  , initInvokeInput
  , statusOk
  , looksOk
  , invoke
) where


import Core
import NeatInterpolation (text)
import Data.Time (UTCTime)
import Control.Lens ((^.), (.~), (?~))
import Data.HashMap.Strict (HashMap)
import Numeric.Natural (Natural)
import GHC.Natural as Nat
import Core.Service.Aws (Aws)
import Data.Conduit.Binary (sinkLbs)


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
import qualified Network.AWS        as AwsSdk
import qualified Network.AWS.S3     as AwsS3
import qualified Network.AWS.Lambda as AwsLam

-- + Local Deps
import           Core.Record              as R
import qualified Core.Class.Renderable    as Render
import qualified Core.Codec.Json          as Json
import qualified Core.Codec.DynamoDb      as CoreDynamo
import qualified Core.Service.Aws         as Aws

import Core.Service.Aws.Internal.Utils

-- To silence the stupid “combine with” linter warning -that only exist for readability.
-- {-# ANN module ("HLint: ignore" :: Pre.String) #-}




-------------------------------------------------------------------------------
-- Input Models
-------------------------------------------------------------------------------

type InvokeInput =
  ( "invocationType" := Maybe AwsLam.InvocationType
  , "logType"        := Maybe AwsLam.LogType
  , "qualifier"      := Maybe Text
  , "clientContext"  := Maybe Text
  , "functionName"   := Text
  , "payload"        := ByteString
  )

-------------------------------------------------------------------------------
-- Output Models
-------------------------------------------------------------------------------

type InvokeOutput =
  ( "functionError"   := Maybe Text
  , "logResult"       := Maybe Text
  , "payload"         := Maybe ByteString
  , "executedVersion" := Maybe Text
  , "status"          := Int
  )




-------------------------------------------------------------------------------
-- Misc. Utils
-------------------------------------------------------------------------------

initInvokeInput :: Text -> ByteString -> InvokeInput
initInvokeInput name payload =
  ( #invocationType := Nothing
  , #logType        := Nothing
  , #qualifier      := Nothing
  , #clientContext  := Nothing
  , #functionName   := name
  , #payload        := payload
  )


statusOk :: R.Has "status" Int rec => rec -> Bool
statusOk = R.get #status >>> \case
  200 -> True
  204 -> True
  _ -> False


looksOk :: (R.Has "status" Int rec, R.Has "functionError" e rec, e ~ Maybe Text) => rec -> Bool
looksOk inp = case (R.get #status inp, R.get #functionError inp) of
  (200, Nothing) -> True
  (_, Just "Unhandled") -> False


-------------------------------------------------------------------------------
-- Senders
-------------------------------------------------------------------------------

invoke :: Aws -> InvokeInput -> IO InvokeOutput
invoke aws inp = convertFromInvokeResponce <$> Aws.exec aws (convertToInvoke inp)



-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------


-- *
-- * Input Conversions
-- *

convertToInvoke :: InvokeInput -> AwsLam.Invoke
convertToInvoke x = con
  & convertIfJust #invocationType x AwsLam.iInvocationType
  & convertIfJust #logType x AwsLam.iLogType
  & convertIfJust #qualifier x AwsLam.iQualifier
  & convertIfJust #clientContext x AwsLam.iClientContext
  & convert #functionName x AwsLam.iFunctionName
  & convert #payload x AwsLam.iPayload
  where
    con = AwsLam.invoke (x \- #functionName) (x \- #payload)



-- *
-- * Output Conversions
-- *


convertFromInvokeResponce :: AwsLam.InvokeResponse -> InvokeOutput
convertFromInvokeResponce x =
  ( #functionError := functionError
  , #logResult := logResult
  , #payload := payload
  , #executedVersion := executedVersion
  , #status := statusCode
  )
  where
    functionError = x ^. AwsLam.irsFunctionError
    logResult = x ^. AwsLam.irsLogResult
    payload = x ^. AwsLam.irsPayload
    executedVersion = x ^. AwsLam.irsExecutedVersion
    statusCode = x ^. AwsLam.irsStatusCode




-- *
-- * Helpers
-- *

-------------------------------------------------------------------------------
-- Internal - JSON Encoding/Decoding
-------------------------------------------------------------------------------

instance Render.Renderable AwsLam.InvocationType where
  manifest = Render.manifest . Text.pack . show
instance Render.Renderable AwsLam.LogType where
  manifest = Render.manifest . Text.pack . show




