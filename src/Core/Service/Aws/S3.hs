-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE Rank2Types #-}
module Core.Service.Aws.S3 (
  -- Input Models
    GetObjectInput
  , NewObjectInput
  , DeleteObjectInput
  
  -- Output Models
  , GetObjectOutput
  , NewObjectOutput
  , DeleteObjectOutput
  
  -- Utils
  , S3Path
  , initGetObjectInput
  , initNewObjectInput
  , initDeleteObjectInput
  , statusOk
  
  -- Senders
  , getObject
  , newObject
  , deleteObject
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
import qualified Network.AWS              as AwsSdk
import qualified Network.AWS.S3           as AwsS3

-- + Local Deps
import           Core.Record              as R
import qualified Core.Class.Renderable    as Render
import qualified Core.Codec.Json          as Json
import qualified Core.Codec.DynamoDb      as CoreDynamo
import qualified Core.Service.Aws         as Aws

import Core.Service.Aws.Internal.Utils

-- To silence the stupid “combine with” linter warning -that only exist for readability.
{-# ANN module ("HLint: ignore" :: Pre.String) #-}



-------------------------------------------------------------------------------
-- Input Models
-------------------------------------------------------------------------------

type GetObjectInput =
  ( "ifMatch"                    := Maybe Text
  , "versionId"                  := Maybe Text -- (Maybe ObjectVersionId)
  , "responseContentType"        := Maybe Text
  , "responseContentDisposition" := Maybe Text
  , "responseContentLanguage"    := Maybe Text
  , "sseCustomerAlgorithm"       := Maybe Text
  , "sseCustomerKey"             := Maybe Text
  , "requestPayer"               := Maybe AwsS3.RequestPayer
  , "responseContentEncoding"    := Maybe Text
  , "ifModifiedSince"            := Maybe UTCTime
  , "partNumber"                 := Maybe Int
  , "range"                      := Maybe Text
  , "ifUnmodifiedSince"          := Maybe UTCTime
  , "sseCustomerKeyMd5"          := Maybe Text
  , "responseCacheControl"       := Maybe Text
  , "responseExpires"            := Maybe UTCTime
  , "ifNoneMatch"                := Maybe Text
  , "bucket"                     := Text
  , "key"                        := Text
  )

-- | Note that the API endpoint is called 'PutObject'.
type NewObjectInput =
  ( "contentLength"           := Maybe Int
  , "expires"                 := Maybe UTCTime
  , "grantReadAcp"            := Maybe Text
  , "sseCustomerAlgorithm"    := Maybe Text
  , "sseCustomerKey"          := Maybe Text
  , "requestPayer"            := Maybe AwsS3.RequestPayer
  , "grantWriteAcp"           := Maybe Text
  , "websiteRedirectLocation" := Maybe Text
  , "grantRead"               := Maybe Text
  , "storageClass"            := Maybe AwsS3.StorageClass
  , "sseCustomerKeyMd5"       := Maybe Text
  , "sseKmsKeyId"             := Maybe Text
  , "grantFullControl"        := Maybe Text
  , "contentEncoding"         := Maybe Text
  , "tagging"                 := Maybe Text
  , "contentMd5"              := Maybe Text
  , "metadata"                := HashMap Text Text
  , "cacheControl"            := Maybe Text
  , "contentLanguage"         := Maybe Text
  , "acl"                     := Maybe AwsS3.ObjectCannedACL
  , "contentDisposition"      := Maybe Text
  , "serverSideEncryption"    := Maybe AwsS3.ServerSideEncryption
  , "contentType"             := Maybe Text
  , "bucket"                  := Text
  , "key"                     := Text
  , "body"                    := ByteString
  )

type DeleteObjectInput =
  ( "versionId"    := Maybe Text -- (Maybe ObjectVersionId)
  , "mfa"          := Maybe Text
  , "requestPayer" := Maybe AwsS3.RequestPayer
  , "bucket"       := Text
  , "key"          := Text
  )


-------------------------------------------------------------------------------
-- Output Models
-------------------------------------------------------------------------------

type GetObjectOutput =
  ( "requestCharged"          := Maybe AwsS3.RequestCharged
  , "partsCount"              := Maybe Int
  , "eTag"                    := Maybe AwsS3.ETag
  , "versionId"               := Maybe Text -- (Maybe ObjectVersionId)
  , "contentLength"           := Maybe Int
  , "expires"                 := Maybe UTCTime
  , "restore"                 := Maybe Text
  , "expiration"              := Maybe Text
  , "deleteMarker"            := Maybe Bool
  , "sseCustomerAlgorithm"    := Maybe Text
  , "tagCount"                := Maybe Int
  , "missingMeta"             := Maybe Int
  , "websiteRedirectLocation" := Maybe Text
  , "acceptRanges"            := Maybe Text
  , "storageClass"            := Maybe AwsS3.StorageClass
  , "sseCustomerKeyMd5"       := Maybe Text
  , "sseKmsKeyId"             := Maybe Text
  , "contentEncoding"         := Maybe Text
  , "metadata"                := HashMap Text Text
  , "replicationStatus"       := Maybe AwsS3.ReplicationStatus
  , "cacheControl"            := Maybe Text
  , "contentLanguage"         := Maybe Text
  , "lastModified"            := Maybe UTCTime
  , "contentDisposition"      := Maybe Text
  , "contentRange"            := Maybe Text
  , "serverSideEncryption"    := Maybe AwsS3.ServerSideEncryption
  , "contentType"             := Maybe Text
  , "status"                  := Int
  , "body"                    := ByteString
  )

type NewObjectOutput = 
  ( "requestCharged"       := Maybe AwsS3.RequestCharged
  , "eTag"                 := Maybe AwsS3.ETag
  , "versionId"            := Maybe Text -- (Maybe ObjectVersionId)
  , "expiration"           := Maybe Text
  , "sseCustomerAlgorithm" := Maybe Text
  , "sseCustomerKeyMd5"    := Maybe Text
  , "sseKmsKeyId"          := Maybe Text
  , "serverSideEncryption" := Maybe AwsS3.ServerSideEncryption
  , "status"               := Int
  )

type DeleteObjectOutput =
  ( "requestCharged" := Maybe AwsS3.RequestCharged
  , "versionId"      := Maybe Text -- (Maybe ObjectVersionId)
  , "deleteMarker"   := Maybe Bool
  , "status"         := Int
  )



-------------------------------------------------------------------------------
-- Misc. Utils
-------------------------------------------------------------------------------

type S3Path =
  ( "bucket" := Text
  , "key" := Text
  )

initGetObjectInput :: S3Path -> GetObjectInput
initGetObjectInput path =
  ( #ifMatch                    := Nothing
  , #versionId                  := Nothing
  , #responseContentType        := Nothing
  , #responseContentDisposition := Nothing
  , #responseContentLanguage    := Nothing
  , #sseCustomerAlgorithm       := Nothing
  , #sseCustomerKey             := Nothing
  , #requestPayer               := Nothing
  , #responseContentEncoding    := Nothing
  , #ifModifiedSince            := Nothing
  , #partNumber                 := Nothing
  , #range                      := Nothing
  , #ifUnmodifiedSince          := Nothing
  , #sseCustomerKeyMd5          := Nothing
  , #responseCacheControl       := Nothing
  , #responseExpires            := Nothing
  , #ifNoneMatch                := Nothing
  , #bucket                     := (path \- #bucket)
  , #key                        := (path \- #key)
  )

initNewObjectInput :: S3Path -> ByteString -> NewObjectInput
initNewObjectInput path body =
  ( #contentLength           := Nothing
  , #expires                 := Nothing
  , #grantReadAcp            := Nothing
  , #sseCustomerAlgorithm    := Nothing
  , #sseCustomerKey          := Nothing
  , #requestPayer            := Nothing
  , #grantWriteAcp           := Nothing
  , #websiteRedirectLocation := Nothing
  , #grantRead               := Nothing
  , #storageClass            := Nothing
  , #sseCustomerKeyMd5       := Nothing
  , #sseKmsKeyId             := Nothing
  , #grantFullControl        := Nothing
  , #contentEncoding         := Nothing
  , #tagging                 := Nothing
  , #contentMd5              := Nothing
  , #metadata                := HashMap.empty
  , #cacheControl            := Nothing
  , #contentLanguage         := Nothing
  , #acl                     := Nothing
  , #contentDisposition      := Nothing
  , #serverSideEncryption    := Nothing
  , #contentType             := Nothing
  , #bucket                  := (path \- #bucket)
  , #key                     := (path \- #key)
  , #body                    := body
  )

initDeleteObjectInput :: S3Path -> DeleteObjectInput
initDeleteObjectInput path =
  ( #versionId    := Nothing
  , #mfa          := Nothing
  , #requestPayer := Nothing
  , #bucket       := (path \- #bucket)
  , #key          := (path \- #key)
  )


statusOk :: R.Has "status" Int rec => rec -> Bool
statusOk = R.get #status >>> \case
  200 -> True
  204 -> True
  _ -> False



-------------------------------------------------------------------------------
-- Senders
-------------------------------------------------------------------------------

getObject :: Aws -> GetObjectInput -> IO GetObjectOutput
getObject aws inp = convertFromGetObjectResponse =<< Aws.exec aws (convertToGetObject inp)

newObject :: Aws -> NewObjectInput -> IO NewObjectOutput
newObject aws inp = convertFromPutObjectResponse <$> Aws.exec aws (convertToPutObject inp)

deleteObject :: Aws -> DeleteObjectInput -> IO DeleteObjectOutput
deleteObject aws inp = convertFromDeleteObjectResponse <$> Aws.exec aws (convertToDeleteObject inp)


-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------


-- *
-- * Input Conversions
-- *

convertToGetObject :: GetObjectInput -> AwsS3.GetObject
convertToGetObject x = con
  & convertIfJust #ifMatch x AwsS3.goIfMatch
  & convertIfJust #versionId (mapRec #versionId (fmap AwsS3.ObjectVersionId) x) AwsS3.goVersionId
  & convertIfJust #responseContentType x AwsS3.goResponseContentType
  & convertIfJust #responseContentDisposition x AwsS3.goResponseContentDisposition
  & convertIfJust #responseContentLanguage x AwsS3.goResponseContentLanguage
  & convertIfJust #sseCustomerAlgorithm x AwsS3.goSSECustomerAlgorithm
  & convertIfJust #sseCustomerKey x AwsS3.goSSECustomerKey
  & convertIfJust #requestPayer x AwsS3.goRequestPayer
  & convertIfJust #responseContentEncoding x AwsS3.goResponseContentEncoding
  & convertIfJust #ifModifiedSince x AwsS3.goIfModifiedSince
  & convertIfJust #partNumber x AwsS3.goPartNumber
  & convertIfJust #range x AwsS3.goRange
  & convertIfJust #ifUnmodifiedSince x AwsS3.goIfUnmodifiedSince
  & convertIfJust #sseCustomerKeyMd5 x AwsS3.goSSECustomerKeyMD5
  & convertIfJust #responseCacheControl x AwsS3.goResponseCacheControl
  & convertIfJust #responseExpires x AwsS3.goResponseExpires
  & convertIfJust #ifNoneMatch x AwsS3.goIfNoneMatch
  & AwsS3.goBucket .~ bucket
  & AwsS3.goKey .~ key
  where
    bucket :: AwsS3.BucketName
    bucket = AwsS3.BucketName (x \- #bucket)
    
    key :: AwsS3.ObjectKey
    key = AwsS3.ObjectKey (x \- #key)
    
    con = AwsS3.getObject bucket key


convertToPutObject :: NewObjectInput -> AwsS3.PutObject
convertToPutObject x = con
  & convertIfJust #contentLength (mapRec #contentLength (fmap Pre.fromIntegral) x) AwsS3.poContentLength
  & convertIfJust #expires x AwsS3.poExpires
  & convertIfJust #grantReadAcp x AwsS3.poGrantReadACP
  & convertIfJust #sseCustomerAlgorithm x AwsS3.poSSECustomerAlgorithm
  & convertIfJust #sseCustomerKey x AwsS3.poSSECustomerKey
  & convertIfJust #requestPayer x AwsS3.poRequestPayer
  & convertIfJust #grantWriteAcp x AwsS3.poGrantWriteACP
  & convertIfJust #websiteRedirectLocation x AwsS3.poWebsiteRedirectLocation
  & convertIfJust #grantRead x AwsS3.poGrantRead
  & convertIfJust #storageClass x AwsS3.poStorageClass
  & convertIfJust #sseCustomerKeyMd5 x AwsS3.poSSECustomerKeyMD5
  & convertIfJust #sseKmsKeyId x AwsS3.poSSEKMSKeyId
  & convertIfJust #grantFullControl x AwsS3.poGrantFullControl
  & convertIfJust #contentEncoding x AwsS3.poContentEncoding
  & convertIfJust #tagging x AwsS3.poTagging
  & convertIfJust #contentMd5 x AwsS3.poContentMD5
  & convert #metadata x AwsS3.poMetadata
  & convertIfJust #cacheControl x AwsS3.poCacheControl
  & convertIfJust #contentLanguage x AwsS3.poContentLanguage
  & convertIfJust #acl x AwsS3.poACL
  & convertIfJust #contentDisposition x AwsS3.poContentDisposition
  & convertIfJust #serverSideEncryption x AwsS3.poServerSideEncryption
  & convertIfJust #contentType x AwsS3.poContentType
  & AwsS3.poBucket .~ bucket
  & AwsS3.poKey .~ key
  & AwsS3.poBody .~ body
  where
    body :: AwsSdk.RqBody
    body = AwsSdk.toBody (x \- #body)
    
    bucket :: AwsS3.BucketName
    bucket = AwsS3.BucketName (x \- #bucket)
    
    key :: AwsS3.ObjectKey
    key = AwsS3.ObjectKey (x \- #key)
    
    con = AwsS3.putObject bucket key body


convertToDeleteObject :: DeleteObjectInput -> AwsS3.DeleteObject
convertToDeleteObject x = con
  & convertIfJust #versionId (mapRec #versionId (fmap AwsS3.ObjectVersionId) x) AwsS3.doVersionId
  & convertIfJust #mfa x AwsS3.doMFA
  & convertIfJust #requestPayer x AwsS3.doRequestPayer
  & AwsS3.doKey .~ key
  & AwsS3.doBucket .~ bucket
  where
    bucket :: AwsS3.BucketName
    bucket = AwsS3.BucketName (x \- #bucket)

    key :: AwsS3.ObjectKey
    key = AwsS3.ObjectKey (x \- #key)
    
    con = AwsS3.deleteObject bucket key





-- *
-- * Output Conversions
-- *

convertFromGetObjectResponse :: AwsS3.GetObjectResponse -> IO GetObjectOutput
convertFromGetObjectResponse x = do
  body <- getBody
  return
    ( #requestCharged := requestCharged
    , #partsCount := partsCount
    , #eTag := eTag
    , #versionId := fmap fromVersionId versionId
    , #contentLength := fmap Pre.fromIntegral contentLength
    , #expires := expires
    , #restore := restore
    , #expiration := expiration
    , #deleteMarker := deleteMarker
    , #sseCustomerAlgorithm := sseCustomerAlgorithm
    , #tagCount := tagCount
    , #missingMeta := missingMeta
    , #websiteRedirectLocation := websiteRedirectLocation
    , #acceptRanges := acceptRanges
    , #storageClass := storageClass
    , #sseCustomerKeyMd5 := sseCustomerKeyMd5
    , #sseKmsKeyId := sseKmsKeyId
    , #contentEncoding := contentEncoding
    , #metadata := metadata
    , #replicationStatus := replicationStatus
    , #cacheControl := cacheControl
    , #contentLanguage := contentLanguage
    , #lastModified := lastModified
    , #contentDisposition := contentDisposition
    , #contentRange := contentRange
    , #serverSideEncryption := serverSideEncryption
    , #contentType := contentType
    , #status := responseStatus
    , #body := body
    )
  where
    requestCharged = x ^. AwsS3.gorsRequestCharged
    partsCount = x ^. AwsS3.gorsPartsCount
    eTag = x ^. AwsS3.gorsETag
    versionId = x ^. AwsS3.gorsVersionId
    contentLength = x ^. AwsS3.gorsContentLength
    expires = x ^. AwsS3.gorsExpires
    restore = x ^. AwsS3.gorsRestore
    expiration = x ^. AwsS3.gorsExpiration
    deleteMarker = x ^. AwsS3.gorsDeleteMarker
    sseCustomerAlgorithm = x ^. AwsS3.gorsSSECustomerAlgorithm
    tagCount = x ^. AwsS3.gorsTagCount
    missingMeta = x ^. AwsS3.gorsMissingMeta
    websiteRedirectLocation = x ^. AwsS3.gorsWebsiteRedirectLocation
    acceptRanges = x ^. AwsS3.gorsAcceptRanges
    storageClass = x ^. AwsS3.gorsStorageClass
    sseCustomerKeyMd5 = x ^. AwsS3.gorsSSECustomerKeyMD5
    sseKmsKeyId = x ^. AwsS3.gorsSSEKMSKeyId
    contentEncoding = x ^. AwsS3.gorsContentEncoding
    metadata = x ^. AwsS3.gorsMetadata
    replicationStatus = x ^. AwsS3.gorsReplicationStatus
    cacheControl = x ^. AwsS3.gorsCacheControl
    contentLanguage = x ^. AwsS3.gorsContentLanguage
    lastModified = x ^. AwsS3.gorsLastModified
    contentDisposition = x ^. AwsS3.gorsContentDisposition
    contentRange = x ^. AwsS3.gorsContentRange
    serverSideEncryption = x ^. AwsS3.gorsServerSideEncryption
    contentType = x ^. AwsS3.gorsContentType
    responseStatus = x ^. AwsS3.gorsResponseStatus
    
    getBody = (x ^. AwsS3.gorsBody) `AwsSdk.sinkBody` sinkLbs <&> LBS.toStrict

convertFromPutObjectResponse :: AwsS3.PutObjectResponse -> NewObjectOutput
convertFromPutObjectResponse x =
  ( #requestCharged := requestCharged
  , #eTag := eTag
  , #versionId := fmap fromVersionId versionId
  , #expiration := expiration
  , #sseCustomerAlgorithm := sseCustomerAlgorithm
  , #sseCustomerKeyMd5 := sseCustomerKeyMd5
  , #sseKmsKeyId := sseKmsKeyId
  , #serverSideEncryption := serverSideEncryption
  , #status := responseStatus
  )
  where
    requestCharged = x ^. AwsS3.porsRequestCharged
    eTag = x ^. AwsS3.porsETag
    versionId = x ^. AwsS3.porsVersionId
    expiration = x ^. AwsS3.porsExpiration
    sseCustomerAlgorithm = x ^. AwsS3.porsSSECustomerAlgorithm
    sseCustomerKeyMd5 = x ^. AwsS3.porsSSECustomerKeyMD5
    sseKmsKeyId = x ^. AwsS3.porsSSEKMSKeyId
    serverSideEncryption = x ^. AwsS3.porsServerSideEncryption
    responseStatus = x ^. AwsS3.porsResponseStatus


convertFromDeleteObjectResponse :: AwsS3.DeleteObjectResponse -> DeleteObjectOutput
convertFromDeleteObjectResponse x =
  ( #requestCharged := requestCharged
  , #versionId := fmap fromVersionId versionId
  , #deleteMarker := deleteMarker
  , #status := responseStatus
  )
  where
    requestCharged = x ^. AwsS3.dorsRequestCharged
    versionId = x ^. AwsS3.dorsVersionId
    deleteMarker = x ^. AwsS3.dorsDeleteMarker
    responseStatus = x ^. AwsS3.dorsResponseStatus


-- *
-- * Helpers
-- *

fromVersionId :: AwsS3.ObjectVersionId -> Text
fromVersionId (AwsS3.ObjectVersionId x) = x



