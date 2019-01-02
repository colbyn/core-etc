module Core.Network.Http.Client (
    Method(..)
  , ContentType(..)
  , Request
  , Responce
  , send
) where


import Core
import Data.Time (UTCTime)
import Control.Lens ((^.), (.~), (?~))

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

-- + OS Apis
import qualified System.IO      as Sys
import qualified System.Exit    as Sys
import qualified System.Process as SP

-- + Serialization
import Data.UUID.Types (UUID)
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Types           as A
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID
import qualified Network.HTTP.Types.Header  as HttpHeader
import qualified Network.HTTP.Types.Status  as HttpStatus
import qualified Data.CaseInsensitive       as CaseInsensitive

-- + Http Client
import qualified Network.HTTP.Client as C

-- + Local Deps
import Core.Record as R




-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data Method
  = Get
  | Head
  | Post
  | Put
  | Delete
  | Patch
  deriving (Show, Eq)

data ContentType
  = Json
  | Jpeg
  | Png
  deriving (Show, Eq)

type Request =
  ( "url" := Text
  , "method" := Either Text Method
  , "contentType" := Maybe ContentType
  , "headers" := [(Text, Text)]
  , "body" := Maybe ByteString
  )

-- TODO: rename to 'Response'.
type Responce =
  ( "status" := Int
  , "headers" := [(Text, Text)]
  , "body" := ByteString
  )



-------------------------------------------------------------------------------
-- Base
-------------------------------------------------------------------------------

send :: Request -> IO Responce
send req = do
  manager <- C.newManager C.defaultManagerSettings
  requestData <- provisionRequest req <$> C.parseUrlThrow (Text.unpack $ req \- #url)
  responce <- C.httpLbs requestData manager
  return $ reformatResponce responce


-- |
-- Helper for 'send'
reformatResponce :: C.Response LBS.ByteString -> Responce
reformatResponce res =
  ( #status := status
  , #headers := headers
  , #body := LBS.toStrict body
  )
  where
    status = C.responseStatus res
      & HttpStatus.statusCode
    headers = C.responseHeaders res
      & map unpackHeader
    body = C.responseBody res
    
    unpackHeader :: HttpHeader.Header -> (Text, Text)
    unpackHeader (name, value) =
      (,)
        (decodeUtf8 $ CaseInsensitive.original name)
        (decodeUtf8 value)



-- |
-- Helper for 'send'
provisionRequest :: Request -> C.Request -> C.Request
provisionRequest req base = base
  & addMethod
  & addContentType
  & addHeaders
  & addPayload
  & addTimeout
  where
    addPayload :: C.Request -> C.Request
    addPayload r = case req \- #body of
      Nothing -> r
      Just x  -> r {C.requestBody = C.RequestBodyLBS $ LBS.fromStrict x}
    
    addContentType :: C.Request -> C.Request
    addContentType r = case req \- #contentType of
      Nothing   -> r
      Just Jpeg -> r
        {C.requestHeaders = (HttpHeader.hContentType, "image/jpeg") : C.requestHeaders r}
      Just Png  -> r
        {C.requestHeaders = (HttpHeader.hContentType, "image/png") : C.requestHeaders r}
      Just Json -> r
        {C.requestHeaders = (HttpHeader.hContentType, "application/json") : C.requestHeaders r}
    
    addHeaders :: C.Request -> C.Request
    addHeaders r =
      r {C.requestHeaders = map packHeader (req \- #headers) <> C.requestHeaders r}
    
    addMethod :: C.Request -> C.Request
    addMethod r = r {C.method = method}
    
    addTimeout :: C.Request -> C.Request
    addTimeout r = r {C.responseTimeout = C.responseTimeoutMicro ((1000000 * 60) * 10)}
    
    packHeader :: (Text, Text) -> HttpHeader.Header
    packHeader (name, value) =
      (,) (CaseInsensitive.mk $ encodeUtf8 name) (encodeUtf8 value)
    
    method :: ByteString
    method = case req \- #method of
      Left raw -> encodeUtf8 raw
      Right x -> case x of
        Get -> "GET"
        Head -> "HEAD"
        Post -> "POST"
        Put -> "PUT"
        Delete -> "DELETE"
        Patch -> "PATCH"


