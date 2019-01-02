-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE StandaloneDeriving #-}
module Core.Service.Aws.DynamoDB (
  -- Input Models
    GetItemInput
  , DeleteItemInput
  , NewItemInput
  , QueryInput
  , UpdateItemInput
  -- Output Models
  , GetItemOutput
  , UpdateItemOutput
  , NewItemOutput
  , DeleteItemOutput
  -- Utils
  , QueryOutput
  , TableName
  , Key
  , Item
  , KeyNames
  , Attributes
  , Expr
  , initGetItemInput
  , initDeleteItemInput
  , initUpdateItemInput
  , initNewItemInput
  , initQueryInput
  -- Senders
  , getItem
  , deleteItem
  , newItem
  , query
  , updateItem
  , statusOk
  -- Re-Exports
  , AwsDyn.AttributeValue
  , AwsDyn.ReturnConsumedCapacity(..)
  , AwsDyn.ReturnItemCollectionMetrics(..)
  , AwsDyn.ConsumedCapacity
  , AwsDyn.ReturnValue(..)
  , AwsDyn.Select(..)
) where


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
import qualified Data.Ord                     as Ord
import qualified Data.Set                     as Set

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

import Core.Service.Aws.Internal.Utils

-- {-# ANN module ("HLint: ignore" :: Pre.String) #-}

-- deriving instance Ord AwsDyn.AttributeValue
-- deriving instance Ord AwsDyn.ConsumedCapacity

instance Ord AwsDyn.AttributeValue where
  compare x y = compare (toValue x) (toValue y)


toValue :: AwsDyn.AttributeValue -> Maybe Value
toValue x
  | not $ null (x ^. AwsDyn.avM) = Just $ Map $ HashMap.mapMaybe toValue (x ^. AwsDyn.avM)
  | not $ null (x ^. AwsDyn.avNS) = Just $ NumberSet (x ^. AwsDyn.avNS)
  | not $ null (x ^. AwsDyn.avBS) = Just $ BinarySet (x ^. AwsDyn.avBS)
  | not $ null (x ^. AwsDyn.avSS) = Just $ StringSet (x ^. AwsDyn.avSS)
  | Maybe.isJust (x ^. AwsDyn.avN) = Just $ Number $ Maybe.fromJust (x ^. AwsDyn.avN)
  | Maybe.isJust (x ^. AwsDyn.avB) = Just $ Binary $ Maybe.fromJust (x ^. AwsDyn.avB)
  | Maybe.isJust (x ^. AwsDyn.avS) = Just $ String $ Maybe.fromJust (x ^. AwsDyn.avS)
  | Maybe.isJust (x ^. AwsDyn.avBOOL) = Just $ Bool $ Maybe.fromJust (x ^. AwsDyn.avBOOL)
  | (x ^. AwsDyn.avNULL) == Just True = Just Null

data Value
  = List [Value]
  | Map (HashMap Text Value)
  | Null
  | Number Text
  | Binary ByteString
  | String Text
  | Bool Bool
  | NumberSet [Text]
  | BinarySet [ByteString]
  | StringSet [Text]
  deriving (Eq, Ord, Show)


-------------------------------------------------------------------------------
-- Input Models
-------------------------------------------------------------------------------

type GetItemInput =
  ( "projectionExpression"     := Maybe Text
  , "expressionAttributeNames" := Maybe (HashMap Text Text)
  , "consistentRead"           := Maybe Bool
  , "returnConsumedCapacity"   := Maybe AwsDyn.ReturnConsumedCapacity
  , "tableName"                := Text
  , "key"                      := HashMap Text AwsDyn.AttributeValue
  )

type DeleteItemInput =
  ( "expressionAttributeNames"    := Maybe (HashMap Text Text)
  , "returnValues"                := Maybe AwsDyn.ReturnValue
  , "expressionAttributeValues"   := Maybe (HashMap Text AwsDyn.AttributeValue)
  , "returnConsumedCapacity"      := Maybe AwsDyn.ReturnConsumedCapacity
  , "returnItemCollectionMetrics" := Maybe AwsDyn.ReturnItemCollectionMetrics
  , "conditionExpression"         := Maybe Text
  , "tableName"                   := Text
  , "key"                         := HashMap Text AwsDyn.AttributeValue
  )

-- | Note that the API endpoint is called 'PutItem'.
type NewItemInput =
  ( "expressionAttributeNames"    := Maybe (HashMap Text Text)
  , "returnValues"                := Maybe AwsDyn.ReturnValue
  , "expressionAttributeValues"   := Maybe (HashMap Text AwsDyn.AttributeValue)
  , "returnConsumedCapacity"      := Maybe AwsDyn.ReturnConsumedCapacity
  , "returnItemCollectionMetrics" := Maybe AwsDyn.ReturnItemCollectionMetrics
  , "conditionExpression"         := Maybe Text
  , "tableName"                   := Text
  , "item"                        := HashMap Text AwsDyn.AttributeValue
  )

type QueryInput =
  ( "projectionExpression"      := Maybe Text
  , "expressionAttributeNames"  := Maybe (HashMap Text Text)
  , "filterExpression"          := Maybe Text
  , "consistentRead"            := Maybe Bool
  , "expressionAttributeValues" := Maybe (HashMap Text AwsDyn.AttributeValue)
  , "returnConsumedCapacity"    := Maybe AwsDyn.ReturnConsumedCapacity
  , "scanIndexForward"          := Maybe Bool
  , "limit"                     := Maybe Int
  , "select"                    := Maybe AwsDyn.Select
  , "keyConditionExpression"    := Maybe Text
  , "exclusiveStartKey"         := Maybe (HashMap Text AwsDyn.AttributeValue)
  , "indexName"                 := Maybe Text
  , "tableName"                 := Text
  )

type UpdateItemInput =
  ( "expressionAttributeNames"    := Maybe (HashMap Text Text)
  , "returnValues"                := Maybe AwsDyn.ReturnValue
  , "updateExpression"            := Maybe Text
  , "expressionAttributeValues"   := Maybe (HashMap Text AwsDyn.AttributeValue)
  , "returnConsumedCapacity"      := Maybe AwsDyn.ReturnConsumedCapacity
  , "returnItemCollectionMetrics" := Maybe AwsDyn.ReturnItemCollectionMetrics
  , "conditionExpression"         := Maybe Text
  , "tableName"                   := Text
  , "key"                         := HashMap Text AwsDyn.AttributeValue
  )


-------------------------------------------------------------------------------
-- Output Models
-------------------------------------------------------------------------------

type GetItemOutput =
  ( "consumedCapacity" := Maybe AwsDyn.ConsumedCapacity
  , "item"             := HashMap Text AwsDyn.AttributeValue
  , "status"           := Int
  )


type UpdateItemOutput =
  ( "itemCollectionMetrics" := Maybe AwsDyn.ItemCollectionMetrics
  , "consumedCapacity"      := Maybe AwsDyn.ConsumedCapacity
  , "attributes"            := HashMap Text AwsDyn.AttributeValue
  , "status"                := Int
  )

-- | Note that the API endpoint is called 'PutItem'.
type NewItemOutput =
  ( "itemCollectionMetrics" := Maybe AwsDyn.ItemCollectionMetrics
  , "consumedCapacity"      := Maybe AwsDyn.ConsumedCapacity
  , "attributes"            := HashMap Text AwsDyn.AttributeValue
  , "status"                := Int
  )

type DeleteItemOutput =
  ( "itemCollectionMetrics" := Maybe AwsDyn.ItemCollectionMetrics
  , "consumedCapacity"      := Maybe AwsDyn.ConsumedCapacity
  , "attributes"            := HashMap Text AwsDyn.AttributeValue
  , "status"                := Int
  )

type QueryOutput =
  ( "lastEvaluatedKey" := HashMap Text AwsDyn.AttributeValue
  , "count"            := Maybe Int
  , "scannedCount"     := Maybe Int
  , "items"            := [HashMap Text AwsDyn.AttributeValue]
  , "consumedCapacity" := Maybe AwsDyn.ConsumedCapacity
  , "status"           := Int
  )

-------------------------------------------------------------------------------
-- Misc. Utils
-------------------------------------------------------------------------------

type TableName = Text
type Key = HashMap Text AwsDyn.AttributeValue
type Item = HashMap Text AwsDyn.AttributeValue
type KeyNames = HashMap Text Text
type Attributes = HashMap Text AwsDyn.AttributeValue
type Expr = Text

initGetItemInput :: TableName -> Key -> GetItemInput
initGetItemInput tableName key =
  ( #projectionExpression     := Nothing
  , #expressionAttributeNames := Nothing
  , #consistentRead           := Nothing
  , #returnConsumedCapacity   := Nothing
  , #tableName                := tableName
  , #key                      := key
  )

initDeleteItemInput :: TableName -> Key -> DeleteItemInput
initDeleteItemInput tableName key =
  ( #expressionAttributeNames    := Nothing
  , #returnValues                := Nothing
  , #expressionAttributeValues   := Nothing
  , #returnConsumedCapacity      := Nothing
  , #returnItemCollectionMetrics := Nothing
  , #conditionExpression         := Nothing
  , #tableName                   := tableName
  , #key                         := key
  )

initUpdateItemInput :: TableName -> Key -> UpdateItemInput
initUpdateItemInput tableName key =
  ( #expressionAttributeNames    := Nothing
  , #returnValues                := Nothing
  , #updateExpression            := Nothing
  , #expressionAttributeValues   := Nothing
  , #returnConsumedCapacity      := Nothing
  , #returnItemCollectionMetrics := Nothing
  , #conditionExpression         := Nothing
  , #tableName                   := tableName
  , #key                         := key
  )

initNewItemInput :: TableName -> Item -> NewItemInput
initNewItemInput tableName item =
  ( #expressionAttributeNames    := Nothing
  , #returnValues                := Nothing
  , #expressionAttributeValues   := Nothing
  , #returnConsumedCapacity      := Nothing
  , #returnItemCollectionMetrics := Nothing
  , #conditionExpression         := Nothing
  , #tableName                   := tableName
  , #item                        := item
  )

initQueryInput :: TableName -> QueryInput
initQueryInput tableName =
  ( #projectionExpression      := Nothing
  , #expressionAttributeNames  := Nothing
  , #filterExpression          := Nothing
  , #consistentRead            := Nothing
  , #expressionAttributeValues := Nothing
  , #returnConsumedCapacity    := Nothing
  , #scanIndexForward          := Nothing
  , #limit                     := Nothing
  , #select                    := Nothing
  , #keyConditionExpression    := Nothing
  , #exclusiveStartKey         := Nothing
  , #indexName                 := Nothing
  , #tableName                 := tableName
  )


statusOk :: R.Has "status" Int rec => rec -> Bool
statusOk = R.get #status >>> \case
  200 -> True
  204 -> True
  _ -> False

-------------------------------------------------------------------------------
-- Senders
-------------------------------------------------------------------------------

getItem :: Aws -> GetItemInput -> IO GetItemOutput
getItem aws inp = convertToGetItemOutput <$> Aws.exec aws (convertFromGetItemInput inp)

deleteItem :: Aws -> DeleteItemInput -> IO DeleteItemOutput
deleteItem aws inp = convertToDeleteItemOutput <$> Aws.exec aws (convertFromDeleteItemInput inp)

newItem :: Aws -> NewItemInput -> IO NewItemOutput
newItem aws inp = convertToNewItemOutput <$> Aws.exec aws (convertFromNewItemInput inp)

query :: Aws -> QueryInput -> IO QueryOutput
query aws inp = convertToQueryOutput <$> Aws.exec aws (convertFromQueryInput inp)

updateItem :: Aws -> UpdateItemInput -> IO UpdateItemOutput
updateItem aws inp = convertToUpdateItemOutput <$> Aws.exec aws (convertFromUpdateItemInput inp)



-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

convertFromGetItemInput :: GetItemInput -> AwsDyn.GetItem
convertFromGetItemInput x = AwsDyn.getItem (x \- #tableName)
  & convertIfJust #projectionExpression x AwsDyn.giProjectionExpression
  & convertIfJust' #expressionAttributeNames x AwsDyn.giExpressionAttributeNames
  & convertIfJust #consistentRead x AwsDyn.giConsistentRead
  & convert #tableName x AwsDyn.giTableName
  & convert #key x AwsDyn.giKey


convertFromDeleteItemInput :: DeleteItemInput -> AwsDyn.DeleteItem
convertFromDeleteItemInput x = AwsDyn.deleteItem (x \- #tableName)
  & convertIfJust' #expressionAttributeNames x AwsDyn.diExpressionAttributeNames
  & convertIfJust #returnValues x AwsDyn.diReturnValues
  & convertIfJust' #expressionAttributeValues x AwsDyn.diExpressionAttributeValues
  & convertIfJust #returnConsumedCapacity x AwsDyn.diReturnConsumedCapacity
  & convertIfJust #returnItemCollectionMetrics x AwsDyn.diReturnItemCollectionMetrics
  & convertIfJust #conditionExpression x AwsDyn.diConditionExpression
  & convert #tableName x AwsDyn.diTableName
  & convert #key x AwsDyn.diKey

convertFromUpdateItemInput :: UpdateItemInput -> AwsDyn.UpdateItem
convertFromUpdateItemInput x = AwsDyn.updateItem (x \- #tableName)
  & convertIfJust' #expressionAttributeNames x AwsDyn.uiExpressionAttributeNames
  & convertIfJust #returnValues x AwsDyn.uiReturnValues
  & convertIfJust #updateExpression x AwsDyn.uiUpdateExpression
  & convertIfJust' #expressionAttributeValues x AwsDyn.uiExpressionAttributeValues
  & convertIfJust #returnConsumedCapacity x AwsDyn.uiReturnConsumedCapacity
  & convertIfJust #returnItemCollectionMetrics x AwsDyn.uiReturnItemCollectionMetrics
  & convertIfJust #conditionExpression x AwsDyn.uiConditionExpression
  & convert #tableName x AwsDyn.uiTableName
  & convert #key x AwsDyn.uiKey

convertFromNewItemInput :: NewItemInput -> AwsDyn.PutItem
convertFromNewItemInput x = AwsDyn.putItem (x \- #tableName)
  & convertIfJust' #expressionAttributeNames x AwsDyn.piExpressionAttributeNames
  & convertIfJust #returnValues x AwsDyn.piReturnValues
  & convertIfJust' #expressionAttributeValues x AwsDyn.piExpressionAttributeValues
  & convertIfJust #returnConsumedCapacity x AwsDyn.piReturnConsumedCapacity
  & convertIfJust #returnItemCollectionMetrics x AwsDyn.piReturnItemCollectionMetrics
  & convertIfJust #conditionExpression x AwsDyn.piConditionExpression
  & convert #tableName x AwsDyn.piTableName
  & convert #item x AwsDyn.piItem

convertFromQueryInput :: QueryInput -> AwsDyn.Query
convertFromQueryInput x = AwsDyn.query (x \- #tableName)
  & convertIfJust #projectionExpression x AwsDyn.qProjectionExpression
  & convertIfJust' #expressionAttributeNames x AwsDyn.qExpressionAttributeNames
  & convertIfJust #filterExpression x AwsDyn.qFilterExpression
  & convertIfJust #consistentRead x AwsDyn.qConsistentRead
  & convertIfJust' #expressionAttributeValues x AwsDyn.qExpressionAttributeValues
  & convertIfJust #returnConsumedCapacity x AwsDyn.qReturnConsumedCapacity
  & convertIfJust #scanIndexForward x AwsDyn.qScanIndexForward
  & convertIfJust #limit (mapRec #limit (fmap intToNatural) x) AwsDyn.qLimit
  & convertIfJust #select x AwsDyn.qSelect
  & convertIfJust #keyConditionExpression x AwsDyn.qKeyConditionExpression
  & convertIfJust' #exclusiveStartKey x AwsDyn.qExclusiveStartKey
  & convertIfJust #indexName x AwsDyn.qIndexName
  & convert #tableName x AwsDyn.qTableName




convertToGetItemOutput :: AwsDyn.GetItemResponse -> GetItemOutput
convertToGetItemOutput x =
    ( #consumedCapacity := consumedCapacity
    , #item := item
    , #status := status
    )
  where
    consumedCapacity :: Maybe AwsDyn.ConsumedCapacity
    consumedCapacity = x ^. AwsDyn.girsConsumedCapacity
    
    item :: HashMap Text AwsDyn.AttributeValue
    item = x ^. AwsDyn.girsItem
    
    status :: Int
    status = x ^. AwsDyn.girsResponseStatus


convertToUpdateItemOutput :: AwsDyn.UpdateItemResponse -> UpdateItemOutput
convertToUpdateItemOutput x =
    ( #itemCollectionMetrics := itemCollectionMetrics
    , #consumedCapacity := consumedCapacity
    , #attributes := attributes
    , #status := status
    )
  where
    itemCollectionMetrics :: Maybe AwsDyn.ItemCollectionMetrics
    itemCollectionMetrics = x ^. AwsDyn.uirsItemCollectionMetrics
  
    consumedCapacity :: Maybe AwsDyn.ConsumedCapacity
    consumedCapacity = x ^. AwsDyn.uirsConsumedCapacity
  
    attributes :: HashMap Text AwsDyn.AttributeValue
    attributes = x ^. AwsDyn.uirsAttributes
  
    status :: Int
    status = x ^. AwsDyn.uirsResponseStatus


convertToNewItemOutput :: AwsDyn.PutItemResponse -> NewItemOutput
convertToNewItemOutput x =
    ( #itemCollectionMetrics := itemCollectionMetrics
    , #consumedCapacity := consumedCapacity
    , #attributes := attributes
    , #status := status
    )
  where
    itemCollectionMetrics :: Maybe AwsDyn.ItemCollectionMetrics
    itemCollectionMetrics = x ^. AwsDyn.pirsItemCollectionMetrics
    
    consumedCapacity :: Maybe AwsDyn.ConsumedCapacity
    consumedCapacity = x ^. AwsDyn.pirsConsumedCapacity
    
    attributes :: HashMap Text AwsDyn.AttributeValue
    attributes = x ^. AwsDyn.pirsAttributes
    
    status :: Int
    status = x ^. AwsDyn.pirsResponseStatus

convertToDeleteItemOutput :: AwsDyn.DeleteItemResponse -> DeleteItemOutput
convertToDeleteItemOutput x =
    ( #itemCollectionMetrics := itemCollectionMetrics
    , #consumedCapacity := consumedCapacity
    , #attributes := attributes
    , #status := status
    )
  where
    itemCollectionMetrics :: Maybe AwsDyn.ItemCollectionMetrics
    itemCollectionMetrics = x ^. AwsDyn.dirsItemCollectionMetrics
    
    consumedCapacity :: Maybe AwsDyn.ConsumedCapacity
    consumedCapacity = x ^. AwsDyn.dirsConsumedCapacity
    
    attributes :: HashMap Text AwsDyn.AttributeValue
    attributes = x ^. AwsDyn.dirsAttributes
    
    status :: Int
    status = x ^. AwsDyn.dirsResponseStatus


convertToQueryOutput :: AwsDyn.QueryResponse -> QueryOutput
convertToQueryOutput x =
    ( #lastEvaluatedKey := lastEvaluatedKey
    , #count := count
    , #scannedCount := scannedCount
    , #items := items
    , #consumedCapacity := consumedCapacity
    , #status := status
    )
  where
    lastEvaluatedKey :: (HashMap Text AwsDyn.AttributeValue)
    lastEvaluatedKey = x ^. AwsDyn.qrsLastEvaluatedKey
    
    count :: Maybe Int
    count = x ^. AwsDyn.qrsCount
    
    scannedCount :: Maybe Int
    scannedCount = x ^. AwsDyn.qrsScannedCount
    
    items :: [HashMap Text AwsDyn.AttributeValue]
    items = x ^. AwsDyn.qrsItems
    
    consumedCapacity :: Maybe AwsDyn.ConsumedCapacity
    consumedCapacity = x ^. AwsDyn.qrsConsumedCapacity
    
    status :: Int
    status = x ^. AwsDyn.qrsResponseStatus

