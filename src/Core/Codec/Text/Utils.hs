module Core.Codec.Text.Utils (
    camlToSnakeCase
  , encodeUtcTime
  , decodeUtcTime
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

-- + Type Utils
import GHC.TypeLits as TL
import GHC.OverloadedLabels (IsLabel(..))
import Data.Type.Bool as Bool


-- + Local Deps
-- import Core.Record as R
-- import Core.Class.Newtype as New


camlToSnakeCase :: Text -> Text
camlToSnakeCase = Text.concatMap f
  where
    f x
      | Char.isUpper x = "_" <> Text.singleton (Char.toLower x)
      | otherwise      = Text.singleton x


encodeUtcTime :: UTCTime -> Text
encodeUtcTime x = case A.toJSON x of
  A.String x -> x

decodeUtcTime :: Text -> UTCTime
decodeUtcTime = A.String >>> A.fromJSON >>> \case
  A.Success x -> x


