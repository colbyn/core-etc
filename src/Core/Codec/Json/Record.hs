{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DefaultSignatures #-}
module Core.Codec.Json.Record (
    encodeField
  , decodeField
) where


import Core
import Data.Kind

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
import Core.Record as R
import Core.Class.Newtype as New
import Core.Codec.Text.Utils (camlToSnakeCase)



{-# ANN module ("HLint: ignore" :: Pre.String) #-}


instance (A.ToJSON v1) => A.ToJSON (k1 := v1) where
  toJSON x = A.object
    [ encodeField x
    ]
instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2) => A.ToJSON (k1 := v1, k2 := v2) where
  toJSON (f1, f2) = A.object
    [ encodeField f1
    , encodeField f2
    ]
instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3) where
  toJSON (f1, f2, f3) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    ]
instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3, A.ToJSON v4) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4) where
  toJSON (f1, f2, f3, f4) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    , encodeField f4
    ]
instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3, A.ToJSON v4, A.ToJSON v5) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5) where
  toJSON (f1, f2, f3, f4, f5) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    , encodeField f4
    , encodeField f5
    ]
instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3, A.ToJSON v4, A.ToJSON v5, A.ToJSON v6) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6) where
  toJSON (f1, f2, f3, f4, f5, f6) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    , encodeField f4
    , encodeField f5
    , encodeField f6
    ]
instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3, A.ToJSON v4, A.ToJSON v5, A.ToJSON v6, A.ToJSON v7) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7) where
  toJSON (f1, f2, f3, f4, f5, f6, f7) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    , encodeField f4
    , encodeField f5
    , encodeField f6
    , encodeField f7
    ]
instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3, A.ToJSON v4, A.ToJSON v5, A.ToJSON v6, A.ToJSON v7, A.ToJSON v8) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8) where
  toJSON (f1, f2, f3, f4, f5, f6, f7, f8) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    , encodeField f4
    , encodeField f5
    , encodeField f6
    , encodeField f7
    , encodeField f8
    ]
instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3, A.ToJSON v4, A.ToJSON v5, A.ToJSON v6, A.ToJSON v7, A.ToJSON v8, A.ToJSON v9) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9) where
  toJSON (f1, f2, f3, f4, f5, f6, f7, f8, f9) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    , encodeField f4
    , encodeField f5
    , encodeField f6
    , encodeField f7
    , encodeField f8
    , encodeField f9
    ]
instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3, A.ToJSON v4, A.ToJSON v5, A.ToJSON v6, A.ToJSON v7, A.ToJSON v8, A.ToJSON v9, A.ToJSON v10) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10) where
  toJSON (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    , encodeField f4
    , encodeField f5
    , encodeField f6
    , encodeField f7
    , encodeField f8
    , encodeField f9
    , encodeField f10
    ]
instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3, A.ToJSON v4, A.ToJSON v5, A.ToJSON v6, A.ToJSON v7, A.ToJSON v8, A.ToJSON v9, A.ToJSON v10, A.ToJSON v11) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11) where
  toJSON (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    , encodeField f4
    , encodeField f5
    , encodeField f6
    , encodeField f7
    , encodeField f8
    , encodeField f9
    , encodeField f10
    , encodeField f11
    ]
instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3, A.ToJSON v4, A.ToJSON v5, A.ToJSON v6, A.ToJSON v7, A.ToJSON v8, A.ToJSON v9, A.ToJSON v10, A.ToJSON v11, A.ToJSON v12) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12) where
  toJSON (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    , encodeField f4
    , encodeField f5
    , encodeField f6
    , encodeField f7
    , encodeField f8
    , encodeField f9
    , encodeField f10
    , encodeField f11
    , encodeField f12
    ]
instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3, A.ToJSON v4, A.ToJSON v5, A.ToJSON v6, A.ToJSON v7, A.ToJSON v8, A.ToJSON v9, A.ToJSON v10, A.ToJSON v11, A.ToJSON v12, A.ToJSON v13) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13) where
  toJSON (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    , encodeField f4
    , encodeField f5
    , encodeField f6
    , encodeField f7
    , encodeField f8
    , encodeField f9
    , encodeField f10
    , encodeField f11
    , encodeField f12
    , encodeField f13
    ]
instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3, A.ToJSON v4, A.ToJSON v5, A.ToJSON v6, A.ToJSON v7, A.ToJSON v8, A.ToJSON v9, A.ToJSON v10, A.ToJSON v11, A.ToJSON v12, A.ToJSON v13, A.ToJSON v14) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14) where
  toJSON (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    , encodeField f4
    , encodeField f5
    , encodeField f6
    , encodeField f7
    , encodeField f8
    , encodeField f9
    , encodeField f10
    , encodeField f11
    , encodeField f12
    , encodeField f13
    , encodeField f14
    ]
instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3, A.ToJSON v4, A.ToJSON v5, A.ToJSON v6, A.ToJSON v7, A.ToJSON v8, A.ToJSON v9, A.ToJSON v10, A.ToJSON v11, A.ToJSON v12, A.ToJSON v13, A.ToJSON v14, A.ToJSON v15) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14, k15 := v15) where
  toJSON (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    , encodeField f4
    , encodeField f5
    , encodeField f6
    , encodeField f7
    , encodeField f8
    , encodeField f9
    , encodeField f10
    , encodeField f11
    , encodeField f12
    , encodeField f13
    , encodeField f14
    , encodeField f15
    ]

instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3, A.ToJSON v4, A.ToJSON v5, A.ToJSON v6, A.ToJSON v7, A.ToJSON v8, A.ToJSON v9, A.ToJSON v10, A.ToJSON v11, A.ToJSON v12, A.ToJSON v13, A.ToJSON v14, A.ToJSON v15, A.ToJSON v16) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14, k15 := v15, k16 := v16) where
  toJSON (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    , encodeField f4
    , encodeField f5
    , encodeField f6
    , encodeField f7
    , encodeField f8
    , encodeField f9
    , encodeField f10
    , encodeField f11
    , encodeField f12
    , encodeField f13
    , encodeField f14
    , encodeField f15
    , encodeField f16
    ]

instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3, A.ToJSON v4, A.ToJSON v5, A.ToJSON v6, A.ToJSON v7, A.ToJSON v8, A.ToJSON v9, A.ToJSON v10, A.ToJSON v11, A.ToJSON v12, A.ToJSON v13, A.ToJSON v14, A.ToJSON v15, A.ToJSON v16, A.ToJSON v17) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14, k15 := v15, k16 := v16, k17 := v17) where
  toJSON (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    , encodeField f4
    , encodeField f5
    , encodeField f6
    , encodeField f7
    , encodeField f8
    , encodeField f9
    , encodeField f10
    , encodeField f11
    , encodeField f12
    , encodeField f13
    , encodeField f14
    , encodeField f15
    , encodeField f16
    , encodeField f17
    ]

instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3, A.ToJSON v4, A.ToJSON v5, A.ToJSON v6, A.ToJSON v7, A.ToJSON v8, A.ToJSON v9, A.ToJSON v10, A.ToJSON v11, A.ToJSON v12, A.ToJSON v13, A.ToJSON v14, A.ToJSON v15, A.ToJSON v16, A.ToJSON v17, A.ToJSON v18) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14, k15 := v15, k16 := v16, k17 := v17, k18 := v18) where
  toJSON (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    , encodeField f4
    , encodeField f5
    , encodeField f6
    , encodeField f7
    , encodeField f8
    , encodeField f9
    , encodeField f10
    , encodeField f11
    , encodeField f12
    , encodeField f13
    , encodeField f14
    , encodeField f15
    , encodeField f16
    , encodeField f17
    , encodeField f18
    ]

instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3, A.ToJSON v4, A.ToJSON v5, A.ToJSON v6, A.ToJSON v7, A.ToJSON v8, A.ToJSON v9, A.ToJSON v10, A.ToJSON v11, A.ToJSON v12, A.ToJSON v13, A.ToJSON v14, A.ToJSON v15, A.ToJSON v16, A.ToJSON v17, A.ToJSON v18, A.ToJSON v19) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14, k15 := v15, k16 := v16, k17 := v17, k18 := v18, k19 := v19) where
  toJSON (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    , encodeField f4
    , encodeField f5
    , encodeField f6
    , encodeField f7
    , encodeField f8
    , encodeField f9
    , encodeField f10
    , encodeField f11
    , encodeField f12
    , encodeField f13
    , encodeField f14
    , encodeField f15
    , encodeField f16
    , encodeField f17
    , encodeField f18
    , encodeField f19
    ]

instance {-# OVERLAPPING #-} (A.ToJSON v1, A.ToJSON v2, A.ToJSON v3, A.ToJSON v4, A.ToJSON v5, A.ToJSON v6, A.ToJSON v7, A.ToJSON v8, A.ToJSON v9, A.ToJSON v10, A.ToJSON v11, A.ToJSON v12, A.ToJSON v13, A.ToJSON v14, A.ToJSON v15, A.ToJSON v16, A.ToJSON v17, A.ToJSON v18, A.ToJSON v19, A.ToJSON v20) => A.ToJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14, k15 := v15, k16 := v16, k17 := v17, k18 := v18, k19 := v19, k20 := v20) where
  toJSON (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20) = A.object
    [ encodeField f1
    , encodeField f2
    , encodeField f3
    , encodeField f4
    , encodeField f5
    , encodeField f6
    , encodeField f7
    , encodeField f8
    , encodeField f9
    , encodeField f10
    , encodeField f11
    , encodeField f12
    , encodeField f13
    , encodeField f14
    , encodeField f15
    , encodeField f16
    , encodeField f17
    , encodeField f18
    , encodeField f19
    , encodeField f20
    ]














instance (KnownSymbol k, A.FromJSON v, FieldDecodeable k v) => A.FromJSON (k := v) where
  parseJSON = A.withObject "record field" decodeField
instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, A.FromJSON v1, A.FromJSON v2, FieldDecodeable k1 v1, FieldDecodeable k2 v2) => A.FromJSON (k1 := v1, k2 := v2) where
  parseJSON = A.withObject "record field" $ \o -> do
    f1 <- decodeField o
    f2 <- decodeField o
    return (f1, f2)
instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, FieldDecodeable k1 v1, FieldDecodeable k2 v2, FieldDecodeable k3 v3) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3) where
  parseJSON = A.withObject "record field" $ \o -> do
    f1 <- decodeField o
    f2 <- decodeField o
    f3 <- decodeField o
    return (f1, f2, f3)
instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, KnownSymbol k4, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, A.FromJSON v4, FieldDecodeable k1 v1, FieldDecodeable k2 v2, FieldDecodeable k3 v3, FieldDecodeable k4 v4) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4) where
  parseJSON = A.withObject "record field" $ \o -> do
    f1 <- decodeField o
    f2 <- decodeField o
    f3 <- decodeField o
    f4 <- decodeField o
    return (f1, f2, f3, f4)
instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, KnownSymbol k4, KnownSymbol k5, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, A.FromJSON v4, A.FromJSON v4, A.FromJSON v5, FieldDecodeable k1 v1, FieldDecodeable k2 v2, FieldDecodeable k3 v3, FieldDecodeable k4 v4, FieldDecodeable k5 v5) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5) where
  parseJSON = A.withObject "record field" $ \o -> do
    f1 <- decodeField o
    f2 <- decodeField o
    f3 <- decodeField o
    f4 <- decodeField o
    f5 <- decodeField o
    return (f1, f2, f3, f4, f5)
instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, KnownSymbol k4, KnownSymbol k5, KnownSymbol k6, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, A.FromJSON v4, A.FromJSON v4, A.FromJSON v5, A.FromJSON v6, FieldDecodeable k1 v1, FieldDecodeable k2 v2, FieldDecodeable k3 v3, FieldDecodeable k4 v4, FieldDecodeable k5 v5, FieldDecodeable k6 v6) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6) where
  parseJSON = A.withObject "record field" $ \o -> do
    f1 <- decodeField o
    f2 <- decodeField o
    f3 <- decodeField o
    f4 <- decodeField o
    f5 <- decodeField o
    f6 <- decodeField o
    return (f1, f2, f3, f4, f5, f6)
instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, KnownSymbol k4, KnownSymbol k5, KnownSymbol k6, KnownSymbol k7, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, A.FromJSON v4, A.FromJSON v4, A.FromJSON v5, A.FromJSON v6, A.FromJSON v7, FieldDecodeable k1 v1, FieldDecodeable k2 v2, FieldDecodeable k3 v3, FieldDecodeable k4 v4, FieldDecodeable k5 v5, FieldDecodeable k6 v6, FieldDecodeable k7 v7) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7) where
  parseJSON = A.withObject "record field" $ \o -> do
    f1 <- decodeField o
    f2 <- decodeField o
    f3 <- decodeField o
    f4 <- decodeField o
    f5 <- decodeField o
    f6 <- decodeField o
    f7 <- decodeField o
    return (f1, f2, f3, f4, f5, f6, f7)
instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, KnownSymbol k4, KnownSymbol k5, KnownSymbol k6, KnownSymbol k7, KnownSymbol k8, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, A.FromJSON v4, A.FromJSON v4, A.FromJSON v5, A.FromJSON v6, A.FromJSON v7, A.FromJSON v8, FieldDecodeable k1 v1, FieldDecodeable k2 v2, FieldDecodeable k3 v3, FieldDecodeable k4 v4, FieldDecodeable k5 v5, FieldDecodeable k6 v6, FieldDecodeable k7 v7, FieldDecodeable k8 v8) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8) where
  parseJSON = A.withObject "record field" $ \o -> do
    f1 <- decodeField o
    f2 <- decodeField o
    f3 <- decodeField o
    f4 <- decodeField o
    f5 <- decodeField o
    f6 <- decodeField o
    f7 <- decodeField o
    f8 <- decodeField o
    return (f1, f2, f3, f4, f5, f6, f7, f8)
instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, KnownSymbol k4, KnownSymbol k5, KnownSymbol k6, KnownSymbol k7, KnownSymbol k8, KnownSymbol k9, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, A.FromJSON v4, A.FromJSON v4, A.FromJSON v5, A.FromJSON v6, A.FromJSON v7, A.FromJSON v8, A.FromJSON v9, FieldDecodeable k1 v1, FieldDecodeable k2 v2, FieldDecodeable k3 v3, FieldDecodeable k4 v4, FieldDecodeable k5 v5, FieldDecodeable k6 v6, FieldDecodeable k7 v7, FieldDecodeable k8 v8, FieldDecodeable k9 v9) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9) where
  parseJSON = A.withObject "record field" $ \o -> do
    f1 <- decodeField o
    f2 <- decodeField o
    f3 <- decodeField o
    f4 <- decodeField o
    f5 <- decodeField o
    f6 <- decodeField o
    f7 <- decodeField o
    f8 <- decodeField o
    f9 <- decodeField o
    return (f1, f2, f3, f4, f5, f6, f7, f8, f9)
instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, KnownSymbol k4, KnownSymbol k5, KnownSymbol k6, KnownSymbol k7, KnownSymbol k8, KnownSymbol k9, KnownSymbol k10, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, A.FromJSON v4, A.FromJSON v4, A.FromJSON v5, A.FromJSON v6, A.FromJSON v7, A.FromJSON v8, A.FromJSON v9, A.FromJSON v10, FieldDecodeable k1 v1, FieldDecodeable k2 v2, FieldDecodeable k3 v3, FieldDecodeable k4 v4, FieldDecodeable k5 v5, FieldDecodeable k6 v6, FieldDecodeable k7 v7, FieldDecodeable k8 v8, FieldDecodeable k9 v9, FieldDecodeable k10 v10) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10) where
  parseJSON = A.withObject "record field" $ \o -> do
    f1 <- decodeField o
    f2 <- decodeField o
    f3 <- decodeField o
    f4 <- decodeField o
    f5 <- decodeField o
    f6 <- decodeField o
    f7 <- decodeField o
    f8 <- decodeField o
    f9 <- decodeField o
    f10 <- decodeField o
    return (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, KnownSymbol k4, KnownSymbol k5, KnownSymbol k6, KnownSymbol k7, KnownSymbol k8, KnownSymbol k9, KnownSymbol k10, KnownSymbol k11, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, A.FromJSON v4, A.FromJSON v4, A.FromJSON v5, A.FromJSON v6, A.FromJSON v7, A.FromJSON v8, A.FromJSON v9, A.FromJSON v10, A.FromJSON v11, FieldDecodeable k1 v1, FieldDecodeable k2 v2, FieldDecodeable k3 v3, FieldDecodeable k4 v4, FieldDecodeable k5 v5, FieldDecodeable k6 v6, FieldDecodeable k7 v7, FieldDecodeable k8 v8, FieldDecodeable k9 v9, FieldDecodeable k10 v10, FieldDecodeable k11 v11) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11) where
  parseJSON = A.withObject "record field" $ \o -> do
    f1 <- decodeField o
    f2 <- decodeField o
    f3 <- decodeField o
    f4 <- decodeField o
    f5 <- decodeField o
    f6 <- decodeField o
    f7 <- decodeField o
    f8 <- decodeField o
    f9 <- decodeField o
    f10 <- decodeField o
    f11 <- decodeField o
    return (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)
instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, KnownSymbol k4, KnownSymbol k5, KnownSymbol k6, KnownSymbol k7, KnownSymbol k8, KnownSymbol k9, KnownSymbol k10, KnownSymbol k11, KnownSymbol k12, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, A.FromJSON v4, A.FromJSON v4, A.FromJSON v5, A.FromJSON v6, A.FromJSON v7, A.FromJSON v8, A.FromJSON v9, A.FromJSON v10, A.FromJSON v11, A.FromJSON v12, FieldDecodeable k1 v1, FieldDecodeable k2 v2, FieldDecodeable k3 v3, FieldDecodeable k4 v4, FieldDecodeable k5 v5, FieldDecodeable k6 v6, FieldDecodeable k7 v7, FieldDecodeable k8 v8, FieldDecodeable k9 v9, FieldDecodeable k10 v10, FieldDecodeable k11 v11, FieldDecodeable k12 v12) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12) where
  parseJSON = A.withObject "record field" $ \o -> do
    f1 <- decodeField o
    f2 <- decodeField o
    f3 <- decodeField o
    f4 <- decodeField o
    f5 <- decodeField o
    f6 <- decodeField o
    f7 <- decodeField o
    f8 <- decodeField o
    f9 <- decodeField o
    f10 <- decodeField o
    f11 <- decodeField o
    f12 <- decodeField o
    return (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)
instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, KnownSymbol k4, KnownSymbol k5, KnownSymbol k6, KnownSymbol k7, KnownSymbol k8, KnownSymbol k9, KnownSymbol k10, KnownSymbol k11, KnownSymbol k12, KnownSymbol k13, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, A.FromJSON v4, A.FromJSON v4, A.FromJSON v5, A.FromJSON v6, A.FromJSON v7, A.FromJSON v8, A.FromJSON v9, A.FromJSON v10, A.FromJSON v11, A.FromJSON v12, A.FromJSON v13, FieldDecodeable k1 v1, FieldDecodeable k2 v2, FieldDecodeable k3 v3, FieldDecodeable k4 v4, FieldDecodeable k5 v5, FieldDecodeable k6 v6, FieldDecodeable k7 v7, FieldDecodeable k8 v8, FieldDecodeable k9 v9, FieldDecodeable k10 v10, FieldDecodeable k11 v11, FieldDecodeable k12 v12, FieldDecodeable k13 v13) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13) where
  parseJSON = A.withObject "record field" $ \o -> do
    f1 <- decodeField o
    f2 <- decodeField o
    f3 <- decodeField o
    f4 <- decodeField o
    f5 <- decodeField o
    f6 <- decodeField o
    f7 <- decodeField o
    f8 <- decodeField o
    f9 <- decodeField o
    f10 <- decodeField o
    f11 <- decodeField o
    f12 <- decodeField o
    f13 <- decodeField o
    return (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)
instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, KnownSymbol k4, KnownSymbol k5, KnownSymbol k6, KnownSymbol k7, KnownSymbol k8, KnownSymbol k9, KnownSymbol k10, KnownSymbol k11, KnownSymbol k12, KnownSymbol k13, KnownSymbol k14, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, A.FromJSON v4, A.FromJSON v4, A.FromJSON v5, A.FromJSON v6, A.FromJSON v7, A.FromJSON v8, A.FromJSON v9, A.FromJSON v10, A.FromJSON v11, A.FromJSON v12, A.FromJSON v13, A.FromJSON v14, FieldDecodeable k1 v1, FieldDecodeable k2 v2, FieldDecodeable k3 v3, FieldDecodeable k4 v4, FieldDecodeable k5 v5, FieldDecodeable k6 v6, FieldDecodeable k7 v7, FieldDecodeable k8 v8, FieldDecodeable k9 v9, FieldDecodeable k10 v10, FieldDecodeable k11 v11, FieldDecodeable k12 v12, FieldDecodeable k13 v13, FieldDecodeable k14 v14) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14) where
  parseJSON = A.withObject "record field" $ \o -> do
    f1 <- decodeField o
    f2 <- decodeField o
    f3 <- decodeField o
    f4 <- decodeField o
    f5 <- decodeField o
    f6 <- decodeField o
    f7 <- decodeField o
    f8 <- decodeField o
    f9 <- decodeField o
    f10 <- decodeField o
    f11 <- decodeField o
    f12 <- decodeField o
    f13 <- decodeField o
    f14 <- decodeField o
    return (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)

instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, KnownSymbol k4, KnownSymbol k5, KnownSymbol k6, KnownSymbol k7, KnownSymbol k8, KnownSymbol k9, KnownSymbol k10, KnownSymbol k11, KnownSymbol k12, KnownSymbol k13, KnownSymbol k14, KnownSymbol k15, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, A.FromJSON v4, A.FromJSON v4, A.FromJSON v5, A.FromJSON v6, A.FromJSON v7, A.FromJSON v8, A.FromJSON v9, A.FromJSON v10, A.FromJSON v11, A.FromJSON v12, A.FromJSON v13, A.FromJSON v14, A.FromJSON v15, FieldDecodeable k1 v1, FieldDecodeable k2 v2, FieldDecodeable k3 v3, FieldDecodeable k4 v4, FieldDecodeable k5 v5, FieldDecodeable k6 v6, FieldDecodeable k7 v7, FieldDecodeable k8 v8, FieldDecodeable k9 v9, FieldDecodeable k10 v10, FieldDecodeable k11 v11, FieldDecodeable k12 v12, FieldDecodeable k13 v13, FieldDecodeable k14 v14, FieldDecodeable k15 v15) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14, k15 := v15) where
  parseJSON = A.withObject "record field" $ \o -> do
    f1 <- decodeField o
    f2 <- decodeField o
    f3 <- decodeField o
    f4 <- decodeField o
    f5 <- decodeField o
    f6 <- decodeField o
    f7 <- decodeField o
    f8 <- decodeField o
    f9 <- decodeField o
    f10 <- decodeField o
    f11 <- decodeField o
    f12 <- decodeField o
    f13 <- decodeField o
    f14 <- decodeField o
    f15 <- decodeField o
    return (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)


-- instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, KnownSymbol k4, KnownSymbol k5, KnownSymbol k6, KnownSymbol k7, KnownSymbol k8, KnownSymbol k9, KnownSymbol k10, KnownSymbol k11, KnownSymbol k12, KnownSymbol k13, KnownSymbol k14, KnownSymbol k15, KnownSymbol k16, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, A.FromJSON v4, A.FromJSON v4, A.FromJSON v5, A.FromJSON v6, A.FromJSON v7, A.FromJSON v8, A.FromJSON v9, A.FromJSON v10, A.FromJSON v11, A.FromJSON v12, A.FromJSON v13, A.FromJSON v14, A.FromJSON v15, A.FromJSON v16) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14, k15 := v15, k16 := v16) where
--   parseJSON = A.withObject "record field" $ \o -> do
--     f1 <- decodeField o
--     f2 <- decodeField o
--     f3 <- decodeField o
--     f4 <- decodeField o
--     f5 <- decodeField o
--     f6 <- decodeField o
--     f7 <- decodeField o
--     f8 <- decodeField o
--     f9 <- decodeField o
--     f10 <- decodeField o
--     f11 <- decodeField o
--     f12 <- decodeField o
--     f13 <- decodeField o
--     f14 <- decodeField o
--     f15 <- decodeField o
--     f16 <- decodeField o
--     return (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)
-- 
-- instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, KnownSymbol k4, KnownSymbol k5, KnownSymbol k6, KnownSymbol k7, KnownSymbol k8, KnownSymbol k9, KnownSymbol k10, KnownSymbol k11, KnownSymbol k12, KnownSymbol k13, KnownSymbol k14, KnownSymbol k15, KnownSymbol k16, KnownSymbol k17, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, A.FromJSON v4, A.FromJSON v4, A.FromJSON v5, A.FromJSON v6, A.FromJSON v7, A.FromJSON v8, A.FromJSON v9, A.FromJSON v10, A.FromJSON v11, A.FromJSON v12, A.FromJSON v13, A.FromJSON v14, A.FromJSON v15, A.FromJSON v16, A.FromJSON v17) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14, k15 := v15, k16 := v16, k17 := v17) where
--   parseJSON = A.withObject "record field" $ \o -> do
--     f1 <- decodeField o
--     f2 <- decodeField o
--     f3 <- decodeField o
--     f4 <- decodeField o
--     f5 <- decodeField o
--     f6 <- decodeField o
--     f7 <- decodeField o
--     f8 <- decodeField o
--     f9 <- decodeField o
--     f10 <- decodeField o
--     f11 <- decodeField o
--     f12 <- decodeField o
--     f13 <- decodeField o
--     f14 <- decodeField o
--     f15 <- decodeField o
--     f16 <- decodeField o
--     f17 <- decodeField o
--     return (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17)
-- 
-- instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, KnownSymbol k4, KnownSymbol k5, KnownSymbol k6, KnownSymbol k7, KnownSymbol k8, KnownSymbol k9, KnownSymbol k10, KnownSymbol k11, KnownSymbol k12, KnownSymbol k13, KnownSymbol k14, KnownSymbol k15, KnownSymbol k16, KnownSymbol k17, KnownSymbol k18, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, A.FromJSON v4, A.FromJSON v4, A.FromJSON v5, A.FromJSON v6, A.FromJSON v7, A.FromJSON v8, A.FromJSON v9, A.FromJSON v10, A.FromJSON v11, A.FromJSON v12, A.FromJSON v13, A.FromJSON v14, A.FromJSON v15, A.FromJSON v16, A.FromJSON v17, A.FromJSON v18) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14, k15 := v15, k16 := v16, k17 := v17, k18 := v18) where
--   parseJSON = A.withObject "record field" $ \o -> do
--     f1 <- decodeField o
--     f2 <- decodeField o
--     f3 <- decodeField o
--     f4 <- decodeField o
--     f5 <- decodeField o
--     f6 <- decodeField o
--     f7 <- decodeField o
--     f8 <- decodeField o
--     f9 <- decodeField o
--     f10 <- decodeField o
--     f11 <- decodeField o
--     f12 <- decodeField o
--     f13 <- decodeField o
--     f14 <- decodeField o
--     f15 <- decodeField o
--     f16 <- decodeField o
--     f17 <- decodeField o
--     f18 <- decodeField o
--     return (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18)
-- 
-- instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, KnownSymbol k4, KnownSymbol k5, KnownSymbol k6, KnownSymbol k7, KnownSymbol k8, KnownSymbol k9, KnownSymbol k10, KnownSymbol k11, KnownSymbol k12, KnownSymbol k13, KnownSymbol k14, KnownSymbol k15, KnownSymbol k16, KnownSymbol k17, KnownSymbol k18, KnownSymbol k19, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, A.FromJSON v4, A.FromJSON v4, A.FromJSON v5, A.FromJSON v6, A.FromJSON v7, A.FromJSON v8, A.FromJSON v9, A.FromJSON v10, A.FromJSON v11, A.FromJSON v12, A.FromJSON v13, A.FromJSON v14, A.FromJSON v15, A.FromJSON v16, A.FromJSON v17, A.FromJSON v18, A.FromJSON v19) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14, k15 := v15, k16 := v16, k17 := v17, k18 := v18, k19 := v19) where
--   parseJSON = A.withObject "record field" $ \o -> do
--     f1 <- decodeField o
--     f2 <- decodeField o
--     f3 <- decodeField o
--     f4 <- decodeField o
--     f5 <- decodeField o
--     f6 <- decodeField o
--     f7 <- decodeField o
--     f8 <- decodeField o
--     f9 <- decodeField o
--     f10 <- decodeField o
--     f11 <- decodeField o
--     f12 <- decodeField o
--     f13 <- decodeField o
--     f14 <- decodeField o
--     f15 <- decodeField o
--     f16 <- decodeField o
--     f17 <- decodeField o
--     f18 <- decodeField o
--     f19 <- decodeField o
--     return (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19)
-- 
-- instance {-# OVERLAPPING #-} (KnownSymbol k1, KnownSymbol k2, KnownSymbol k3, KnownSymbol k4, KnownSymbol k5, KnownSymbol k6, KnownSymbol k7, KnownSymbol k8, KnownSymbol k9, KnownSymbol k10, KnownSymbol k11, KnownSymbol k12, KnownSymbol k13, KnownSymbol k14, KnownSymbol k15, KnownSymbol k16, KnownSymbol k17, KnownSymbol k18, KnownSymbol k19, KnownSymbol k20, A.FromJSON v1, A.FromJSON v2, A.FromJSON v3, A.FromJSON v4, A.FromJSON v4, A.FromJSON v5, A.FromJSON v6, A.FromJSON v7, A.FromJSON v8, A.FromJSON v9, A.FromJSON v10, A.FromJSON v11, A.FromJSON v12, A.FromJSON v13, A.FromJSON v14, A.FromJSON v15, A.FromJSON v16, A.FromJSON v17, A.FromJSON v18, A.FromJSON v19, A.FromJSON v20) => A.FromJSON (k1 := v1, k2 := v2, k3 := v3, k4 := v4, k5 := v5, k6 := v6, k7 := v7, k8 := v8, k9 := v9, k10 := v10, k11 := v11, k12 := v12, k13 := v13, k14 := v14, k15 := v15, k16 := v16, k17 := v17, k18 := v18, k19 := v19, k20 := v20) where
--   parseJSON = A.withObject "record field" $ \o -> do
--     f1 <- decodeField o
--     f2 <- decodeField o
--     f3 <- decodeField o
--     f4 <- decodeField o
--     f5 <- decodeField o
--     f6 <- decodeField o
--     f7 <- decodeField o
--     f8 <- decodeField o
--     f9 <- decodeField o
--     f10 <- decodeField o
--     f11 <- decodeField o
--     f12 <- decodeField o
--     f13 <- decodeField o
--     f14 <- decodeField o
--     f15 <- decodeField o
--     f16 <- decodeField o
--     f17 <- decodeField o
--     f18 <- decodeField o
--     f19 <- decodeField o
--     f20 <- decodeField o
--     return (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20)

















class KnownSymbol k => FieldDecodeable k v where
  decodeField :: A.Object -> A.Parser (k := v)


instance {-# OVERLAPPING #-} (KnownSymbol k) => FieldDecodeable k () where
  decodeField o = if not (hasNormalField key o)
    then return (Proxy @k := ())
    else decodeField' o
    where
      key :: Text
      key = Text.pack $ symbolVal (Proxy @k)

instance {-# OVERLAPPING #-} (KnownSymbol k, A.FromJSON v) => FieldDecodeable k (Maybe v) where
  decodeField o = if not (hasNormalField key o)
    then return (Proxy @k := Nothing)
    else decodeField' o
    where
      key :: Text
      key = Text.pack $ symbolVal (Proxy @k)

instance {-# OVERLAPPING #-} (KnownSymbol k, A.FromJSON v) => FieldDecodeable k v where
  decodeField o = decodeField' o







encodeField
  :: forall k v res. (A.ToJSON v, A.KeyValue res) => (k := v) -> res
encodeField (_ := v) = key .= v
  where
    key :: Text
    key = camlToSnakeCase $ Text.pack $ symbolVal (Proxy :: Proxy k)


decodeField'
  :: forall k v res. ( KnownSymbol k, A.FromJSON v) => A.Object -> A.Parser (k := v)
decodeField' o = do
  res <- A.parseJSON value
  return (Proxy @k := res)
  where
    value :: A.Value
    value = normalField label o
    
    label :: Text
    label = Text.pack $ symbolVal (Proxy @k)


hasNormalField :: Text -> A.Object -> Bool
hasNormalField key = extract . HashMap.toList . HashMap.filterWithKey f
  where
    extract :: [(Text, A.Value)] -> Bool
    extract = \case
      [(_, val)] -> True
      _ -> False
    
    f k _ = normalize key == normalize k


normalField :: Text -> A.Object -> A.Value
normalField key = extract . HashMap.toList . HashMap.filterWithKey f
  where
    extract :: [(Text, A.Value)] -> A.Value
    extract [(_, val)] = val
    -- TODO: find closest match
    -- extract xs = error $ show (xs, key)
    
    f k _ = normalize key == normalize k


normalize :: Text -> Text
normalize = Text.filter (not . f) . Text.toCaseFold
  where
    f '_' = True
    f '-' = True
    f _ = False



run :: IO ()
run = Text.putStrLn $ camlToSnakeCase "loremIpsumDollar"











