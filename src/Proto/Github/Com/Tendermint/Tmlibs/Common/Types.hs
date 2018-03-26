{- This file was auto-generated from github.com/tendermint/tmlibs/common/types.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, MultiParamTypeClasses, FlexibleContexts,
  FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude
  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Proto.Github.Com.Tendermint.Tmlibs.Common.Types where
import qualified Data.ProtoLens.Reexport.Prelude as Prelude
import qualified Data.ProtoLens.Reexport.Data.Int as Data.Int
import qualified Data.ProtoLens.Reexport.Data.Word as Data.Word
import qualified Data.ProtoLens.Reexport.Data.ProtoLens
       as Data.ProtoLens
import qualified
       Data.ProtoLens.Reexport.Data.ProtoLens.Message.Enum
       as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Reexport.Lens.Family2
       as Lens.Family2
import qualified Data.ProtoLens.Reexport.Lens.Family2.Unchecked
       as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Reexport.Data.Default.Class
       as Data.Default.Class
import qualified Data.ProtoLens.Reexport.Data.Text as Data.Text
import qualified Data.ProtoLens.Reexport.Data.Map as Data.Map
import qualified Data.ProtoLens.Reexport.Data.ByteString
       as Data.ByteString
import qualified Data.ProtoLens.Reexport.Lens.Labels as Lens.Labels
import qualified Proto.Github.Com.Gogo.Protobuf.Gogoproto.Gogo

data KI64Pair = KI64Pair{_KI64Pair'key ::
                         !Data.ByteString.ByteString,
                         _KI64Pair'value :: !Data.Int.Int64}
              deriving (Prelude.Show, Prelude.Eq)

instance (a ~ Data.ByteString.ByteString,
          b ~ Data.ByteString.ByteString, Prelude.Functor f) =>
         Lens.Labels.HasLens "key" f KI64Pair KI64Pair a b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _KI64Pair'key
              (\ x__ y__ -> x__{_KI64Pair'key = y__})

instance (a ~ Data.Int.Int64, b ~ Data.Int.Int64,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "value" f KI64Pair KI64Pair a b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _KI64Pair'value
              (\ x__ y__ -> x__{_KI64Pair'value = y__})

instance Data.Default.Class.Default KI64Pair where
        def
          = KI64Pair{_KI64Pair'key = Data.ProtoLens.fieldDefault,
                     _KI64Pair'value = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message KI64Pair where
        descriptor
          = let key__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "key"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional key)
                      :: Data.ProtoLens.FieldDescriptor KI64Pair
                value__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "value"
                      (Data.ProtoLens.Int64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional value)
                      :: Data.ProtoLens.FieldDescriptor KI64Pair
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, key__field_descriptor),
                    (Data.ProtoLens.Tag 2, value__field_descriptor)])
                (Data.Map.fromList
                   [("key", key__field_descriptor),
                    ("value", value__field_descriptor)])

data KVPair = KVPair{_KVPair'key :: !Data.ByteString.ByteString,
                     _KVPair'value :: !Data.ByteString.ByteString}
            deriving (Prelude.Show, Prelude.Eq)

instance (a ~ Data.ByteString.ByteString,
          b ~ Data.ByteString.ByteString, Prelude.Functor f) =>
         Lens.Labels.HasLens "key" f KVPair KVPair a b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _KVPair'key
              (\ x__ y__ -> x__{_KVPair'key = y__})

instance (a ~ Data.ByteString.ByteString,
          b ~ Data.ByteString.ByteString, Prelude.Functor f) =>
         Lens.Labels.HasLens "value" f KVPair KVPair a b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _KVPair'value
              (\ x__ y__ -> x__{_KVPair'value = y__})

instance Data.Default.Class.Default KVPair where
        def
          = KVPair{_KVPair'key = Data.ProtoLens.fieldDefault,
                   _KVPair'value = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message KVPair where
        descriptor
          = let key__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "key"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional key)
                      :: Data.ProtoLens.FieldDescriptor KVPair
                value__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "value"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional value)
                      :: Data.ProtoLens.FieldDescriptor KVPair
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, key__field_descriptor),
                    (Data.ProtoLens.Tag 2, value__field_descriptor)])
                (Data.Map.fromList
                   [("key", key__field_descriptor),
                    ("value", value__field_descriptor)])

key ::
    forall f s t a b . (Lens.Labels.HasLens "key" f s t a b) =>
      Lens.Family2.LensLike f s t a b
key
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "key")

value ::
      forall f s t a b . (Lens.Labels.HasLens "value" f s t a b) =>
        Lens.Family2.LensLike f s t a b
value
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "value")