{- This file was auto-generated from onnx.proto3 by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.Onnx
       (AttributeProto(..), AttributeProto'AttributeType(..),
        AttributeProto'AttributeType(),
        AttributeProto'AttributeType'UnrecognizedValue, GraphProto(..),
        ModelProto(..), NodeProto(..), OperatorSetIdProto(..),
        StringStringEntryProto(..), TensorProto(..),
        TensorProto'DataType(..), TensorProto'DataType(),
        TensorProto'DataType'UnrecognizedValue, TensorProto'Segment(..),
        TensorShapeProto(..), TensorShapeProto'Dimension(..),
        TensorShapeProto'Dimension'Value(..),
        _TensorShapeProto'Dimension'DimValue,
        _TensorShapeProto'Dimension'DimParam, TypeProto(..),
        TypeProto'Value(..), _TypeProto'TensorType, TypeProto'Tensor(..),
        ValueInfoProto(..), Version(..), Version(),
        Version'UnrecognizedValue)
       where
import qualified Data.ProtoLens.Reexport.Lens.Labels.Prism
       as Lens.Labels.Prism
import qualified Data.ProtoLens.Reexport.Prelude as Prelude
import qualified Data.ProtoLens.Reexport.Data.Int as Data.Int
import qualified Data.ProtoLens.Reexport.Data.Word as Data.Word
import qualified Data.ProtoLens.Reexport.Data.ProtoLens
       as Data.ProtoLens
import qualified
       Data.ProtoLens.Reexport.Data.ProtoLens.Message.Enum
       as Data.ProtoLens.Message.Enum
import qualified
       Data.ProtoLens.Reexport.Data.ProtoLens.Service.Types
       as Data.ProtoLens.Service.Types
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
import qualified Data.ProtoLens.Reexport.Data.ByteString.Char8
       as Data.ByteString.Char8
import qualified Data.ProtoLens.Reexport.Lens.Labels as Lens.Labels
import qualified Data.ProtoLens.Reexport.Text.Read as Text.Read

{- | Fields :

    * 'Proto.Onnx_Fields.name' @:: Lens' AttributeProto Data.Text.Text@
    * 'Proto.Onnx_Fields.docString' @:: Lens' AttributeProto Data.Text.Text@
    * 'Proto.Onnx_Fields.type'' @:: Lens' AttributeProto AttributeProto'AttributeType@
    * 'Proto.Onnx_Fields.f' @:: Lens' AttributeProto Prelude.Float@
    * 'Proto.Onnx_Fields.i' @:: Lens' AttributeProto Data.Int.Int64@
    * 'Proto.Onnx_Fields.s' @:: Lens' AttributeProto Data.ByteString.ByteString@
    * 'Proto.Onnx_Fields.t' @:: Lens' AttributeProto TensorProto@
    * 'Proto.Onnx_Fields.maybe't' @:: Lens' AttributeProto (Prelude.Maybe TensorProto)@
    * 'Proto.Onnx_Fields.g' @:: Lens' AttributeProto GraphProto@
    * 'Proto.Onnx_Fields.maybe'g' @:: Lens' AttributeProto (Prelude.Maybe GraphProto)@
    * 'Proto.Onnx_Fields.floats' @:: Lens' AttributeProto [Prelude.Float]@
    * 'Proto.Onnx_Fields.ints' @:: Lens' AttributeProto [Data.Int.Int64]@
    * 'Proto.Onnx_Fields.strings' @:: Lens' AttributeProto [Data.ByteString.ByteString]@
    * 'Proto.Onnx_Fields.tensors' @:: Lens' AttributeProto [TensorProto]@
    * 'Proto.Onnx_Fields.graphs' @:: Lens' AttributeProto [GraphProto]@
 -}
data AttributeProto = AttributeProto{_AttributeProto'name ::
                                     !Data.Text.Text,
                                     _AttributeProto'docString :: !Data.Text.Text,
                                     _AttributeProto'type' :: !AttributeProto'AttributeType,
                                     _AttributeProto'f :: !Prelude.Float,
                                     _AttributeProto'i :: !Data.Int.Int64,
                                     _AttributeProto's :: !Data.ByteString.ByteString,
                                     _AttributeProto't :: !(Prelude.Maybe TensorProto),
                                     _AttributeProto'g :: !(Prelude.Maybe GraphProto),
                                     _AttributeProto'floats :: ![Prelude.Float],
                                     _AttributeProto'ints :: ![Data.Int.Int64],
                                     _AttributeProto'strings :: ![Data.ByteString.ByteString],
                                     _AttributeProto'tensors :: ![TensorProto],
                                     _AttributeProto'graphs :: ![GraphProto],
                                     _AttributeProto'_unknownFields :: !Data.ProtoLens.FieldSet}
                    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f AttributeProto x a, a ~ b) =>
         Lens.Labels.HasLens f AttributeProto AttributeProto x a b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f AttributeProto "name" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _AttributeProto'name
                 (\ x__ y__ -> x__{_AttributeProto'name = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f AttributeProto "docString" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _AttributeProto'docString
                 (\ x__ y__ -> x__{_AttributeProto'docString = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f AttributeProto "type'"
           (AttributeProto'AttributeType)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _AttributeProto'type'
                 (\ x__ y__ -> x__{_AttributeProto'type' = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f AttributeProto "f" (Prelude.Float)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _AttributeProto'f
                 (\ x__ y__ -> x__{_AttributeProto'f = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f AttributeProto "i" (Data.Int.Int64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _AttributeProto'i
                 (\ x__ y__ -> x__{_AttributeProto'i = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f AttributeProto "s"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _AttributeProto's
                 (\ x__ y__ -> x__{_AttributeProto's = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f AttributeProto "t" (TensorProto)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _AttributeProto't
                 (\ x__ y__ -> x__{_AttributeProto't = y__}))
              (Data.ProtoLens.maybeLens Data.Default.Class.def)
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f AttributeProto "maybe't"
           (Prelude.Maybe TensorProto)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _AttributeProto't
                 (\ x__ y__ -> x__{_AttributeProto't = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f AttributeProto "g" (GraphProto)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _AttributeProto'g
                 (\ x__ y__ -> x__{_AttributeProto'g = y__}))
              (Data.ProtoLens.maybeLens Data.Default.Class.def)
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f AttributeProto "maybe'g"
           (Prelude.Maybe GraphProto)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _AttributeProto'g
                 (\ x__ y__ -> x__{_AttributeProto'g = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f AttributeProto "floats" ([Prelude.Float])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _AttributeProto'floats
                 (\ x__ y__ -> x__{_AttributeProto'floats = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f AttributeProto "ints" ([Data.Int.Int64])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _AttributeProto'ints
                 (\ x__ y__ -> x__{_AttributeProto'ints = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f AttributeProto "strings"
           ([Data.ByteString.ByteString])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _AttributeProto'strings
                 (\ x__ y__ -> x__{_AttributeProto'strings = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f AttributeProto "tensors" ([TensorProto])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _AttributeProto'tensors
                 (\ x__ y__ -> x__{_AttributeProto'tensors = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f AttributeProto "graphs" ([GraphProto])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _AttributeProto'graphs
                 (\ x__ y__ -> x__{_AttributeProto'graphs = y__}))
              Prelude.id
instance Data.Default.Class.Default AttributeProto where
        def
          = AttributeProto{_AttributeProto'name =
                             Data.ProtoLens.fieldDefault,
                           _AttributeProto'docString = Data.ProtoLens.fieldDefault,
                           _AttributeProto'type' = Data.Default.Class.def,
                           _AttributeProto'f = Data.ProtoLens.fieldDefault,
                           _AttributeProto'i = Data.ProtoLens.fieldDefault,
                           _AttributeProto's = Data.ProtoLens.fieldDefault,
                           _AttributeProto't = Prelude.Nothing,
                           _AttributeProto'g = Prelude.Nothing, _AttributeProto'floats = [],
                           _AttributeProto'ints = [], _AttributeProto'strings = [],
                           _AttributeProto'tensors = [], _AttributeProto'graphs = [],
                           _AttributeProto'_unknownFields = ([])}
instance Data.ProtoLens.Message AttributeProto where
        messageName _ = Data.Text.pack "onnx.AttributeProto"
        fieldsByTag
          = let name__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "name"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "name")))
                      :: Data.ProtoLens.FieldDescriptor AttributeProto
                docString__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "doc_string"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "docString")))
                      :: Data.ProtoLens.FieldDescriptor AttributeProto
                type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                         Data.ProtoLens.FieldTypeDescriptor AttributeProto'AttributeType)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "type'")))
                      :: Data.ProtoLens.FieldDescriptor AttributeProto
                f__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "f"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.FloatField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Float)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "f")))
                      :: Data.ProtoLens.FieldDescriptor AttributeProto
                i__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "i"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "i")))
                      :: Data.ProtoLens.FieldDescriptor AttributeProto
                s__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "s"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "s")))
                      :: Data.ProtoLens.FieldDescriptor AttributeProto
                t__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "t"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor TensorProto)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe't")))
                      :: Data.ProtoLens.FieldDescriptor AttributeProto
                g__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "g"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor GraphProto)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'g")))
                      :: Data.ProtoLens.FieldDescriptor AttributeProto
                floats__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "floats"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.FloatField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Float)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Packed
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "floats")))
                      :: Data.ProtoLens.FieldDescriptor AttributeProto
                ints__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "ints"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Packed
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "ints")))
                      :: Data.ProtoLens.FieldDescriptor AttributeProto
                strings__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "strings"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "strings")))
                      :: Data.ProtoLens.FieldDescriptor AttributeProto
                tensors__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "tensors"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor TensorProto)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "tensors")))
                      :: Data.ProtoLens.FieldDescriptor AttributeProto
                graphs__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "graphs"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor GraphProto)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "graphs")))
                      :: Data.ProtoLens.FieldDescriptor AttributeProto
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, name__field_descriptor),
                 (Data.ProtoLens.Tag 13, docString__field_descriptor),
                 (Data.ProtoLens.Tag 20, type'__field_descriptor),
                 (Data.ProtoLens.Tag 2, f__field_descriptor),
                 (Data.ProtoLens.Tag 3, i__field_descriptor),
                 (Data.ProtoLens.Tag 4, s__field_descriptor),
                 (Data.ProtoLens.Tag 5, t__field_descriptor),
                 (Data.ProtoLens.Tag 6, g__field_descriptor),
                 (Data.ProtoLens.Tag 7, floats__field_descriptor),
                 (Data.ProtoLens.Tag 8, ints__field_descriptor),
                 (Data.ProtoLens.Tag 9, strings__field_descriptor),
                 (Data.ProtoLens.Tag 10, tensors__field_descriptor),
                 (Data.ProtoLens.Tag 11, graphs__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _AttributeProto'_unknownFields
              (\ x__ y__ -> x__{_AttributeProto'_unknownFields = y__})
data AttributeProto'AttributeType = AttributeProto'UNDEFINED
                                  | AttributeProto'FLOAT
                                  | AttributeProto'INT
                                  | AttributeProto'STRING
                                  | AttributeProto'TENSOR
                                  | AttributeProto'GRAPH
                                  | AttributeProto'FLOATS
                                  | AttributeProto'INTS
                                  | AttributeProto'STRINGS
                                  | AttributeProto'TENSORS
                                  | AttributeProto'GRAPHS
                                  | AttributeProto'AttributeType'Unrecognized !AttributeProto'AttributeType'UnrecognizedValue
                                  deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
newtype AttributeProto'AttributeType'UnrecognizedValue = AttributeProto'AttributeType'UnrecognizedValue Data.Int.Int32
                                                       deriving (Prelude.Eq, Prelude.Ord,
                                                                 Prelude.Show)
instance Data.ProtoLens.MessageEnum AttributeProto'AttributeType
         where
        maybeToEnum 0 = Prelude.Just AttributeProto'UNDEFINED
        maybeToEnum 1 = Prelude.Just AttributeProto'FLOAT
        maybeToEnum 2 = Prelude.Just AttributeProto'INT
        maybeToEnum 3 = Prelude.Just AttributeProto'STRING
        maybeToEnum 4 = Prelude.Just AttributeProto'TENSOR
        maybeToEnum 5 = Prelude.Just AttributeProto'GRAPH
        maybeToEnum 6 = Prelude.Just AttributeProto'FLOATS
        maybeToEnum 7 = Prelude.Just AttributeProto'INTS
        maybeToEnum 8 = Prelude.Just AttributeProto'STRINGS
        maybeToEnum 9 = Prelude.Just AttributeProto'TENSORS
        maybeToEnum 10 = Prelude.Just AttributeProto'GRAPHS
        maybeToEnum k
          = Prelude.Just
              (AttributeProto'AttributeType'Unrecognized
                 (AttributeProto'AttributeType'UnrecognizedValue
                    (Prelude.fromIntegral k)))
        showEnum AttributeProto'UNDEFINED = "UNDEFINED"
        showEnum AttributeProto'FLOAT = "FLOAT"
        showEnum AttributeProto'INT = "INT"
        showEnum AttributeProto'STRING = "STRING"
        showEnum AttributeProto'TENSOR = "TENSOR"
        showEnum AttributeProto'GRAPH = "GRAPH"
        showEnum AttributeProto'FLOATS = "FLOATS"
        showEnum AttributeProto'INTS = "INTS"
        showEnum AttributeProto'STRINGS = "STRINGS"
        showEnum AttributeProto'TENSORS = "TENSORS"
        showEnum AttributeProto'GRAPHS = "GRAPHS"
        showEnum
          (AttributeProto'AttributeType'Unrecognized
             (AttributeProto'AttributeType'UnrecognizedValue k))
          = Prelude.show k
        readEnum "UNDEFINED" = Prelude.Just AttributeProto'UNDEFINED
        readEnum "FLOAT" = Prelude.Just AttributeProto'FLOAT
        readEnum "INT" = Prelude.Just AttributeProto'INT
        readEnum "STRING" = Prelude.Just AttributeProto'STRING
        readEnum "TENSOR" = Prelude.Just AttributeProto'TENSOR
        readEnum "GRAPH" = Prelude.Just AttributeProto'GRAPH
        readEnum "FLOATS" = Prelude.Just AttributeProto'FLOATS
        readEnum "INTS" = Prelude.Just AttributeProto'INTS
        readEnum "STRINGS" = Prelude.Just AttributeProto'STRINGS
        readEnum "TENSORS" = Prelude.Just AttributeProto'TENSORS
        readEnum "GRAPHS" = Prelude.Just AttributeProto'GRAPHS
        readEnum k
          = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded AttributeProto'AttributeType where
        minBound = AttributeProto'UNDEFINED
        maxBound = AttributeProto'GRAPHS
instance Prelude.Enum AttributeProto'AttributeType where
        toEnum k__
          = Prelude.maybe
              (Prelude.error
                 ((Prelude.++) "toEnum: unknown value for enum AttributeType: "
                    (Prelude.show k__)))
              Prelude.id
              (Data.ProtoLens.maybeToEnum k__)
        fromEnum AttributeProto'UNDEFINED = 0
        fromEnum AttributeProto'FLOAT = 1
        fromEnum AttributeProto'INT = 2
        fromEnum AttributeProto'STRING = 3
        fromEnum AttributeProto'TENSOR = 4
        fromEnum AttributeProto'GRAPH = 5
        fromEnum AttributeProto'FLOATS = 6
        fromEnum AttributeProto'INTS = 7
        fromEnum AttributeProto'STRINGS = 8
        fromEnum AttributeProto'TENSORS = 9
        fromEnum AttributeProto'GRAPHS = 10
        fromEnum
          (AttributeProto'AttributeType'Unrecognized
             (AttributeProto'AttributeType'UnrecognizedValue k))
          = Prelude.fromIntegral k
        succ AttributeProto'GRAPHS
          = Prelude.error
              "AttributeProto'AttributeType.succ: bad argument AttributeProto'GRAPHS. This value would be out of bounds."
        succ AttributeProto'UNDEFINED = AttributeProto'FLOAT
        succ AttributeProto'FLOAT = AttributeProto'INT
        succ AttributeProto'INT = AttributeProto'STRING
        succ AttributeProto'STRING = AttributeProto'TENSOR
        succ AttributeProto'TENSOR = AttributeProto'GRAPH
        succ AttributeProto'GRAPH = AttributeProto'FLOATS
        succ AttributeProto'FLOATS = AttributeProto'INTS
        succ AttributeProto'INTS = AttributeProto'STRINGS
        succ AttributeProto'STRINGS = AttributeProto'TENSORS
        succ AttributeProto'TENSORS = AttributeProto'GRAPHS
        succ _
          = Prelude.error
              "AttributeProto'AttributeType.succ: bad argument: unrecognized value"
        pred AttributeProto'UNDEFINED
          = Prelude.error
              "AttributeProto'AttributeType.pred: bad argument AttributeProto'UNDEFINED. This value would be out of bounds."
        pred AttributeProto'FLOAT = AttributeProto'UNDEFINED
        pred AttributeProto'INT = AttributeProto'FLOAT
        pred AttributeProto'STRING = AttributeProto'INT
        pred AttributeProto'TENSOR = AttributeProto'STRING
        pred AttributeProto'GRAPH = AttributeProto'TENSOR
        pred AttributeProto'FLOATS = AttributeProto'GRAPH
        pred AttributeProto'INTS = AttributeProto'FLOATS
        pred AttributeProto'STRINGS = AttributeProto'INTS
        pred AttributeProto'TENSORS = AttributeProto'STRINGS
        pred AttributeProto'GRAPHS = AttributeProto'TENSORS
        pred _
          = Prelude.error
              "AttributeProto'AttributeType.pred: bad argument: unrecognized value"
        enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
        enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
        enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
        enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.Default.Class.Default AttributeProto'AttributeType
         where
        def = AttributeProto'UNDEFINED
instance Data.ProtoLens.FieldDefault AttributeProto'AttributeType
         where
        fieldDefault = AttributeProto'UNDEFINED
{- | Fields :

    * 'Proto.Onnx_Fields.node' @:: Lens' GraphProto [NodeProto]@
    * 'Proto.Onnx_Fields.name' @:: Lens' GraphProto Data.Text.Text@
    * 'Proto.Onnx_Fields.initializer' @:: Lens' GraphProto [TensorProto]@
    * 'Proto.Onnx_Fields.docString' @:: Lens' GraphProto Data.Text.Text@
    * 'Proto.Onnx_Fields.input' @:: Lens' GraphProto [ValueInfoProto]@
    * 'Proto.Onnx_Fields.output' @:: Lens' GraphProto [ValueInfoProto]@
    * 'Proto.Onnx_Fields.valueInfo' @:: Lens' GraphProto [ValueInfoProto]@
 -}
data GraphProto = GraphProto{_GraphProto'node :: ![NodeProto],
                             _GraphProto'name :: !Data.Text.Text,
                             _GraphProto'initializer :: ![TensorProto],
                             _GraphProto'docString :: !Data.Text.Text,
                             _GraphProto'input :: ![ValueInfoProto],
                             _GraphProto'output :: ![ValueInfoProto],
                             _GraphProto'valueInfo :: ![ValueInfoProto],
                             _GraphProto'_unknownFields :: !Data.ProtoLens.FieldSet}
                deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f GraphProto x a, a ~ b) =>
         Lens.Labels.HasLens f GraphProto GraphProto x a b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f GraphProto "node" ([NodeProto])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GraphProto'node
                 (\ x__ y__ -> x__{_GraphProto'node = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f GraphProto "name" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GraphProto'name
                 (\ x__ y__ -> x__{_GraphProto'name = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f GraphProto "initializer" ([TensorProto])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GraphProto'initializer
                 (\ x__ y__ -> x__{_GraphProto'initializer = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f GraphProto "docString" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GraphProto'docString
                 (\ x__ y__ -> x__{_GraphProto'docString = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f GraphProto "input" ([ValueInfoProto])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GraphProto'input
                 (\ x__ y__ -> x__{_GraphProto'input = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f GraphProto "output" ([ValueInfoProto])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GraphProto'output
                 (\ x__ y__ -> x__{_GraphProto'output = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f GraphProto "valueInfo" ([ValueInfoProto])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GraphProto'valueInfo
                 (\ x__ y__ -> x__{_GraphProto'valueInfo = y__}))
              Prelude.id
instance Data.Default.Class.Default GraphProto where
        def
          = GraphProto{_GraphProto'node = [],
                       _GraphProto'name = Data.ProtoLens.fieldDefault,
                       _GraphProto'initializer = [],
                       _GraphProto'docString = Data.ProtoLens.fieldDefault,
                       _GraphProto'input = [], _GraphProto'output = [],
                       _GraphProto'valueInfo = [], _GraphProto'_unknownFields = ([])}
instance Data.ProtoLens.Message GraphProto where
        messageName _ = Data.Text.pack "onnx.GraphProto"
        fieldsByTag
          = let node__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "node"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor NodeProto)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "node")))
                      :: Data.ProtoLens.FieldDescriptor GraphProto
                name__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "name"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "name")))
                      :: Data.ProtoLens.FieldDescriptor GraphProto
                initializer__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "initializer"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor TensorProto)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "initializer")))
                      :: Data.ProtoLens.FieldDescriptor GraphProto
                docString__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "doc_string"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "docString")))
                      :: Data.ProtoLens.FieldDescriptor GraphProto
                input__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "input"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor ValueInfoProto)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "input")))
                      :: Data.ProtoLens.FieldDescriptor GraphProto
                output__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "output"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor ValueInfoProto)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "output")))
                      :: Data.ProtoLens.FieldDescriptor GraphProto
                valueInfo__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "value_info"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor ValueInfoProto)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "valueInfo")))
                      :: Data.ProtoLens.FieldDescriptor GraphProto
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, node__field_descriptor),
                 (Data.ProtoLens.Tag 2, name__field_descriptor),
                 (Data.ProtoLens.Tag 5, initializer__field_descriptor),
                 (Data.ProtoLens.Tag 10, docString__field_descriptor),
                 (Data.ProtoLens.Tag 11, input__field_descriptor),
                 (Data.ProtoLens.Tag 12, output__field_descriptor),
                 (Data.ProtoLens.Tag 13, valueInfo__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _GraphProto'_unknownFields
              (\ x__ y__ -> x__{_GraphProto'_unknownFields = y__})
{- | Fields :

    * 'Proto.Onnx_Fields.irVersion' @:: Lens' ModelProto Data.Int.Int64@
    * 'Proto.Onnx_Fields.opsetImport' @:: Lens' ModelProto [OperatorSetIdProto]@
    * 'Proto.Onnx_Fields.producerName' @:: Lens' ModelProto Data.Text.Text@
    * 'Proto.Onnx_Fields.producerVersion' @:: Lens' ModelProto Data.Text.Text@
    * 'Proto.Onnx_Fields.domain' @:: Lens' ModelProto Data.Text.Text@
    * 'Proto.Onnx_Fields.modelVersion' @:: Lens' ModelProto Data.Int.Int64@
    * 'Proto.Onnx_Fields.docString' @:: Lens' ModelProto Data.Text.Text@
    * 'Proto.Onnx_Fields.graph' @:: Lens' ModelProto GraphProto@
    * 'Proto.Onnx_Fields.maybe'graph' @:: Lens' ModelProto (Prelude.Maybe GraphProto)@
    * 'Proto.Onnx_Fields.metadataProps' @:: Lens' ModelProto [StringStringEntryProto]@
 -}
data ModelProto = ModelProto{_ModelProto'irVersion ::
                             !Data.Int.Int64,
                             _ModelProto'opsetImport :: ![OperatorSetIdProto],
                             _ModelProto'producerName :: !Data.Text.Text,
                             _ModelProto'producerVersion :: !Data.Text.Text,
                             _ModelProto'domain :: !Data.Text.Text,
                             _ModelProto'modelVersion :: !Data.Int.Int64,
                             _ModelProto'docString :: !Data.Text.Text,
                             _ModelProto'graph :: !(Prelude.Maybe GraphProto),
                             _ModelProto'metadataProps :: ![StringStringEntryProto],
                             _ModelProto'_unknownFields :: !Data.ProtoLens.FieldSet}
                deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f ModelProto x a, a ~ b) =>
         Lens.Labels.HasLens f ModelProto ModelProto x a b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f ModelProto "irVersion" (Data.Int.Int64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ModelProto'irVersion
                 (\ x__ y__ -> x__{_ModelProto'irVersion = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f ModelProto "opsetImport"
           ([OperatorSetIdProto])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ModelProto'opsetImport
                 (\ x__ y__ -> x__{_ModelProto'opsetImport = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f ModelProto "producerName" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ModelProto'producerName
                 (\ x__ y__ -> x__{_ModelProto'producerName = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f ModelProto "producerVersion"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ModelProto'producerVersion
                 (\ x__ y__ -> x__{_ModelProto'producerVersion = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f ModelProto "domain" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ModelProto'domain
                 (\ x__ y__ -> x__{_ModelProto'domain = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f ModelProto "modelVersion" (Data.Int.Int64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ModelProto'modelVersion
                 (\ x__ y__ -> x__{_ModelProto'modelVersion = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f ModelProto "docString" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ModelProto'docString
                 (\ x__ y__ -> x__{_ModelProto'docString = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f ModelProto "graph" (GraphProto)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ModelProto'graph
                 (\ x__ y__ -> x__{_ModelProto'graph = y__}))
              (Data.ProtoLens.maybeLens Data.Default.Class.def)
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f ModelProto "maybe'graph"
           (Prelude.Maybe GraphProto)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ModelProto'graph
                 (\ x__ y__ -> x__{_ModelProto'graph = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f ModelProto "metadataProps"
           ([StringStringEntryProto])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ModelProto'metadataProps
                 (\ x__ y__ -> x__{_ModelProto'metadataProps = y__}))
              Prelude.id
instance Data.Default.Class.Default ModelProto where
        def
          = ModelProto{_ModelProto'irVersion = Data.ProtoLens.fieldDefault,
                       _ModelProto'opsetImport = [],
                       _ModelProto'producerName = Data.ProtoLens.fieldDefault,
                       _ModelProto'producerVersion = Data.ProtoLens.fieldDefault,
                       _ModelProto'domain = Data.ProtoLens.fieldDefault,
                       _ModelProto'modelVersion = Data.ProtoLens.fieldDefault,
                       _ModelProto'docString = Data.ProtoLens.fieldDefault,
                       _ModelProto'graph = Prelude.Nothing,
                       _ModelProto'metadataProps = [], _ModelProto'_unknownFields = ([])}
instance Data.ProtoLens.Message ModelProto where
        messageName _ = Data.Text.pack "onnx.ModelProto"
        fieldsByTag
          = let irVersion__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "ir_version"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "irVersion")))
                      :: Data.ProtoLens.FieldDescriptor ModelProto
                opsetImport__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "opset_import"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor OperatorSetIdProto)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "opsetImport")))
                      :: Data.ProtoLens.FieldDescriptor ModelProto
                producerName__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "producer_name"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "producerName")))
                      :: Data.ProtoLens.FieldDescriptor ModelProto
                producerVersion__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "producer_version"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "producerVersion")))
                      :: Data.ProtoLens.FieldDescriptor ModelProto
                domain__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "domain"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "domain")))
                      :: Data.ProtoLens.FieldDescriptor ModelProto
                modelVersion__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "model_version"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "modelVersion")))
                      :: Data.ProtoLens.FieldDescriptor ModelProto
                docString__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "doc_string"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "docString")))
                      :: Data.ProtoLens.FieldDescriptor ModelProto
                graph__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "graph"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor GraphProto)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'graph")))
                      :: Data.ProtoLens.FieldDescriptor ModelProto
                metadataProps__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "metadata_props"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor StringStringEntryProto)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "metadataProps")))
                      :: Data.ProtoLens.FieldDescriptor ModelProto
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, irVersion__field_descriptor),
                 (Data.ProtoLens.Tag 8, opsetImport__field_descriptor),
                 (Data.ProtoLens.Tag 2, producerName__field_descriptor),
                 (Data.ProtoLens.Tag 3, producerVersion__field_descriptor),
                 (Data.ProtoLens.Tag 4, domain__field_descriptor),
                 (Data.ProtoLens.Tag 5, modelVersion__field_descriptor),
                 (Data.ProtoLens.Tag 6, docString__field_descriptor),
                 (Data.ProtoLens.Tag 7, graph__field_descriptor),
                 (Data.ProtoLens.Tag 14, metadataProps__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _ModelProto'_unknownFields
              (\ x__ y__ -> x__{_ModelProto'_unknownFields = y__})
{- | Fields :

    * 'Proto.Onnx_Fields.input' @:: Lens' NodeProto [Data.Text.Text]@
    * 'Proto.Onnx_Fields.output' @:: Lens' NodeProto [Data.Text.Text]@
    * 'Proto.Onnx_Fields.name' @:: Lens' NodeProto Data.Text.Text@
    * 'Proto.Onnx_Fields.opType' @:: Lens' NodeProto Data.Text.Text@
    * 'Proto.Onnx_Fields.domain' @:: Lens' NodeProto Data.Text.Text@
    * 'Proto.Onnx_Fields.attribute' @:: Lens' NodeProto [AttributeProto]@
    * 'Proto.Onnx_Fields.docString' @:: Lens' NodeProto Data.Text.Text@
 -}
data NodeProto = NodeProto{_NodeProto'input :: ![Data.Text.Text],
                           _NodeProto'output :: ![Data.Text.Text],
                           _NodeProto'name :: !Data.Text.Text,
                           _NodeProto'opType :: !Data.Text.Text,
                           _NodeProto'domain :: !Data.Text.Text,
                           _NodeProto'attribute :: ![AttributeProto],
                           _NodeProto'docString :: !Data.Text.Text,
                           _NodeProto'_unknownFields :: !Data.ProtoLens.FieldSet}
               deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f NodeProto x a, a ~ b) =>
         Lens.Labels.HasLens f NodeProto NodeProto x a b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f NodeProto "input" ([Data.Text.Text])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _NodeProto'input
                 (\ x__ y__ -> x__{_NodeProto'input = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f NodeProto "output" ([Data.Text.Text])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _NodeProto'output
                 (\ x__ y__ -> x__{_NodeProto'output = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f NodeProto "name" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _NodeProto'name
                 (\ x__ y__ -> x__{_NodeProto'name = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f NodeProto "opType" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _NodeProto'opType
                 (\ x__ y__ -> x__{_NodeProto'opType = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f NodeProto "domain" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _NodeProto'domain
                 (\ x__ y__ -> x__{_NodeProto'domain = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f NodeProto "attribute" ([AttributeProto])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _NodeProto'attribute
                 (\ x__ y__ -> x__{_NodeProto'attribute = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f NodeProto "docString" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _NodeProto'docString
                 (\ x__ y__ -> x__{_NodeProto'docString = y__}))
              Prelude.id
instance Data.Default.Class.Default NodeProto where
        def
          = NodeProto{_NodeProto'input = [], _NodeProto'output = [],
                      _NodeProto'name = Data.ProtoLens.fieldDefault,
                      _NodeProto'opType = Data.ProtoLens.fieldDefault,
                      _NodeProto'domain = Data.ProtoLens.fieldDefault,
                      _NodeProto'attribute = [],
                      _NodeProto'docString = Data.ProtoLens.fieldDefault,
                      _NodeProto'_unknownFields = ([])}
instance Data.ProtoLens.Message NodeProto where
        messageName _ = Data.Text.pack "onnx.NodeProto"
        fieldsByTag
          = let input__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "input"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "input")))
                      :: Data.ProtoLens.FieldDescriptor NodeProto
                output__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "output"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "output")))
                      :: Data.ProtoLens.FieldDescriptor NodeProto
                name__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "name"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "name")))
                      :: Data.ProtoLens.FieldDescriptor NodeProto
                opType__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "op_type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "opType")))
                      :: Data.ProtoLens.FieldDescriptor NodeProto
                domain__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "domain"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "domain")))
                      :: Data.ProtoLens.FieldDescriptor NodeProto
                attribute__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "attribute"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor AttributeProto)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "attribute")))
                      :: Data.ProtoLens.FieldDescriptor NodeProto
                docString__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "doc_string"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "docString")))
                      :: Data.ProtoLens.FieldDescriptor NodeProto
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, input__field_descriptor),
                 (Data.ProtoLens.Tag 2, output__field_descriptor),
                 (Data.ProtoLens.Tag 3, name__field_descriptor),
                 (Data.ProtoLens.Tag 4, opType__field_descriptor),
                 (Data.ProtoLens.Tag 7, domain__field_descriptor),
                 (Data.ProtoLens.Tag 5, attribute__field_descriptor),
                 (Data.ProtoLens.Tag 6, docString__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _NodeProto'_unknownFields
              (\ x__ y__ -> x__{_NodeProto'_unknownFields = y__})
{- | Fields :

    * 'Proto.Onnx_Fields.domain' @:: Lens' OperatorSetIdProto Data.Text.Text@
    * 'Proto.Onnx_Fields.version' @:: Lens' OperatorSetIdProto Data.Int.Int64@
 -}
data OperatorSetIdProto = OperatorSetIdProto{_OperatorSetIdProto'domain
                                             :: !Data.Text.Text,
                                             _OperatorSetIdProto'version :: !Data.Int.Int64,
                                             _OperatorSetIdProto'_unknownFields ::
                                             !Data.ProtoLens.FieldSet}
                        deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f OperatorSetIdProto x a, a ~ b) =>
         Lens.Labels.HasLens f OperatorSetIdProto OperatorSetIdProto x a b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f OperatorSetIdProto "domain" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _OperatorSetIdProto'domain
                 (\ x__ y__ -> x__{_OperatorSetIdProto'domain = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f OperatorSetIdProto "version"
           (Data.Int.Int64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _OperatorSetIdProto'version
                 (\ x__ y__ -> x__{_OperatorSetIdProto'version = y__}))
              Prelude.id
instance Data.Default.Class.Default OperatorSetIdProto where
        def
          = OperatorSetIdProto{_OperatorSetIdProto'domain =
                                 Data.ProtoLens.fieldDefault,
                               _OperatorSetIdProto'version = Data.ProtoLens.fieldDefault,
                               _OperatorSetIdProto'_unknownFields = ([])}
instance Data.ProtoLens.Message OperatorSetIdProto where
        messageName _ = Data.Text.pack "onnx.OperatorSetIdProto"
        fieldsByTag
          = let domain__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "domain"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "domain")))
                      :: Data.ProtoLens.FieldDescriptor OperatorSetIdProto
                version__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "version"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "version")))
                      :: Data.ProtoLens.FieldDescriptor OperatorSetIdProto
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, domain__field_descriptor),
                 (Data.ProtoLens.Tag 2, version__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _OperatorSetIdProto'_unknownFields
              (\ x__ y__ -> x__{_OperatorSetIdProto'_unknownFields = y__})
{- | Fields :

    * 'Proto.Onnx_Fields.key' @:: Lens' StringStringEntryProto Data.Text.Text@
    * 'Proto.Onnx_Fields.value' @:: Lens' StringStringEntryProto Data.Text.Text@
 -}
data StringStringEntryProto = StringStringEntryProto{_StringStringEntryProto'key
                                                     :: !Data.Text.Text,
                                                     _StringStringEntryProto'value ::
                                                     !Data.Text.Text,
                                                     _StringStringEntryProto'_unknownFields ::
                                                     !Data.ProtoLens.FieldSet}
                            deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f StringStringEntryProto x a,
          a ~ b) =>
         Lens.Labels.HasLens f StringStringEntryProto StringStringEntryProto
           x
           a
           b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f StringStringEntryProto "key"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StringStringEntryProto'key
                 (\ x__ y__ -> x__{_StringStringEntryProto'key = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f StringStringEntryProto "value"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StringStringEntryProto'value
                 (\ x__ y__ -> x__{_StringStringEntryProto'value = y__}))
              Prelude.id
instance Data.Default.Class.Default StringStringEntryProto where
        def
          = StringStringEntryProto{_StringStringEntryProto'key =
                                     Data.ProtoLens.fieldDefault,
                                   _StringStringEntryProto'value = Data.ProtoLens.fieldDefault,
                                   _StringStringEntryProto'_unknownFields = ([])}
instance Data.ProtoLens.Message StringStringEntryProto where
        messageName _ = Data.Text.pack "onnx.StringStringEntryProto"
        fieldsByTag
          = let key__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "key"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "key")))
                      :: Data.ProtoLens.FieldDescriptor StringStringEntryProto
                value__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "value"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "value")))
                      :: Data.ProtoLens.FieldDescriptor StringStringEntryProto
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, key__field_descriptor),
                 (Data.ProtoLens.Tag 2, value__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens
              _StringStringEntryProto'_unknownFields
              (\ x__ y__ -> x__{_StringStringEntryProto'_unknownFields = y__})
{- | Fields :

    * 'Proto.Onnx_Fields.dims' @:: Lens' TensorProto [Data.Int.Int64]@
    * 'Proto.Onnx_Fields.dataType' @:: Lens' TensorProto TensorProto'DataType@
    * 'Proto.Onnx_Fields.segment' @:: Lens' TensorProto TensorProto'Segment@
    * 'Proto.Onnx_Fields.maybe'segment' @:: Lens' TensorProto (Prelude.Maybe TensorProto'Segment)@
    * 'Proto.Onnx_Fields.floatData' @:: Lens' TensorProto [Prelude.Float]@
    * 'Proto.Onnx_Fields.int32Data' @:: Lens' TensorProto [Data.Int.Int32]@
    * 'Proto.Onnx_Fields.stringData' @:: Lens' TensorProto [Data.ByteString.ByteString]@
    * 'Proto.Onnx_Fields.int64Data' @:: Lens' TensorProto [Data.Int.Int64]@
    * 'Proto.Onnx_Fields.name' @:: Lens' TensorProto Data.Text.Text@
    * 'Proto.Onnx_Fields.docString' @:: Lens' TensorProto Data.Text.Text@
    * 'Proto.Onnx_Fields.rawData' @:: Lens' TensorProto Data.ByteString.ByteString@
    * 'Proto.Onnx_Fields.doubleData' @:: Lens' TensorProto [Prelude.Double]@
    * 'Proto.Onnx_Fields.uint64Data' @:: Lens' TensorProto [Data.Word.Word64]@
 -}
data TensorProto = TensorProto{_TensorProto'dims ::
                               ![Data.Int.Int64],
                               _TensorProto'dataType :: !TensorProto'DataType,
                               _TensorProto'segment :: !(Prelude.Maybe TensorProto'Segment),
                               _TensorProto'floatData :: ![Prelude.Float],
                               _TensorProto'int32Data :: ![Data.Int.Int32],
                               _TensorProto'stringData :: ![Data.ByteString.ByteString],
                               _TensorProto'int64Data :: ![Data.Int.Int64],
                               _TensorProto'name :: !Data.Text.Text,
                               _TensorProto'docString :: !Data.Text.Text,
                               _TensorProto'rawData :: !Data.ByteString.ByteString,
                               _TensorProto'doubleData :: ![Prelude.Double],
                               _TensorProto'uint64Data :: ![Data.Word.Word64],
                               _TensorProto'_unknownFields :: !Data.ProtoLens.FieldSet}
                 deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f TensorProto x a, a ~ b) =>
         Lens.Labels.HasLens f TensorProto TensorProto x a b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorProto "dims" ([Data.Int.Int64])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorProto'dims
                 (\ x__ y__ -> x__{_TensorProto'dims = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorProto "dataType"
           (TensorProto'DataType)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorProto'dataType
                 (\ x__ y__ -> x__{_TensorProto'dataType = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorProto "segment" (TensorProto'Segment)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorProto'segment
                 (\ x__ y__ -> x__{_TensorProto'segment = y__}))
              (Data.ProtoLens.maybeLens Data.Default.Class.def)
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorProto "maybe'segment"
           (Prelude.Maybe TensorProto'Segment)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorProto'segment
                 (\ x__ y__ -> x__{_TensorProto'segment = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorProto "floatData" ([Prelude.Float])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorProto'floatData
                 (\ x__ y__ -> x__{_TensorProto'floatData = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorProto "int32Data" ([Data.Int.Int32])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorProto'int32Data
                 (\ x__ y__ -> x__{_TensorProto'int32Data = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorProto "stringData"
           ([Data.ByteString.ByteString])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorProto'stringData
                 (\ x__ y__ -> x__{_TensorProto'stringData = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorProto "int64Data" ([Data.Int.Int64])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorProto'int64Data
                 (\ x__ y__ -> x__{_TensorProto'int64Data = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorProto "name" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorProto'name
                 (\ x__ y__ -> x__{_TensorProto'name = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorProto "docString" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorProto'docString
                 (\ x__ y__ -> x__{_TensorProto'docString = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorProto "rawData"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorProto'rawData
                 (\ x__ y__ -> x__{_TensorProto'rawData = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorProto "doubleData" ([Prelude.Double])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorProto'doubleData
                 (\ x__ y__ -> x__{_TensorProto'doubleData = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorProto "uint64Data"
           ([Data.Word.Word64])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorProto'uint64Data
                 (\ x__ y__ -> x__{_TensorProto'uint64Data = y__}))
              Prelude.id
instance Data.Default.Class.Default TensorProto where
        def
          = TensorProto{_TensorProto'dims = [],
                        _TensorProto'dataType = Data.Default.Class.def,
                        _TensorProto'segment = Prelude.Nothing,
                        _TensorProto'floatData = [], _TensorProto'int32Data = [],
                        _TensorProto'stringData = [], _TensorProto'int64Data = [],
                        _TensorProto'name = Data.ProtoLens.fieldDefault,
                        _TensorProto'docString = Data.ProtoLens.fieldDefault,
                        _TensorProto'rawData = Data.ProtoLens.fieldDefault,
                        _TensorProto'doubleData = [], _TensorProto'uint64Data = [],
                        _TensorProto'_unknownFields = ([])}
instance Data.ProtoLens.Message TensorProto where
        messageName _ = Data.Text.pack "onnx.TensorProto"
        fieldsByTag
          = let dims__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "dims"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Packed
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "dims")))
                      :: Data.ProtoLens.FieldDescriptor TensorProto
                dataType__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "data_type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                         Data.ProtoLens.FieldTypeDescriptor TensorProto'DataType)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "dataType")))
                      :: Data.ProtoLens.FieldDescriptor TensorProto
                segment__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "segment"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor TensorProto'Segment)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'segment")))
                      :: Data.ProtoLens.FieldDescriptor TensorProto
                floatData__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "float_data"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.FloatField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Float)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Packed
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "floatData")))
                      :: Data.ProtoLens.FieldDescriptor TensorProto
                int32Data__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "int32_data"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Packed
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "int32Data")))
                      :: Data.ProtoLens.FieldDescriptor TensorProto
                stringData__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "string_data"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "stringData")))
                      :: Data.ProtoLens.FieldDescriptor TensorProto
                int64Data__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "int64_data"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Packed
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "int64Data")))
                      :: Data.ProtoLens.FieldDescriptor TensorProto
                name__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "name"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "name")))
                      :: Data.ProtoLens.FieldDescriptor TensorProto
                docString__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "doc_string"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "docString")))
                      :: Data.ProtoLens.FieldDescriptor TensorProto
                rawData__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "raw_data"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "rawData")))
                      :: Data.ProtoLens.FieldDescriptor TensorProto
                doubleData__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "double_data"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Packed
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "doubleData")))
                      :: Data.ProtoLens.FieldDescriptor TensorProto
                uint64Data__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "uint64_data"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Packed
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "uint64Data")))
                      :: Data.ProtoLens.FieldDescriptor TensorProto
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, dims__field_descriptor),
                 (Data.ProtoLens.Tag 2, dataType__field_descriptor),
                 (Data.ProtoLens.Tag 3, segment__field_descriptor),
                 (Data.ProtoLens.Tag 4, floatData__field_descriptor),
                 (Data.ProtoLens.Tag 5, int32Data__field_descriptor),
                 (Data.ProtoLens.Tag 6, stringData__field_descriptor),
                 (Data.ProtoLens.Tag 7, int64Data__field_descriptor),
                 (Data.ProtoLens.Tag 8, name__field_descriptor),
                 (Data.ProtoLens.Tag 12, docString__field_descriptor),
                 (Data.ProtoLens.Tag 9, rawData__field_descriptor),
                 (Data.ProtoLens.Tag 10, doubleData__field_descriptor),
                 (Data.ProtoLens.Tag 11, uint64Data__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _TensorProto'_unknownFields
              (\ x__ y__ -> x__{_TensorProto'_unknownFields = y__})
data TensorProto'DataType = TensorProto'UNDEFINED
                          | TensorProto'FLOAT
                          | TensorProto'UINT8
                          | TensorProto'INT8
                          | TensorProto'UINT16
                          | TensorProto'INT16
                          | TensorProto'INT32
                          | TensorProto'INT64
                          | TensorProto'STRING
                          | TensorProto'BOOL
                          | TensorProto'FLOAT16
                          | TensorProto'DOUBLE
                          | TensorProto'UINT32
                          | TensorProto'UINT64
                          | TensorProto'COMPLEX64
                          | TensorProto'COMPLEX128
                          | TensorProto'DataType'Unrecognized !TensorProto'DataType'UnrecognizedValue
                          deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
newtype TensorProto'DataType'UnrecognizedValue = TensorProto'DataType'UnrecognizedValue Data.Int.Int32
                                               deriving (Prelude.Eq, Prelude.Ord, Prelude.Show)
instance Data.ProtoLens.MessageEnum TensorProto'DataType where
        maybeToEnum 0 = Prelude.Just TensorProto'UNDEFINED
        maybeToEnum 1 = Prelude.Just TensorProto'FLOAT
        maybeToEnum 2 = Prelude.Just TensorProto'UINT8
        maybeToEnum 3 = Prelude.Just TensorProto'INT8
        maybeToEnum 4 = Prelude.Just TensorProto'UINT16
        maybeToEnum 5 = Prelude.Just TensorProto'INT16
        maybeToEnum 6 = Prelude.Just TensorProto'INT32
        maybeToEnum 7 = Prelude.Just TensorProto'INT64
        maybeToEnum 8 = Prelude.Just TensorProto'STRING
        maybeToEnum 9 = Prelude.Just TensorProto'BOOL
        maybeToEnum 10 = Prelude.Just TensorProto'FLOAT16
        maybeToEnum 11 = Prelude.Just TensorProto'DOUBLE
        maybeToEnum 12 = Prelude.Just TensorProto'UINT32
        maybeToEnum 13 = Prelude.Just TensorProto'UINT64
        maybeToEnum 14 = Prelude.Just TensorProto'COMPLEX64
        maybeToEnum 15 = Prelude.Just TensorProto'COMPLEX128
        maybeToEnum k
          = Prelude.Just
              (TensorProto'DataType'Unrecognized
                 (TensorProto'DataType'UnrecognizedValue (Prelude.fromIntegral k)))
        showEnum TensorProto'UNDEFINED = "UNDEFINED"
        showEnum TensorProto'FLOAT = "FLOAT"
        showEnum TensorProto'UINT8 = "UINT8"
        showEnum TensorProto'INT8 = "INT8"
        showEnum TensorProto'UINT16 = "UINT16"
        showEnum TensorProto'INT16 = "INT16"
        showEnum TensorProto'INT32 = "INT32"
        showEnum TensorProto'INT64 = "INT64"
        showEnum TensorProto'STRING = "STRING"
        showEnum TensorProto'BOOL = "BOOL"
        showEnum TensorProto'FLOAT16 = "FLOAT16"
        showEnum TensorProto'DOUBLE = "DOUBLE"
        showEnum TensorProto'UINT32 = "UINT32"
        showEnum TensorProto'UINT64 = "UINT64"
        showEnum TensorProto'COMPLEX64 = "COMPLEX64"
        showEnum TensorProto'COMPLEX128 = "COMPLEX128"
        showEnum
          (TensorProto'DataType'Unrecognized
             (TensorProto'DataType'UnrecognizedValue k))
          = Prelude.show k
        readEnum "UNDEFINED" = Prelude.Just TensorProto'UNDEFINED
        readEnum "FLOAT" = Prelude.Just TensorProto'FLOAT
        readEnum "UINT8" = Prelude.Just TensorProto'UINT8
        readEnum "INT8" = Prelude.Just TensorProto'INT8
        readEnum "UINT16" = Prelude.Just TensorProto'UINT16
        readEnum "INT16" = Prelude.Just TensorProto'INT16
        readEnum "INT32" = Prelude.Just TensorProto'INT32
        readEnum "INT64" = Prelude.Just TensorProto'INT64
        readEnum "STRING" = Prelude.Just TensorProto'STRING
        readEnum "BOOL" = Prelude.Just TensorProto'BOOL
        readEnum "FLOAT16" = Prelude.Just TensorProto'FLOAT16
        readEnum "DOUBLE" = Prelude.Just TensorProto'DOUBLE
        readEnum "UINT32" = Prelude.Just TensorProto'UINT32
        readEnum "UINT64" = Prelude.Just TensorProto'UINT64
        readEnum "COMPLEX64" = Prelude.Just TensorProto'COMPLEX64
        readEnum "COMPLEX128" = Prelude.Just TensorProto'COMPLEX128
        readEnum k
          = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded TensorProto'DataType where
        minBound = TensorProto'UNDEFINED
        maxBound = TensorProto'COMPLEX128
instance Prelude.Enum TensorProto'DataType where
        toEnum k__
          = Prelude.maybe
              (Prelude.error
                 ((Prelude.++) "toEnum: unknown value for enum DataType: "
                    (Prelude.show k__)))
              Prelude.id
              (Data.ProtoLens.maybeToEnum k__)
        fromEnum TensorProto'UNDEFINED = 0
        fromEnum TensorProto'FLOAT = 1
        fromEnum TensorProto'UINT8 = 2
        fromEnum TensorProto'INT8 = 3
        fromEnum TensorProto'UINT16 = 4
        fromEnum TensorProto'INT16 = 5
        fromEnum TensorProto'INT32 = 6
        fromEnum TensorProto'INT64 = 7
        fromEnum TensorProto'STRING = 8
        fromEnum TensorProto'BOOL = 9
        fromEnum TensorProto'FLOAT16 = 10
        fromEnum TensorProto'DOUBLE = 11
        fromEnum TensorProto'UINT32 = 12
        fromEnum TensorProto'UINT64 = 13
        fromEnum TensorProto'COMPLEX64 = 14
        fromEnum TensorProto'COMPLEX128 = 15
        fromEnum
          (TensorProto'DataType'Unrecognized
             (TensorProto'DataType'UnrecognizedValue k))
          = Prelude.fromIntegral k
        succ TensorProto'COMPLEX128
          = Prelude.error
              "TensorProto'DataType.succ: bad argument TensorProto'COMPLEX128. This value would be out of bounds."
        succ TensorProto'UNDEFINED = TensorProto'FLOAT
        succ TensorProto'FLOAT = TensorProto'UINT8
        succ TensorProto'UINT8 = TensorProto'INT8
        succ TensorProto'INT8 = TensorProto'UINT16
        succ TensorProto'UINT16 = TensorProto'INT16
        succ TensorProto'INT16 = TensorProto'INT32
        succ TensorProto'INT32 = TensorProto'INT64
        succ TensorProto'INT64 = TensorProto'STRING
        succ TensorProto'STRING = TensorProto'BOOL
        succ TensorProto'BOOL = TensorProto'FLOAT16
        succ TensorProto'FLOAT16 = TensorProto'DOUBLE
        succ TensorProto'DOUBLE = TensorProto'UINT32
        succ TensorProto'UINT32 = TensorProto'UINT64
        succ TensorProto'UINT64 = TensorProto'COMPLEX64
        succ TensorProto'COMPLEX64 = TensorProto'COMPLEX128
        succ _
          = Prelude.error
              "TensorProto'DataType.succ: bad argument: unrecognized value"
        pred TensorProto'UNDEFINED
          = Prelude.error
              "TensorProto'DataType.pred: bad argument TensorProto'UNDEFINED. This value would be out of bounds."
        pred TensorProto'FLOAT = TensorProto'UNDEFINED
        pred TensorProto'UINT8 = TensorProto'FLOAT
        pred TensorProto'INT8 = TensorProto'UINT8
        pred TensorProto'UINT16 = TensorProto'INT8
        pred TensorProto'INT16 = TensorProto'UINT16
        pred TensorProto'INT32 = TensorProto'INT16
        pred TensorProto'INT64 = TensorProto'INT32
        pred TensorProto'STRING = TensorProto'INT64
        pred TensorProto'BOOL = TensorProto'STRING
        pred TensorProto'FLOAT16 = TensorProto'BOOL
        pred TensorProto'DOUBLE = TensorProto'FLOAT16
        pred TensorProto'UINT32 = TensorProto'DOUBLE
        pred TensorProto'UINT64 = TensorProto'UINT32
        pred TensorProto'COMPLEX64 = TensorProto'UINT64
        pred TensorProto'COMPLEX128 = TensorProto'COMPLEX64
        pred _
          = Prelude.error
              "TensorProto'DataType.pred: bad argument: unrecognized value"
        enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
        enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
        enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
        enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.Default.Class.Default TensorProto'DataType where
        def = TensorProto'UNDEFINED
instance Data.ProtoLens.FieldDefault TensorProto'DataType where
        fieldDefault = TensorProto'UNDEFINED
{- | Fields :

    * 'Proto.Onnx_Fields.begin' @:: Lens' TensorProto'Segment Data.Int.Int64@
    * 'Proto.Onnx_Fields.end' @:: Lens' TensorProto'Segment Data.Int.Int64@
 -}
data TensorProto'Segment = TensorProto'Segment{_TensorProto'Segment'begin
                                               :: !Data.Int.Int64,
                                               _TensorProto'Segment'end :: !Data.Int.Int64,
                                               _TensorProto'Segment'_unknownFields ::
                                               !Data.ProtoLens.FieldSet}
                         deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f TensorProto'Segment x a, a ~ b) =>
         Lens.Labels.HasLens f TensorProto'Segment TensorProto'Segment x a b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorProto'Segment "begin" (Data.Int.Int64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorProto'Segment'begin
                 (\ x__ y__ -> x__{_TensorProto'Segment'begin = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorProto'Segment "end" (Data.Int.Int64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorProto'Segment'end
                 (\ x__ y__ -> x__{_TensorProto'Segment'end = y__}))
              Prelude.id
instance Data.Default.Class.Default TensorProto'Segment where
        def
          = TensorProto'Segment{_TensorProto'Segment'begin =
                                  Data.ProtoLens.fieldDefault,
                                _TensorProto'Segment'end = Data.ProtoLens.fieldDefault,
                                _TensorProto'Segment'_unknownFields = ([])}
instance Data.ProtoLens.Message TensorProto'Segment where
        messageName _ = Data.Text.pack "onnx.TensorProto.Segment"
        fieldsByTag
          = let begin__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "begin"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "begin")))
                      :: Data.ProtoLens.FieldDescriptor TensorProto'Segment
                end__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "end"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "end")))
                      :: Data.ProtoLens.FieldDescriptor TensorProto'Segment
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, begin__field_descriptor),
                 (Data.ProtoLens.Tag 2, end__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _TensorProto'Segment'_unknownFields
              (\ x__ y__ -> x__{_TensorProto'Segment'_unknownFields = y__})
{- | Fields :

    * 'Proto.Onnx_Fields.dim' @:: Lens' TensorShapeProto [TensorShapeProto'Dimension]@
 -}
data TensorShapeProto = TensorShapeProto{_TensorShapeProto'dim ::
                                         ![TensorShapeProto'Dimension],
                                         _TensorShapeProto'_unknownFields ::
                                         !Data.ProtoLens.FieldSet}
                      deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f TensorShapeProto x a, a ~ b) =>
         Lens.Labels.HasLens f TensorShapeProto TensorShapeProto x a b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorShapeProto "dim"
           ([TensorShapeProto'Dimension])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorShapeProto'dim
                 (\ x__ y__ -> x__{_TensorShapeProto'dim = y__}))
              Prelude.id
instance Data.Default.Class.Default TensorShapeProto where
        def
          = TensorShapeProto{_TensorShapeProto'dim = [],
                             _TensorShapeProto'_unknownFields = ([])}
instance Data.ProtoLens.Message TensorShapeProto where
        messageName _ = Data.Text.pack "onnx.TensorShapeProto"
        fieldsByTag
          = let dim__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "dim"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor TensorShapeProto'Dimension)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "dim")))
                      :: Data.ProtoLens.FieldDescriptor TensorShapeProto
              in
              Data.Map.fromList [(Data.ProtoLens.Tag 1, dim__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _TensorShapeProto'_unknownFields
              (\ x__ y__ -> x__{_TensorShapeProto'_unknownFields = y__})
{- | Fields :

    * 'Proto.Onnx_Fields.maybe'value' @:: Lens' TensorShapeProto'Dimension
  (Prelude.Maybe TensorShapeProto'Dimension'Value)@
    * 'Proto.Onnx_Fields.maybe'dimValue' @:: Lens' TensorShapeProto'Dimension (Prelude.Maybe Data.Int.Int64)@
    * 'Proto.Onnx_Fields.dimValue' @:: Lens' TensorShapeProto'Dimension Data.Int.Int64@
    * 'Proto.Onnx_Fields.maybe'dimParam' @:: Lens' TensorShapeProto'Dimension (Prelude.Maybe Data.Text.Text)@
    * 'Proto.Onnx_Fields.dimParam' @:: Lens' TensorShapeProto'Dimension Data.Text.Text@
 -}
data TensorShapeProto'Dimension = TensorShapeProto'Dimension{_TensorShapeProto'Dimension'value
                                                             ::
                                                             !(Prelude.Maybe
                                                                 TensorShapeProto'Dimension'Value),
                                                             _TensorShapeProto'Dimension'_unknownFields
                                                             :: !Data.ProtoLens.FieldSet}
                                deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
data TensorShapeProto'Dimension'Value = TensorShapeProto'Dimension'DimValue !Data.Int.Int64
                                      | TensorShapeProto'Dimension'DimParam !Data.Text.Text
                                      deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f TensorShapeProto'Dimension x a,
          a ~ b) =>
         Lens.Labels.HasLens f TensorShapeProto'Dimension
           TensorShapeProto'Dimension
           x
           a
           b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorShapeProto'Dimension "maybe'value"
           (Prelude.Maybe TensorShapeProto'Dimension'Value)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorShapeProto'Dimension'value
                 (\ x__ y__ -> x__{_TensorShapeProto'Dimension'value = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorShapeProto'Dimension "maybe'dimValue"
           (Prelude.Maybe Data.Int.Int64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorShapeProto'Dimension'value
                 (\ x__ y__ -> x__{_TensorShapeProto'Dimension'value = y__}))
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just
                          (TensorShapeProto'Dimension'DimValue x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap TensorShapeProto'Dimension'DimValue y__))
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorShapeProto'Dimension "dimValue"
           (Data.Int.Int64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorShapeProto'Dimension'value
                 (\ x__ y__ -> x__{_TensorShapeProto'Dimension'value = y__}))
              ((Prelude..)
                 (Lens.Family2.Unchecked.lens
                    (\ x__ ->
                       case x__ of
                           Prelude.Just
                             (TensorShapeProto'Dimension'DimValue x__val) -> Prelude.Just x__val
                           _otherwise -> Prelude.Nothing)
                    (\ _ y__ -> Prelude.fmap TensorShapeProto'Dimension'DimValue y__))
                 (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorShapeProto'Dimension "maybe'dimParam"
           (Prelude.Maybe Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorShapeProto'Dimension'value
                 (\ x__ y__ -> x__{_TensorShapeProto'Dimension'value = y__}))
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just
                          (TensorShapeProto'Dimension'DimParam x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap TensorShapeProto'Dimension'DimParam y__))
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TensorShapeProto'Dimension "dimParam"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TensorShapeProto'Dimension'value
                 (\ x__ y__ -> x__{_TensorShapeProto'Dimension'value = y__}))
              ((Prelude..)
                 (Lens.Family2.Unchecked.lens
                    (\ x__ ->
                       case x__ of
                           Prelude.Just
                             (TensorShapeProto'Dimension'DimParam x__val) -> Prelude.Just x__val
                           _otherwise -> Prelude.Nothing)
                    (\ _ y__ -> Prelude.fmap TensorShapeProto'Dimension'DimParam y__))
                 (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.Default.Class.Default TensorShapeProto'Dimension
         where
        def
          = TensorShapeProto'Dimension{_TensorShapeProto'Dimension'value =
                                         Prelude.Nothing,
                                       _TensorShapeProto'Dimension'_unknownFields = ([])}
instance Data.ProtoLens.Message TensorShapeProto'Dimension where
        messageName _ = Data.Text.pack "onnx.TensorShapeProto.Dimension"
        fieldsByTag
          = let dimValue__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "dim_value"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'dimValue")))
                      :: Data.ProtoLens.FieldDescriptor TensorShapeProto'Dimension
                dimParam__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "dim_param"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'dimParam")))
                      :: Data.ProtoLens.FieldDescriptor TensorShapeProto'Dimension
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, dimValue__field_descriptor),
                 (Data.ProtoLens.Tag 2, dimParam__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens
              _TensorShapeProto'Dimension'_unknownFields
              (\ x__ y__ ->
                 x__{_TensorShapeProto'Dimension'_unknownFields = y__})
_TensorShapeProto'Dimension'DimValue ::
                                     Lens.Labels.Prism.Prism' TensorShapeProto'Dimension'Value
                                       Data.Int.Int64
_TensorShapeProto'Dimension'DimValue
  = Lens.Labels.Prism.prism' TensorShapeProto'Dimension'DimValue
      (\ p__ ->
         case p__ of
             TensorShapeProto'Dimension'DimValue p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_TensorShapeProto'Dimension'DimParam ::
                                     Lens.Labels.Prism.Prism' TensorShapeProto'Dimension'Value
                                       Data.Text.Text
_TensorShapeProto'Dimension'DimParam
  = Lens.Labels.Prism.prism' TensorShapeProto'Dimension'DimParam
      (\ p__ ->
         case p__ of
             TensorShapeProto'Dimension'DimParam p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
{- | Fields :

    * 'Proto.Onnx_Fields.maybe'value' @:: Lens' TypeProto (Prelude.Maybe TypeProto'Value)@
    * 'Proto.Onnx_Fields.maybe'tensorType' @:: Lens' TypeProto (Prelude.Maybe TypeProto'Tensor)@
    * 'Proto.Onnx_Fields.tensorType' @:: Lens' TypeProto TypeProto'Tensor@
 -}
data TypeProto = TypeProto{_TypeProto'value ::
                           !(Prelude.Maybe TypeProto'Value),
                           _TypeProto'_unknownFields :: !Data.ProtoLens.FieldSet}
               deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
data TypeProto'Value = TypeProto'TensorType !TypeProto'Tensor
                     deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f TypeProto x a, a ~ b) =>
         Lens.Labels.HasLens f TypeProto TypeProto x a b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TypeProto "maybe'value"
           (Prelude.Maybe TypeProto'Value)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TypeProto'value
                 (\ x__ y__ -> x__{_TypeProto'value = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TypeProto "maybe'tensorType"
           (Prelude.Maybe TypeProto'Tensor)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TypeProto'value
                 (\ x__ y__ -> x__{_TypeProto'value = y__}))
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (TypeProto'TensorType x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap TypeProto'TensorType y__))
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TypeProto "tensorType" (TypeProto'Tensor)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TypeProto'value
                 (\ x__ y__ -> x__{_TypeProto'value = y__}))
              ((Prelude..)
                 (Lens.Family2.Unchecked.lens
                    (\ x__ ->
                       case x__ of
                           Prelude.Just (TypeProto'TensorType x__val) -> Prelude.Just x__val
                           _otherwise -> Prelude.Nothing)
                    (\ _ y__ -> Prelude.fmap TypeProto'TensorType y__))
                 (Data.ProtoLens.maybeLens Data.Default.Class.def))
instance Data.Default.Class.Default TypeProto where
        def
          = TypeProto{_TypeProto'value = Prelude.Nothing,
                      _TypeProto'_unknownFields = ([])}
instance Data.ProtoLens.Message TypeProto where
        messageName _ = Data.Text.pack "onnx.TypeProto"
        fieldsByTag
          = let tensorType__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "tensor_type"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor TypeProto'Tensor)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'tensorType")))
                      :: Data.ProtoLens.FieldDescriptor TypeProto
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, tensorType__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _TypeProto'_unknownFields
              (\ x__ y__ -> x__{_TypeProto'_unknownFields = y__})
_TypeProto'TensorType ::
                      Lens.Labels.Prism.Prism' TypeProto'Value TypeProto'Tensor
_TypeProto'TensorType
  = Lens.Labels.Prism.prism' TypeProto'TensorType
      (\ p__ ->
         case p__ of
             TypeProto'TensorType p__val -> Prelude.Just p__val)
{- | Fields :

    * 'Proto.Onnx_Fields.elemType' @:: Lens' TypeProto'Tensor TensorProto'DataType@
    * 'Proto.Onnx_Fields.shape' @:: Lens' TypeProto'Tensor TensorShapeProto@
    * 'Proto.Onnx_Fields.maybe'shape' @:: Lens' TypeProto'Tensor (Prelude.Maybe TensorShapeProto)@
 -}
data TypeProto'Tensor = TypeProto'Tensor{_TypeProto'Tensor'elemType
                                         :: !TensorProto'DataType,
                                         _TypeProto'Tensor'shape ::
                                         !(Prelude.Maybe TensorShapeProto),
                                         _TypeProto'Tensor'_unknownFields ::
                                         !Data.ProtoLens.FieldSet}
                      deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f TypeProto'Tensor x a, a ~ b) =>
         Lens.Labels.HasLens f TypeProto'Tensor TypeProto'Tensor x a b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TypeProto'Tensor "elemType"
           (TensorProto'DataType)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TypeProto'Tensor'elemType
                 (\ x__ y__ -> x__{_TypeProto'Tensor'elemType = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TypeProto'Tensor "shape" (TensorShapeProto)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TypeProto'Tensor'shape
                 (\ x__ y__ -> x__{_TypeProto'Tensor'shape = y__}))
              (Data.ProtoLens.maybeLens Data.Default.Class.def)
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f TypeProto'Tensor "maybe'shape"
           (Prelude.Maybe TensorShapeProto)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TypeProto'Tensor'shape
                 (\ x__ y__ -> x__{_TypeProto'Tensor'shape = y__}))
              Prelude.id
instance Data.Default.Class.Default TypeProto'Tensor where
        def
          = TypeProto'Tensor{_TypeProto'Tensor'elemType =
                               Data.Default.Class.def,
                             _TypeProto'Tensor'shape = Prelude.Nothing,
                             _TypeProto'Tensor'_unknownFields = ([])}
instance Data.ProtoLens.Message TypeProto'Tensor where
        messageName _ = Data.Text.pack "onnx.TypeProto.Tensor"
        fieldsByTag
          = let elemType__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "elem_type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                         Data.ProtoLens.FieldTypeDescriptor TensorProto'DataType)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "elemType")))
                      :: Data.ProtoLens.FieldDescriptor TypeProto'Tensor
                shape__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "shape"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor TensorShapeProto)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'shape")))
                      :: Data.ProtoLens.FieldDescriptor TypeProto'Tensor
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, elemType__field_descriptor),
                 (Data.ProtoLens.Tag 2, shape__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _TypeProto'Tensor'_unknownFields
              (\ x__ y__ -> x__{_TypeProto'Tensor'_unknownFields = y__})
{- | Fields :

    * 'Proto.Onnx_Fields.name' @:: Lens' ValueInfoProto Data.Text.Text@
    * 'Proto.Onnx_Fields.type'' @:: Lens' ValueInfoProto TypeProto@
    * 'Proto.Onnx_Fields.maybe'type'' @:: Lens' ValueInfoProto (Prelude.Maybe TypeProto)@
    * 'Proto.Onnx_Fields.docString' @:: Lens' ValueInfoProto Data.Text.Text@
 -}
data ValueInfoProto = ValueInfoProto{_ValueInfoProto'name ::
                                     !Data.Text.Text,
                                     _ValueInfoProto'type' :: !(Prelude.Maybe TypeProto),
                                     _ValueInfoProto'docString :: !Data.Text.Text,
                                     _ValueInfoProto'_unknownFields :: !Data.ProtoLens.FieldSet}
                    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f ValueInfoProto x a, a ~ b) =>
         Lens.Labels.HasLens f ValueInfoProto ValueInfoProto x a b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f ValueInfoProto "name" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ValueInfoProto'name
                 (\ x__ y__ -> x__{_ValueInfoProto'name = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f ValueInfoProto "type'" (TypeProto)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ValueInfoProto'type'
                 (\ x__ y__ -> x__{_ValueInfoProto'type' = y__}))
              (Data.ProtoLens.maybeLens Data.Default.Class.def)
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f ValueInfoProto "maybe'type'"
           (Prelude.Maybe TypeProto)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ValueInfoProto'type'
                 (\ x__ y__ -> x__{_ValueInfoProto'type' = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f ValueInfoProto "docString" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ValueInfoProto'docString
                 (\ x__ y__ -> x__{_ValueInfoProto'docString = y__}))
              Prelude.id
instance Data.Default.Class.Default ValueInfoProto where
        def
          = ValueInfoProto{_ValueInfoProto'name =
                             Data.ProtoLens.fieldDefault,
                           _ValueInfoProto'type' = Prelude.Nothing,
                           _ValueInfoProto'docString = Data.ProtoLens.fieldDefault,
                           _ValueInfoProto'_unknownFields = ([])}
instance Data.ProtoLens.Message ValueInfoProto where
        messageName _ = Data.Text.pack "onnx.ValueInfoProto"
        fieldsByTag
          = let name__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "name"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "name")))
                      :: Data.ProtoLens.FieldDescriptor ValueInfoProto
                type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor TypeProto)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'type'")))
                      :: Data.ProtoLens.FieldDescriptor ValueInfoProto
                docString__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "doc_string"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "docString")))
                      :: Data.ProtoLens.FieldDescriptor ValueInfoProto
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, name__field_descriptor),
                 (Data.ProtoLens.Tag 2, type'__field_descriptor),
                 (Data.ProtoLens.Tag 3, docString__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _ValueInfoProto'_unknownFields
              (\ x__ y__ -> x__{_ValueInfoProto'_unknownFields = y__})
data Version = START_VERSION
             | IR_VERSION_2017_10_10
             | IR_VERSION_2017_10_30
             | IR_VERSION
             | Version'Unrecognized !Version'UnrecognizedValue
             deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
newtype Version'UnrecognizedValue = Version'UnrecognizedValue Data.Int.Int32
                                  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show)
instance Data.ProtoLens.MessageEnum Version where
        maybeToEnum 0 = Prelude.Just START_VERSION
        maybeToEnum 1 = Prelude.Just IR_VERSION_2017_10_10
        maybeToEnum 2 = Prelude.Just IR_VERSION_2017_10_30
        maybeToEnum 3 = Prelude.Just IR_VERSION
        maybeToEnum k
          = Prelude.Just
              (Version'Unrecognized
                 (Version'UnrecognizedValue (Prelude.fromIntegral k)))
        showEnum START_VERSION = "START_VERSION"
        showEnum IR_VERSION_2017_10_10 = "IR_VERSION_2017_10_10"
        showEnum IR_VERSION_2017_10_30 = "IR_VERSION_2017_10_30"
        showEnum IR_VERSION = "IR_VERSION"
        showEnum (Version'Unrecognized (Version'UnrecognizedValue k))
          = Prelude.show k
        readEnum "START_VERSION" = Prelude.Just START_VERSION
        readEnum "IR_VERSION_2017_10_10"
          = Prelude.Just IR_VERSION_2017_10_10
        readEnum "IR_VERSION_2017_10_30"
          = Prelude.Just IR_VERSION_2017_10_30
        readEnum "IR_VERSION" = Prelude.Just IR_VERSION
        readEnum k
          = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded Version where
        minBound = START_VERSION
        maxBound = IR_VERSION
instance Prelude.Enum Version where
        toEnum k__
          = Prelude.maybe
              (Prelude.error
                 ((Prelude.++) "toEnum: unknown value for enum Version: "
                    (Prelude.show k__)))
              Prelude.id
              (Data.ProtoLens.maybeToEnum k__)
        fromEnum START_VERSION = 0
        fromEnum IR_VERSION_2017_10_10 = 1
        fromEnum IR_VERSION_2017_10_30 = 2
        fromEnum IR_VERSION = 3
        fromEnum (Version'Unrecognized (Version'UnrecognizedValue k))
          = Prelude.fromIntegral k
        succ IR_VERSION
          = Prelude.error
              "Version.succ: bad argument IR_VERSION. This value would be out of bounds."
        succ START_VERSION = IR_VERSION_2017_10_10
        succ IR_VERSION_2017_10_10 = IR_VERSION_2017_10_30
        succ IR_VERSION_2017_10_30 = IR_VERSION
        succ _
          = Prelude.error "Version.succ: bad argument: unrecognized value"
        pred START_VERSION
          = Prelude.error
              "Version.pred: bad argument START_VERSION. This value would be out of bounds."
        pred IR_VERSION_2017_10_10 = START_VERSION
        pred IR_VERSION_2017_10_30 = IR_VERSION_2017_10_10
        pred IR_VERSION = IR_VERSION_2017_10_30
        pred _
          = Prelude.error "Version.pred: bad argument: unrecognized value"
        enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
        enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
        enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
        enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.Default.Class.Default Version where
        def = START_VERSION
instance Data.ProtoLens.FieldDefault Version where
        fieldDefault = START_VERSION