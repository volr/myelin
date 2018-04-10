{- This file was auto-generated from onnx-operators-ml.proto3 by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.OnnxOperatorsMl
       (OperatorProto(..), OperatorProto'OperatorStatus(..),
        OperatorProto'OperatorStatus(),
        OperatorProto'OperatorStatus'UnrecognizedValue,
        OperatorSetProto(..))
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
import qualified Proto.OnnxMl

{- | Fields :

    * 'Proto.OnnxOperatorsMl_Fields.opType' @:: Lens' OperatorProto Data.Text.Text@
    * 'Proto.OnnxOperatorsMl_Fields.sinceVersion' @:: Lens' OperatorProto Data.Int.Int64@
    * 'Proto.OnnxOperatorsMl_Fields.status' @:: Lens' OperatorProto OperatorProto'OperatorStatus@
    * 'Proto.OnnxOperatorsMl_Fields.docString' @:: Lens' OperatorProto Data.Text.Text@
 -}
data OperatorProto = OperatorProto{_OperatorProto'opType ::
                                   !Data.Text.Text,
                                   _OperatorProto'sinceVersion :: !Data.Int.Int64,
                                   _OperatorProto'status :: !OperatorProto'OperatorStatus,
                                   _OperatorProto'docString :: !Data.Text.Text,
                                   _OperatorProto'_unknownFields :: !Data.ProtoLens.FieldSet}
                   deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f OperatorProto x a, a ~ b) =>
         Lens.Labels.HasLens f OperatorProto OperatorProto x a b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f OperatorProto "opType" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _OperatorProto'opType
                 (\ x__ y__ -> x__{_OperatorProto'opType = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f OperatorProto "sinceVersion"
           (Data.Int.Int64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _OperatorProto'sinceVersion
                 (\ x__ y__ -> x__{_OperatorProto'sinceVersion = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f OperatorProto "status"
           (OperatorProto'OperatorStatus)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _OperatorProto'status
                 (\ x__ y__ -> x__{_OperatorProto'status = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f OperatorProto "docString" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _OperatorProto'docString
                 (\ x__ y__ -> x__{_OperatorProto'docString = y__}))
              Prelude.id
instance Data.Default.Class.Default OperatorProto where
        def
          = OperatorProto{_OperatorProto'opType =
                            Data.ProtoLens.fieldDefault,
                          _OperatorProto'sinceVersion = Data.ProtoLens.fieldDefault,
                          _OperatorProto'status = Data.Default.Class.def,
                          _OperatorProto'docString = Data.ProtoLens.fieldDefault,
                          _OperatorProto'_unknownFields = ([])}
instance Data.ProtoLens.Message OperatorProto where
        messageName _ = Data.Text.pack "onnx.OperatorProto"
        fieldsByTag
          = let opType__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "op_type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "opType")))
                      :: Data.ProtoLens.FieldDescriptor OperatorProto
                sinceVersion__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "since_version"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "sinceVersion")))
                      :: Data.ProtoLens.FieldDescriptor OperatorProto
                status__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "status"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                         Data.ProtoLens.FieldTypeDescriptor OperatorProto'OperatorStatus)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "status")))
                      :: Data.ProtoLens.FieldDescriptor OperatorProto
                docString__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "doc_string"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "docString")))
                      :: Data.ProtoLens.FieldDescriptor OperatorProto
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, opType__field_descriptor),
                 (Data.ProtoLens.Tag 2, sinceVersion__field_descriptor),
                 (Data.ProtoLens.Tag 3, status__field_descriptor),
                 (Data.ProtoLens.Tag 10, docString__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _OperatorProto'_unknownFields
              (\ x__ y__ -> x__{_OperatorProto'_unknownFields = y__})
data OperatorProto'OperatorStatus = OperatorProto'EXPERIMENTAL
                                  | OperatorProto'STABLE
                                  | OperatorProto'OperatorStatus'Unrecognized !OperatorProto'OperatorStatus'UnrecognizedValue
                                  deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
newtype OperatorProto'OperatorStatus'UnrecognizedValue = OperatorProto'OperatorStatus'UnrecognizedValue Data.Int.Int32
                                                       deriving (Prelude.Eq, Prelude.Ord,
                                                                 Prelude.Show)
instance Data.ProtoLens.MessageEnum OperatorProto'OperatorStatus
         where
        maybeToEnum 0 = Prelude.Just OperatorProto'EXPERIMENTAL
        maybeToEnum 1 = Prelude.Just OperatorProto'STABLE
        maybeToEnum k
          = Prelude.Just
              (OperatorProto'OperatorStatus'Unrecognized
                 (OperatorProto'OperatorStatus'UnrecognizedValue
                    (Prelude.fromIntegral k)))
        showEnum OperatorProto'EXPERIMENTAL = "EXPERIMENTAL"
        showEnum OperatorProto'STABLE = "STABLE"
        showEnum
          (OperatorProto'OperatorStatus'Unrecognized
             (OperatorProto'OperatorStatus'UnrecognizedValue k))
          = Prelude.show k
        readEnum "EXPERIMENTAL" = Prelude.Just OperatorProto'EXPERIMENTAL
        readEnum "STABLE" = Prelude.Just OperatorProto'STABLE
        readEnum k
          = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded OperatorProto'OperatorStatus where
        minBound = OperatorProto'EXPERIMENTAL
        maxBound = OperatorProto'STABLE
instance Prelude.Enum OperatorProto'OperatorStatus where
        toEnum k__
          = Prelude.maybe
              (Prelude.error
                 ((Prelude.++) "toEnum: unknown value for enum OperatorStatus: "
                    (Prelude.show k__)))
              Prelude.id
              (Data.ProtoLens.maybeToEnum k__)
        fromEnum OperatorProto'EXPERIMENTAL = 0
        fromEnum OperatorProto'STABLE = 1
        fromEnum
          (OperatorProto'OperatorStatus'Unrecognized
             (OperatorProto'OperatorStatus'UnrecognizedValue k))
          = Prelude.fromIntegral k
        succ OperatorProto'STABLE
          = Prelude.error
              "OperatorProto'OperatorStatus.succ: bad argument OperatorProto'STABLE. This value would be out of bounds."
        succ OperatorProto'EXPERIMENTAL = OperatorProto'STABLE
        succ _
          = Prelude.error
              "OperatorProto'OperatorStatus.succ: bad argument: unrecognized value"
        pred OperatorProto'EXPERIMENTAL
          = Prelude.error
              "OperatorProto'OperatorStatus.pred: bad argument OperatorProto'EXPERIMENTAL. This value would be out of bounds."
        pred OperatorProto'STABLE = OperatorProto'EXPERIMENTAL
        pred _
          = Prelude.error
              "OperatorProto'OperatorStatus.pred: bad argument: unrecognized value"
        enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
        enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
        enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
        enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.Default.Class.Default OperatorProto'OperatorStatus
         where
        def = OperatorProto'EXPERIMENTAL
instance Data.ProtoLens.FieldDefault OperatorProto'OperatorStatus
         where
        fieldDefault = OperatorProto'EXPERIMENTAL
{- | Fields :

    * 'Proto.OnnxOperatorsMl_Fields.magic' @:: Lens' OperatorSetProto Data.Text.Text@
    * 'Proto.OnnxOperatorsMl_Fields.irVersion' @:: Lens' OperatorSetProto Data.Int.Int32@
    * 'Proto.OnnxOperatorsMl_Fields.irVersionPrerelease' @:: Lens' OperatorSetProto Data.Text.Text@
    * 'Proto.OnnxOperatorsMl_Fields.irBuildMetadata' @:: Lens' OperatorSetProto Data.Text.Text@
    * 'Proto.OnnxOperatorsMl_Fields.domain' @:: Lens' OperatorSetProto Data.Text.Text@
    * 'Proto.OnnxOperatorsMl_Fields.opsetVersion' @:: Lens' OperatorSetProto Data.Int.Int64@
    * 'Proto.OnnxOperatorsMl_Fields.docString' @:: Lens' OperatorSetProto Data.Text.Text@
    * 'Proto.OnnxOperatorsMl_Fields.operator' @:: Lens' OperatorSetProto [OperatorProto]@
 -}
data OperatorSetProto = OperatorSetProto{_OperatorSetProto'magic ::
                                         !Data.Text.Text,
                                         _OperatorSetProto'irVersion :: !Data.Int.Int32,
                                         _OperatorSetProto'irVersionPrerelease :: !Data.Text.Text,
                                         _OperatorSetProto'irBuildMetadata :: !Data.Text.Text,
                                         _OperatorSetProto'domain :: !Data.Text.Text,
                                         _OperatorSetProto'opsetVersion :: !Data.Int.Int64,
                                         _OperatorSetProto'docString :: !Data.Text.Text,
                                         _OperatorSetProto'operator :: ![OperatorProto],
                                         _OperatorSetProto'_unknownFields ::
                                         !Data.ProtoLens.FieldSet}
                      deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f OperatorSetProto x a, a ~ b) =>
         Lens.Labels.HasLens f OperatorSetProto OperatorSetProto x a b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f OperatorSetProto "magic" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _OperatorSetProto'magic
                 (\ x__ y__ -> x__{_OperatorSetProto'magic = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f OperatorSetProto "irVersion"
           (Data.Int.Int32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _OperatorSetProto'irVersion
                 (\ x__ y__ -> x__{_OperatorSetProto'irVersion = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f OperatorSetProto "irVersionPrerelease"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _OperatorSetProto'irVersionPrerelease
                 (\ x__ y__ -> x__{_OperatorSetProto'irVersionPrerelease = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f OperatorSetProto "irBuildMetadata"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _OperatorSetProto'irBuildMetadata
                 (\ x__ y__ -> x__{_OperatorSetProto'irBuildMetadata = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f OperatorSetProto "domain" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _OperatorSetProto'domain
                 (\ x__ y__ -> x__{_OperatorSetProto'domain = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f OperatorSetProto "opsetVersion"
           (Data.Int.Int64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _OperatorSetProto'opsetVersion
                 (\ x__ y__ -> x__{_OperatorSetProto'opsetVersion = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f OperatorSetProto "docString"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _OperatorSetProto'docString
                 (\ x__ y__ -> x__{_OperatorSetProto'docString = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f OperatorSetProto "operator"
           ([OperatorProto])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _OperatorSetProto'operator
                 (\ x__ y__ -> x__{_OperatorSetProto'operator = y__}))
              Prelude.id
instance Data.Default.Class.Default OperatorSetProto where
        def
          = OperatorSetProto{_OperatorSetProto'magic =
                               Data.ProtoLens.fieldDefault,
                             _OperatorSetProto'irVersion = Data.ProtoLens.fieldDefault,
                             _OperatorSetProto'irVersionPrerelease =
                               Data.ProtoLens.fieldDefault,
                             _OperatorSetProto'irBuildMetadata = Data.ProtoLens.fieldDefault,
                             _OperatorSetProto'domain = Data.ProtoLens.fieldDefault,
                             _OperatorSetProto'opsetVersion = Data.ProtoLens.fieldDefault,
                             _OperatorSetProto'docString = Data.ProtoLens.fieldDefault,
                             _OperatorSetProto'operator = [],
                             _OperatorSetProto'_unknownFields = ([])}
instance Data.ProtoLens.Message OperatorSetProto where
        messageName _ = Data.Text.pack "onnx.OperatorSetProto"
        fieldsByTag
          = let magic__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "magic"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "magic")))
                      :: Data.ProtoLens.FieldDescriptor OperatorSetProto
                irVersion__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "ir_version"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "irVersion")))
                      :: Data.ProtoLens.FieldDescriptor OperatorSetProto
                irVersionPrerelease__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "ir_version_prerelease"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "irVersionPrerelease")))
                      :: Data.ProtoLens.FieldDescriptor OperatorSetProto
                irBuildMetadata__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "ir_build_metadata"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "irBuildMetadata")))
                      :: Data.ProtoLens.FieldDescriptor OperatorSetProto
                domain__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "domain"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "domain")))
                      :: Data.ProtoLens.FieldDescriptor OperatorSetProto
                opsetVersion__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "opset_version"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "opsetVersion")))
                      :: Data.ProtoLens.FieldDescriptor OperatorSetProto
                docString__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "doc_string"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "docString")))
                      :: Data.ProtoLens.FieldDescriptor OperatorSetProto
                operator__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "operator"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor OperatorProto)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "operator")))
                      :: Data.ProtoLens.FieldDescriptor OperatorSetProto
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, magic__field_descriptor),
                 (Data.ProtoLens.Tag 2, irVersion__field_descriptor),
                 (Data.ProtoLens.Tag 3, irVersionPrerelease__field_descriptor),
                 (Data.ProtoLens.Tag 7, irBuildMetadata__field_descriptor),
                 (Data.ProtoLens.Tag 4, domain__field_descriptor),
                 (Data.ProtoLens.Tag 5, opsetVersion__field_descriptor),
                 (Data.ProtoLens.Tag 6, docString__field_descriptor),
                 (Data.ProtoLens.Tag 8, operator__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _OperatorSetProto'_unknownFields
              (\ x__ y__ -> x__{_OperatorSetProto'_unknownFields = y__})