{- This file was auto-generated from onnx-ml.proto3 by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.OnnxMl_Fields where
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

attribute ::
          forall f s t a b . (Lens.Labels.HasLens f s t "attribute" a b) =>
            Lens.Family2.LensLike f s t a b
attribute
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "attribute")
begin ::
      forall f s t a b . (Lens.Labels.HasLens f s t "begin" a b) =>
        Lens.Family2.LensLike f s t a b
begin
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "begin")
dataType ::
         forall f s t a b . (Lens.Labels.HasLens f s t "dataType" a b) =>
           Lens.Family2.LensLike f s t a b
dataType
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "dataType")
dim ::
    forall f s t a b . (Lens.Labels.HasLens f s t "dim" a b) =>
      Lens.Family2.LensLike f s t a b
dim
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "dim")
dimParam ::
         forall f s t a b . (Lens.Labels.HasLens f s t "dimParam" a b) =>
           Lens.Family2.LensLike f s t a b
dimParam
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "dimParam")
dimValue ::
         forall f s t a b . (Lens.Labels.HasLens f s t "dimValue" a b) =>
           Lens.Family2.LensLike f s t a b
dimValue
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "dimValue")
dims ::
     forall f s t a b . (Lens.Labels.HasLens f s t "dims" a b) =>
       Lens.Family2.LensLike f s t a b
dims
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "dims")
docString ::
          forall f s t a b . (Lens.Labels.HasLens f s t "docString" a b) =>
            Lens.Family2.LensLike f s t a b
docString
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "docString")
domain ::
       forall f s t a b . (Lens.Labels.HasLens f s t "domain" a b) =>
         Lens.Family2.LensLike f s t a b
domain
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "domain")
doubleData ::
           forall f s t a b . (Lens.Labels.HasLens f s t "doubleData" a b) =>
             Lens.Family2.LensLike f s t a b
doubleData
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "doubleData")
elemType ::
         forall f s t a b . (Lens.Labels.HasLens f s t "elemType" a b) =>
           Lens.Family2.LensLike f s t a b
elemType
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "elemType")
end ::
    forall f s t a b . (Lens.Labels.HasLens f s t "end" a b) =>
      Lens.Family2.LensLike f s t a b
end
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "end")
f ::
  forall f s t a b . (Lens.Labels.HasLens f s t "f" a b) =>
    Lens.Family2.LensLike f s t a b
f = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "f")
floatData ::
          forall f s t a b . (Lens.Labels.HasLens f s t "floatData" a b) =>
            Lens.Family2.LensLike f s t a b
floatData
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "floatData")
floats ::
       forall f s t a b . (Lens.Labels.HasLens f s t "floats" a b) =>
         Lens.Family2.LensLike f s t a b
floats
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "floats")
g ::
  forall f s t a b . (Lens.Labels.HasLens f s t "g" a b) =>
    Lens.Family2.LensLike f s t a b
g = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "g")
graph ::
      forall f s t a b . (Lens.Labels.HasLens f s t "graph" a b) =>
        Lens.Family2.LensLike f s t a b
graph
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "graph")
graphs ::
       forall f s t a b . (Lens.Labels.HasLens f s t "graphs" a b) =>
         Lens.Family2.LensLike f s t a b
graphs
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "graphs")
i ::
  forall f s t a b . (Lens.Labels.HasLens f s t "i" a b) =>
    Lens.Family2.LensLike f s t a b
i = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "i")
initializer ::
            forall f s t a b . (Lens.Labels.HasLens f s t "initializer" a b) =>
              Lens.Family2.LensLike f s t a b
initializer
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "initializer")
input ::
      forall f s t a b . (Lens.Labels.HasLens f s t "input" a b) =>
        Lens.Family2.LensLike f s t a b
input
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "input")
int32Data ::
          forall f s t a b . (Lens.Labels.HasLens f s t "int32Data" a b) =>
            Lens.Family2.LensLike f s t a b
int32Data
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "int32Data")
int64Data ::
          forall f s t a b . (Lens.Labels.HasLens f s t "int64Data" a b) =>
            Lens.Family2.LensLike f s t a b
int64Data
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "int64Data")
ints ::
     forall f s t a b . (Lens.Labels.HasLens f s t "ints" a b) =>
       Lens.Family2.LensLike f s t a b
ints
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "ints")
irVersion ::
          forall f s t a b . (Lens.Labels.HasLens f s t "irVersion" a b) =>
            Lens.Family2.LensLike f s t a b
irVersion
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "irVersion")
key ::
    forall f s t a b . (Lens.Labels.HasLens f s t "key" a b) =>
      Lens.Family2.LensLike f s t a b
key
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "key")
keyType ::
        forall f s t a b . (Lens.Labels.HasLens f s t "keyType" a b) =>
          Lens.Family2.LensLike f s t a b
keyType
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "keyType")
mapType ::
        forall f s t a b . (Lens.Labels.HasLens f s t "mapType" a b) =>
          Lens.Family2.LensLike f s t a b
mapType
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "mapType")
maybe'dimParam ::
               forall f s t a b .
                 (Lens.Labels.HasLens f s t "maybe'dimParam" a b) =>
                 Lens.Family2.LensLike f s t a b
maybe'dimParam
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'dimParam")
maybe'dimValue ::
               forall f s t a b .
                 (Lens.Labels.HasLens f s t "maybe'dimValue" a b) =>
                 Lens.Family2.LensLike f s t a b
maybe'dimValue
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'dimValue")
maybe'elemType ::
               forall f s t a b .
                 (Lens.Labels.HasLens f s t "maybe'elemType" a b) =>
                 Lens.Family2.LensLike f s t a b
maybe'elemType
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'elemType")
maybe'g ::
        forall f s t a b . (Lens.Labels.HasLens f s t "maybe'g" a b) =>
          Lens.Family2.LensLike f s t a b
maybe'g
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'g")
maybe'graph ::
            forall f s t a b . (Lens.Labels.HasLens f s t "maybe'graph" a b) =>
              Lens.Family2.LensLike f s t a b
maybe'graph
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'graph")
maybe'mapType ::
              forall f s t a b .
                (Lens.Labels.HasLens f s t "maybe'mapType" a b) =>
                Lens.Family2.LensLike f s t a b
maybe'mapType
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'mapType")
maybe'segment ::
              forall f s t a b .
                (Lens.Labels.HasLens f s t "maybe'segment" a b) =>
                Lens.Family2.LensLike f s t a b
maybe'segment
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'segment")
maybe'sequenceType ::
                   forall f s t a b .
                     (Lens.Labels.HasLens f s t "maybe'sequenceType" a b) =>
                     Lens.Family2.LensLike f s t a b
maybe'sequenceType
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'sequenceType")
maybe'shape ::
            forall f s t a b . (Lens.Labels.HasLens f s t "maybe'shape" a b) =>
              Lens.Family2.LensLike f s t a b
maybe'shape
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'shape")
maybe't ::
        forall f s t a b . (Lens.Labels.HasLens f s t "maybe't" a b) =>
          Lens.Family2.LensLike f s t a b
maybe't
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe't")
maybe'tensorType ::
                 forall f s t a b .
                   (Lens.Labels.HasLens f s t "maybe'tensorType" a b) =>
                   Lens.Family2.LensLike f s t a b
maybe'tensorType
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'tensorType")
maybe'type' ::
            forall f s t a b . (Lens.Labels.HasLens f s t "maybe'type'" a b) =>
              Lens.Family2.LensLike f s t a b
maybe'type'
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'type'")
maybe'value ::
            forall f s t a b . (Lens.Labels.HasLens f s t "maybe'value" a b) =>
              Lens.Family2.LensLike f s t a b
maybe'value
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'value")
maybe'valueType ::
                forall f s t a b .
                  (Lens.Labels.HasLens f s t "maybe'valueType" a b) =>
                  Lens.Family2.LensLike f s t a b
maybe'valueType
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'valueType")
metadataProps ::
              forall f s t a b .
                (Lens.Labels.HasLens f s t "metadataProps" a b) =>
                Lens.Family2.LensLike f s t a b
metadataProps
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "metadataProps")
modelVersion ::
             forall f s t a b .
               (Lens.Labels.HasLens f s t "modelVersion" a b) =>
               Lens.Family2.LensLike f s t a b
modelVersion
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "modelVersion")
name ::
     forall f s t a b . (Lens.Labels.HasLens f s t "name" a b) =>
       Lens.Family2.LensLike f s t a b
name
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "name")
node ::
     forall f s t a b . (Lens.Labels.HasLens f s t "node" a b) =>
       Lens.Family2.LensLike f s t a b
node
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "node")
opType ::
       forall f s t a b . (Lens.Labels.HasLens f s t "opType" a b) =>
         Lens.Family2.LensLike f s t a b
opType
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "opType")
opsetImport ::
            forall f s t a b . (Lens.Labels.HasLens f s t "opsetImport" a b) =>
              Lens.Family2.LensLike f s t a b
opsetImport
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "opsetImport")
output ::
       forall f s t a b . (Lens.Labels.HasLens f s t "output" a b) =>
         Lens.Family2.LensLike f s t a b
output
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "output")
producerName ::
             forall f s t a b .
               (Lens.Labels.HasLens f s t "producerName" a b) =>
               Lens.Family2.LensLike f s t a b
producerName
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "producerName")
producerVersion ::
                forall f s t a b .
                  (Lens.Labels.HasLens f s t "producerVersion" a b) =>
                  Lens.Family2.LensLike f s t a b
producerVersion
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "producerVersion")
rawData ::
        forall f s t a b . (Lens.Labels.HasLens f s t "rawData" a b) =>
          Lens.Family2.LensLike f s t a b
rawData
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "rawData")
s ::
  forall f s t a b . (Lens.Labels.HasLens f s t "s" a b) =>
    Lens.Family2.LensLike f s t a b
s = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "s")
segment ::
        forall f s t a b . (Lens.Labels.HasLens f s t "segment" a b) =>
          Lens.Family2.LensLike f s t a b
segment
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "segment")
sequenceType ::
             forall f s t a b .
               (Lens.Labels.HasLens f s t "sequenceType" a b) =>
               Lens.Family2.LensLike f s t a b
sequenceType
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "sequenceType")
shape ::
      forall f s t a b . (Lens.Labels.HasLens f s t "shape" a b) =>
        Lens.Family2.LensLike f s t a b
shape
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "shape")
stringData ::
           forall f s t a b . (Lens.Labels.HasLens f s t "stringData" a b) =>
             Lens.Family2.LensLike f s t a b
stringData
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "stringData")
strings ::
        forall f s t a b . (Lens.Labels.HasLens f s t "strings" a b) =>
          Lens.Family2.LensLike f s t a b
strings
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "strings")
t ::
  forall f s t a b . (Lens.Labels.HasLens f s t "t" a b) =>
    Lens.Family2.LensLike f s t a b
t = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "t")
tensorType ::
           forall f s t a b . (Lens.Labels.HasLens f s t "tensorType" a b) =>
             Lens.Family2.LensLike f s t a b
tensorType
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "tensorType")
tensors ::
        forall f s t a b . (Lens.Labels.HasLens f s t "tensors" a b) =>
          Lens.Family2.LensLike f s t a b
tensors
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "tensors")
type' ::
      forall f s t a b . (Lens.Labels.HasLens f s t "type'" a b) =>
        Lens.Family2.LensLike f s t a b
type'
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "type'")
uint64Data ::
           forall f s t a b . (Lens.Labels.HasLens f s t "uint64Data" a b) =>
             Lens.Family2.LensLike f s t a b
uint64Data
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "uint64Data")
value ::
      forall f s t a b . (Lens.Labels.HasLens f s t "value" a b) =>
        Lens.Family2.LensLike f s t a b
value
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "value")
valueInfo ::
          forall f s t a b . (Lens.Labels.HasLens f s t "valueInfo" a b) =>
            Lens.Family2.LensLike f s t a b
valueInfo
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "valueInfo")
valueType ::
          forall f s t a b . (Lens.Labels.HasLens f s t "valueType" a b) =>
            Lens.Family2.LensLike f s t a b
valueType
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "valueType")
version ::
        forall f s t a b . (Lens.Labels.HasLens f s t "version" a b) =>
          Lens.Family2.LensLike f s t a b
version
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "version")