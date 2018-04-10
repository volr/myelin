{- This file was auto-generated from onnx-operators.proto3 by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.OnnxOperators_Fields where
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
import qualified Proto.Onnx

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
irBuildMetadata ::
                forall f s t a b .
                  (Lens.Labels.HasLens f s t "irBuildMetadata" a b) =>
                  Lens.Family2.LensLike f s t a b
irBuildMetadata
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "irBuildMetadata")
irVersion ::
          forall f s t a b . (Lens.Labels.HasLens f s t "irVersion" a b) =>
            Lens.Family2.LensLike f s t a b
irVersion
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "irVersion")
irVersionPrerelease ::
                    forall f s t a b .
                      (Lens.Labels.HasLens f s t "irVersionPrerelease" a b) =>
                      Lens.Family2.LensLike f s t a b
irVersionPrerelease
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) ::
         (Lens.Labels.Proxy#) "irVersionPrerelease")
magic ::
      forall f s t a b . (Lens.Labels.HasLens f s t "magic" a b) =>
        Lens.Family2.LensLike f s t a b
magic
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "magic")
opType ::
       forall f s t a b . (Lens.Labels.HasLens f s t "opType" a b) =>
         Lens.Family2.LensLike f s t a b
opType
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "opType")
operator ::
         forall f s t a b . (Lens.Labels.HasLens f s t "operator" a b) =>
           Lens.Family2.LensLike f s t a b
operator
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "operator")
opsetVersion ::
             forall f s t a b .
               (Lens.Labels.HasLens f s t "opsetVersion" a b) =>
               Lens.Family2.LensLike f s t a b
opsetVersion
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "opsetVersion")
sinceVersion ::
             forall f s t a b .
               (Lens.Labels.HasLens f s t "sinceVersion" a b) =>
               Lens.Family2.LensLike f s t a b
sinceVersion
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "sinceVersion")
status ::
       forall f s t a b . (Lens.Labels.HasLens f s t "status" a b) =>
         Lens.Family2.LensLike f s t a b
status
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "status")