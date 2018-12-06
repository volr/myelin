{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Myelin.Music.Internal (
--    context,
    Setup,
    Runtime,
    ContinuousOutputPort,
    ContinuousInputPort,
    EventOutputPort,
    EventInputPort,
    MessageOutputPort,
    MessageInputPort
) where

-- import Language.C.Inline as C
-- import Language.C.Inline.Context
import Data.Map

data Setup
data Runtime
data ContinuousOutputPort
data ContinuousInputPort
data EventOutputPort
data EventInputPort
data MessageOutputPort
data MessageInputPort

{--
musicTypesTable = Map.fromList
[
  (C.TypeName "MUSIC_Setup", [t| Setup |])
, (C.TypeName "MUSIC_ContOutputPort",    [t| ContOutputPort |])
, (C.TypeName "MUSIC_ContInputPort",     [t| ContInputPort |])
, (C.TypeName "MUSIC_EventOutputPort",   [t| EventOutputPort |])
, (C.TypeName "MUSIC_EventInputPort",    [t| EventInputPort |])
, (C.TypeName "MUSIC_MessageOutputPort", [t| MessageOutputPort |])
, (C.TypeName "MUSIC_MessageInputPort",  [t| MessageInputPort |])
]

context = (C.baseCtx <> C.funCtx <> C.vecCtx <> ctx)
where ctx = mempty { ctxTypesTable = musicTypesTable }
--}
