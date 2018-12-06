module Myelin.Lambda.NestPrimitive where

import Myelin.Lambda.Lambda as L
import Myelin.Nest.Types.Node as N
import Myelin.Nest.Types.Synapse as S

data Primitive =
      Create 
    | Connect
    | Tuple Primitive Primitive
    | Synapse S.Synapse
    | Node N.Node

type Term = L.Term Primitive

--  exampleTerm = L.A (L.P Create) (L.P (Node ))
    