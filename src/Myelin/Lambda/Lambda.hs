module Myelin.Lambda.Lambda where

type Index = Int

data EvalPrimitive a = EvalPrimitive {
    applyPrimitive :: a
}

-- | lambda term
data Term a =
    V Index -- ^ variable using the barendengt convention
    | L (Term a) -- ^ lambda abstraction
    | A (Term a) (Term a) -- ^ application
    | P a -- ^ primitive

-- | 0 1 2 ...
id i = V i

-- | 1 2 3 ..
liftOne i = V (i + 1)

-- | t sigma(0) sigma(1) ...
cons :: (Index -> Term a) -- ^ substitution to apply
    -> Term a -- ^ term the substitution is applied to
    -> Index -- ^ index under consideration
    -> Term a
cons sigma t i = case i of
    0 -> t
    x -> sigma (x - 1)

applySubs :: (Index -> Term a) -> Term a -> Term a
applySubs sigma t = case t of
    V i -> sigma i
    L t -> L (applySubs (\i -> case i of
                            0 -> V 0
                            i -> applySubs liftOne (sigma (i - 1))) 
                            t)
    A t1 t2 -> A (applySubs sigma t1) (applySubs sigma t2)
    P p -> P p

isValue (L _) = True
isValue (V _) = True
isValue (P _) = True
isValue _ = False