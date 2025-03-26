{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

-- |Module for type-level operations.
module Xeno.DOM.Internal.Typelevel
  ( All
  , Arrows
  , Currying (currys, uncurrys)
  , Fields
  , Foldr
  , Products
  )
where
import GHC.Exts (Constraint)
import Data.Kind (Type)
import Xeno.Types (HCons (HCons))

-- |Uncurry a binary function to a `HCons`.
uncurryProduct :: (a -> b -> c) -> (a `HCons` b) -> c
uncurryProduct f (a `HCons` b) = f a b

-- |Curry a `HCons` to a binary function.
curryProduct :: ((a `HCons` b) -> c) -> a -> b -> c
curryProduct f a b = f (a `HCons` b)

-- |Constraint ensuring all types in the given HList satisfy a specific constraint.
type family All (p :: k -> Constraint) (as :: [k]) :: Constraint where
  All p '[]       = ()
  All p (a ': as) = (p a, All p as)

-- |Type-level fold right.
type family Foldr (c :: k -> l -> l) (n :: l) (as :: [k]) :: l where
  Foldr c n '[]       = n
  Foldr c n (a ': as) = c a (Foldr c n as)

-- |Type of a function taking each element of `as` as argument and returning `r`.
type Arrows   (as :: [Type]) (r :: Type) = Foldr (->) r as

-- |Type of a HCons containing each element of `as`.
-- Often used as extractor.
type Products (as :: [Type])             = Foldr HCons () as

-- |Type of a HList of the same size as the `as`, containing only Strings.
type family Fields (as :: [k]) :: l where
  Fields '[] = ()
  Fields (x ': xs) = String `HCons` Fields xs

-- |Typeclass for uncurrying n-ary functions to n-size HList and vice-versa.
class Currying as b where
  -- |Uncurry a n-ary function to a function taking a n-size HList.
  uncurrys :: Arrows as b -> Products as -> b

  -- |Curry a function taking a n-size HList to a n-ary function
  currys   :: (Products as -> b) -> Arrows as b

instance Currying '[] b where
  currys   f = f ()
  uncurrys f () = f

instance Currying as b => Currying (a ': as) b where
  uncurrys f = uncurryProduct $ uncurrys @as @b . f
  currys   f = currys @as @b . curryProduct f