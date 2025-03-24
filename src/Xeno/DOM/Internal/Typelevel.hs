{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

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

uncurryProduct :: (a -> b -> c) -> (a `HCons` b) -> c
uncurryProduct f (a `HCons` b) = f a b

curryProduct :: ((a `HCons` b) -> c) -> a -> b -> c
curryProduct f a b = f (a `HCons` b)

type family All (p :: k -> Constraint) (as :: [k]) :: Constraint where
  All p '[]       = ()
  All p (a ': as) = (p a, All p as)

type family Foldr (c :: k -> l -> l) (n :: l) (as :: [k]) :: l where
  Foldr c n '[]       = n
  Foldr c n (a ': as) = c a (Foldr c n as)

type Arrows   (as :: [Type]) (r :: Type) = Foldr (->) r as
type Products (as :: [Type])             = Foldr HCons () as

type family Fields (as :: [k]) :: l where
  Fields '[] = ()
  Fields (x ': xs) = String `HCons` Fields xs

class Currying as b where
  uncurrys :: Arrows as b -> Products as -> b
  currys   :: (Products as -> b) -> Arrows as b

instance Currying '[] b where
  currys   f = f ()
  uncurrys f () = f

instance Currying as b => Currying (a ': as) b where
  uncurrys f = uncurryProduct $ uncurrys @as @b . f
  currys   f = currys @as @b . curryProduct f