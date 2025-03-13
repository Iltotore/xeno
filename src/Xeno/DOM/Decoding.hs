{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- Public API

module Xeno.DOM.Decoding
where

import Xeno.DOM
import Data.ByteString ( ByteString )
import Data.ByteString.Char8 ( unpack, pack )
import Control.Monad ((>=>))
import Data.List (find)
import Xeno.DOM.Internal.Typelevel
import Text.Read (readEither)
import Xeno.Types (type (:*:)((:*:)))

--- AttrDecoder ---

class AttrDecoder a where
  decodeAttr :: ByteString -> Either String a

instance AttrDecoder ByteString where
  decodeAttr = Right

instance AttrDecoder String where
  decodeAttr = Right . unpack

instance AttrDecoder Bool where
  decodeAttr = decodeFromRead "Invalid bool"

instance AttrDecoder Int where
  decodeAttr = decodeFromRead "Invalid int"

instance AttrDecoder Integer where
  decodeAttr = decodeFromRead "Invalid integer"

instance AttrDecoder Float where
  decodeAttr = decodeFromRead "Invalid float"

instance AttrDecoder Double where
  decodeAttr = decodeFromRead "Invalid double"

decodeFromRead :: Read a => String -> ByteString -> Either String a
decodeFromRead err str = case readEither $ unpack str of
  Right value -> Right value
  Left _ -> Left (err ++ ": " ++ unpack str)

--- NodeDecoder ---

newtype NodeDecoder a = NodeDecoder { decodeXML :: Node -> Either String a }

instance Functor NodeDecoder where
  fmap f da = NodeDecoder (fmap f . decodeXML da)

zipProduct :: NodeDecoder a -> NodeDecoder b -> NodeDecoder (a :*: b)
zipProduct da db = NodeDecoder (\n -> decodeXML da n >>= (\x -> fmap (x :*:) (decodeXML db n)))

(***) :: NodeDecoder a -> NodeDecoder b -> NodeDecoder (a :*: b)
(***) = zipProduct
infixr ***

findAttribute :: String -> Node -> Maybe ByteString
findAttribute fname node = fmap snd (find (\(n, _) -> n == pack fname) (attributes node))

findAttributeOrFail :: String -> Node -> Either String ByteString
findAttributeOrFail fname node = case findAttribute fname node of
  Just result -> Right result
  Nothing -> Left ("Field not found: " ++ fname)

mapOrFail :: (a -> Either String b) -> NodeDecoder a -> NodeDecoder b
mapOrFail f da = NodeDecoder (decodeXML da >=> f)

decodeField :: AttrDecoder a => String -> NodeDecoder a
decodeField attr1 = NodeDecoder (findAttributeOrFail attr1 >=> decodeAttr)

mapAll :: forall as b. Currying as b => Arrows as b -> NodeDecoder (Products as) -> NodeDecoder b
mapAll f = fmap (uncurrys @as f)

decodeUnit :: NodeDecoder ()
decodeUnit = NodeDecoder (\_ -> Right ())

class ToFieldDecoders as where
  fieldDecoders :: Fields as -> NodeDecoder (Products as)

instance ToFieldDecoders '[] where
  fieldDecoders () = decodeUnit

instance (AttrDecoder a, ToFieldDecoders as) => ToFieldDecoders (a ': as) where
  fieldDecoders (x :*: xs) = decodeField x *** fieldDecoders @as xs

decodeAllFields :: forall as b. (Currying as b, ToFieldDecoders as) => Arrows as b -> Fields as -> NodeDecoder b
decodeAllFields f fields = mapAll @as f (fieldDecoders @as fields)