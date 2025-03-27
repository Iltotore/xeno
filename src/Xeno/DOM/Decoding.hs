{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- Public API


--- |Higher level DOM decoding utils.
module Xeno.DOM.Decoding
where
import Xeno.DOM
import Data.ByteString ( ByteString )
import Data.ByteString.Char8 ( unpack, pack )
import Control.Monad (ap)
import Data.List (find)
import Xeno.DOM.Internal.Typelevel
import Text.Read (readEither)
import Xeno.Types (HCons (HCons))
import Data.Char (toLower)

--- AttrDecoder ---

-- |Decoder for a single node attribute.
class AttrDecoder a where
  decodeAttr :: ByteString -> Either String a

-- |AttrDecoder for ByteString.
instance AttrDecoder ByteString where
  decodeAttr = Right

-- |AttrDecoder for String.
instance AttrDecoder String where
  decodeAttr = Right . unpack

-- |AttrDecoder for Bool.
instance AttrDecoder Bool where
  decodeAttr bs =
    let
      unpacked = unpack bs
      lower = map toLower unpacked
    in
      if lower == "true" || lower == "1" then Right True
      else if lower == "false" || lower == "0" then Right False
      else Left ("Invalid bool: " ++ unpacked)

-- |AttrDecoder for Int.
instance AttrDecoder Int where
  decodeAttr = decodeFromRead "Invalid int"

-- |AttrDecoder for Integer.
instance AttrDecoder Integer where
  decodeAttr = decodeFromRead "Invalid integer"

-- |AttrDecoder for Float.
instance AttrDecoder Float where
  decodeAttr = decodeFromRead "Invalid float"

-- |AttrDecoder for Double.
instance AttrDecoder Double where
  decodeAttr = decodeFromRead "Invalid double"

-- |Decode an attribute using an instance of `Read`.
decodeFromRead :: Read a => String -> ByteString -> Either String a
decodeFromRead err str = case readEither $ unpack str of
  Right value -> Right value
  Left _ -> Left (err ++ ": " ++ unpack str)

--- NodeDecoder ---

-- |The cursor pointing to a part of the DOM being decoded.
data DecodingCursor =
  RootCursor -- ^The root of the XML arborescence.
  | FieldCursor String DecodingCursor -- ^A field in a node.
  | NodeCursor String DecodingCursor -- ^A node.
  deriving Eq

instance Show DecodingCursor where
  show cursor = case cursor of
    RootCursor -> "root"
    FieldCursor str p -> showAsParent p ++ "field:" ++ str
    NodeCursor  str p -> showAsParent p ++ "node:"  ++ str
    where
      showAsParent :: DecodingCursor -> String
      showAsParent RootCursor = ""
      showAsParent p          = show p ++ " "


-- |Get the parent of a given cursor.
cursorParent :: DecodingCursor -> Maybe DecodingCursor
cursorParent RootCursor = Nothing
cursorParent (FieldCursor _ parent) = Just parent
cursorParent (NodeCursor _ parent)  = Just parent

-- |A decoding failure represented by a cursor where the failure occurred and an error message.
data DecodingFailure = DecodingFailure DecodingCursor String
  deriving Eq

instance Show DecodingFailure where
  show (DecodingFailure cursor str) = "Decoding failure at " ++ show cursor ++ " (" ++ str ++ ")"

-- |An XML node decoder. Since a node can be decoded in several "natural" ways, this is not a typeclass.
-- Instead, the goal is to provide some sort of DSL to easily describe the decoding logic.
newtype NodeDecoder a = NodeDecoder { applyDecoder :: DecodingCursor -> Node -> Either [DecodingFailure] a }

instance Functor NodeDecoder where
  fmap f da = NodeDecoder (\cursor node -> fmap f (applyDecoder da cursor node))

instance Applicative NodeDecoder where
  pure x = NodeDecoder (\_ _ -> Right x)

  (<*>) = ap

instance Monad NodeDecoder where
  return = pure

  da >>= f = NodeDecoder
    (\cursor node -> case applyDecoder da cursor node of
      Right x -> applyDecoder (f x) cursor node
      Left err -> Left err
    )

-- |Decode a XML node. Same as using `applyDecoder` with `RootCursor`.
decodeXML :: NodeDecoder a -> Node -> Either [DecodingFailure] a
decodeXML decoder = applyDecoder decoder RootCursor

-- |Decoder that always fail.
decodeFail :: String -> NodeDecoder a
decodeFail msg = NodeDecoder (\c _ -> Left [DecodingFailure c msg])

-- |Map the cursor passed as input of the given decoder.
contramapCursor :: (DecodingCursor -> DecodingCursor) -> NodeDecoder a -> NodeDecoder a
contramapCursor f decoder = NodeDecoder (applyDecoder decoder . f)

-- |Zip the result of two decoders into a HCons.
zipHCons :: NodeDecoder a -> NodeDecoder b -> NodeDecoder (a `HCons` b)
zipHCons da db = NodeDecoder
  (\c n -> case (applyDecoder da c n, applyDecoder db c n) of
    (Right x, Right y) -> Right (x `HCons` y)
    (Right _, Left yErr) -> Left yErr
    (Left xErr, Right _) -> Left xErr
    (Left xErr, Left yErr) -> Left (xErr ++ yErr)
  )

-- |Find the value of an attribute.
findAttribute :: String -> Node -> Maybe ByteString
findAttribute fname node = fmap snd (find (\(n, _) -> n == pack fname) (attributes node))

-- |Find the value of an attribute, converting Nothing into a failing either.
findAttributeOrFail :: String -> DecodingCursor -> Node -> Either [DecodingFailure] ByteString
findAttributeOrFail fname cursor node = case findAttribute fname node of
  Just result -> Right result
  Nothing -> Left [DecodingFailure (FieldCursor fname cursor) ("Field not found: " ++ fname)]

-- |Map the given value or fail.
mapOrFail :: (a -> Either String b) -> NodeDecoder a -> NodeDecoder b
mapOrFail f da = NodeDecoder
  (\c n -> case applyDecoder da c n of
    Left err -> Left err
    Right x  -> case f x of
      Left msg -> Left [DecodingFailure c msg]
      Right y  -> Right y
  )

-- |Decode a field in the current node using the associated `AttrDecoder`.
decodeField :: AttrDecoder a => String -> NodeDecoder a
decodeField attrName = NodeDecoder
  (\c n -> case findAttributeOrFail attrName c n of
    Left err   -> Left err
    Right attr -> case decodeAttr attr of
      Left msg -> Left [DecodingFailure (FieldCursor attrName c) msg]
      Right x  -> Right x
  )

-- |Map the values of the HList produced by the given `NodeDecoder`
mapAll :: forall as b. Currying as b => Arrows as b -> NodeDecoder (HList as) -> NodeDecoder b
mapAll f = fmap (uncurrys @as f)

-- |Typeclass for producing a HList of String the same size than the given one.
class ToFieldDecoders as where
  fieldDecoders :: Fields as -> NodeDecoder (HList as)

instance ToFieldDecoders '[] where
  fieldDecoders () = return ()

instance (AttrDecoder a, ToFieldDecoders as) => ToFieldDecoders (a ': as) where
  fieldDecoders (x `HCons` xs) = decodeField x `zipHCons` fieldDecoders @as xs

-- |Assert a condition on the decoded node or fail with the given error message.
decodeAssert :: (Node -> Bool) -> (Node -> String) -> NodeDecoder ()
decodeAssert fcheck fmsg = NodeDecoder
  (\cursor node ->
    if fcheck node then Right ()
    else Left [DecodingFailure cursor (fmsg node)]
  )

-- |Use the given decoder inside the current node if the name checks out.
inNode :: String -> NodeDecoder a -> NodeDecoder a
inNode n decoder = contramapCursor
  (NodeCursor n)
  (
    decodeAssert (\nd -> name nd == pack n) (\nd -> "Invalid node name: " ++ unpack (name nd))
    >> decoder
  )

-- |Decode node multiple attributes and map them using the given function, usually to a product.
decodeAllAttributes :: forall as b. (Currying as b, ToFieldDecoders as) => Arrows as b -> Fields as -> NodeDecoder b
decodeAllAttributes f fields = mapAll @as f (fieldDecoders @as fields)

-- |Use the given decoder to decode children inside the current node.
decodeChildren :: NodeDecoder a -> NodeDecoder [a]
decodeChildren childDecoder = NodeDecoder
  (\cursor node ->
    foldr
      (\child acc -> case (applyDecoder childDecoder cursor child, acc) of
        (Right res, Right values) -> Right (res : values)
        (Left resErrors, Right _) -> Left resErrors
        (Right _, Left accErrors) -> Left accErrors
        (Left resErrors, Left accErrors) -> Left (resErrors ++ accErrors)
      )
      (Right [])
      (children node)
  )