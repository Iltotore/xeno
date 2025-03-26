{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- Public API
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


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

data DecodingCursor =
  RootCursor
  | FieldCursor String DecodingCursor
  | NodeCursor String DecodingCursor
  deriving Show

cursorParent :: DecodingCursor -> Maybe DecodingCursor
cursorParent RootCursor = Nothing
cursorParent (FieldCursor _ parent) = Just parent
cursorParent (NodeCursor _ parent)  = Just parent

data DecodingFailure = DecodingFailure DecodingCursor String
  deriving Show

newtype NodeDecoder a = NodeDecoder { applyDecoder :: DecodingCursor -> Node -> Either [DecodingFailure] a }

decodeXML :: NodeDecoder a -> Node -> Either [DecodingFailure] a
decodeXML decoder = applyDecoder decoder RootCursor

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

contramapCursor :: (DecodingCursor -> DecodingCursor) -> NodeDecoder a -> NodeDecoder a
contramapCursor f decoder = NodeDecoder (applyDecoder decoder . f)

zipProduct :: NodeDecoder a -> NodeDecoder b -> NodeDecoder (a `HCons` b)
zipProduct da db = NodeDecoder
  (\c n -> case (applyDecoder da c n, applyDecoder db c n) of
    (Right x, Right y) -> Right (x `HCons` y)
    (Right _, Left yErr) -> Left yErr
    (Left xErr, Right _) -> Left xErr
    (Left xErr, Left yErr) -> Left (xErr ++ yErr)
  )

findAttribute :: String -> Node -> Maybe ByteString
findAttribute fname node = fmap snd (find (\(n, _) -> n == pack fname) (attributes node))

findAttributeOrFail :: String -> DecodingCursor -> Node -> Either [DecodingFailure] ByteString
findAttributeOrFail fname cursor node = case findAttribute fname node of
  Just result -> Right result
  Nothing -> Left [DecodingFailure (FieldCursor fname cursor) ("Field not found: " ++ fname)]

mapOrFail :: (a -> Either String b) -> NodeDecoder a -> NodeDecoder b
mapOrFail f da = NodeDecoder
  (\c n -> case applyDecoder da c n of
    Left err -> Left err
    Right x  -> case f x of
      Left msg -> Left [DecodingFailure c msg]
      Right y  -> Right y
  )

decodeField :: AttrDecoder a => String -> NodeDecoder a
decodeField attrName = NodeDecoder
  (\c n -> case findAttributeOrFail attrName c n of
    Left err   -> Left err
    Right attr -> case decodeAttr attr of
      Left msg -> Left [DecodingFailure (FieldCursor attrName c) msg]
      Right x  -> Right x
  )

mapAll :: forall as b. Currying as b => Arrows as b -> NodeDecoder (Products as) -> NodeDecoder b
mapAll f = fmap (uncurrys @as f)

class ToFieldDecoders as where
  fieldDecoders :: Fields as -> NodeDecoder (Products as)

instance ToFieldDecoders '[] where
  fieldDecoders () = return ()

instance (AttrDecoder a, ToFieldDecoders as) => ToFieldDecoders (a ': as) where
  fieldDecoders (x `HCons` xs) = decodeField x `zipProduct` fieldDecoders @as xs

decodeAssert :: (Node -> Bool) -> (Node -> String) -> NodeDecoder ()
decodeAssert fcheck fmsg = NodeDecoder
  (\cursor node ->
    if fcheck node then Right ()
    else Left [DecodingFailure cursor (fmsg node)]
  )

inNode :: String -> NodeDecoder a -> NodeDecoder a
inNode n decoder = contramapCursor
  (NodeCursor n)
  (
    decodeAssert (\nd -> name nd == pack n) (\nd -> "Invalid product name: " ++ unpack (name nd))
    >> decoder
  )

decodeProductAttributes :: forall as b. (Currying as b, ToFieldDecoders as) => Arrows as b -> Fields as -> NodeDecoder b
decodeProductAttributes f fields = mapAll @as f (fieldDecoders @as fields)

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

-- TODO Remove example

newtype Username = Username String
  deriving (Show, AttrDecoder)

data UserInfo = UserInfo Username Int
  deriving Show

data Friend = Friend Username Username
  deriving Show

data User = User UserInfo [Friend]
  deriving Show

decodeFriend :: NodeDecoder Friend
decodeFriend = inNode
    "friend"
    (decodeProductAttributes
      @[Username, Username]
      Friend
      ("name" `HCons` "nickname" `HCons` ())
    )

decodeUser :: NodeDecoder User
decodeUser = inNode
  "user"
  (do
    info <- decodeProductAttributes
      @[Username, Int]
      UserInfo
      ("name" `HCons` "age" `HCons` ())
    friends <- decodeChildren decodeFriend
    return (User info friends)
  )