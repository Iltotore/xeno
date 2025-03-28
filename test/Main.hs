{-# LANGUAGE CPP               #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

-- | Simple test suite.

module Main where

import           Data.Either (isRight)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Test.Hspec
import           Xeno.SAX  (validate, skipDoctype)
import           Xeno.DOM  (Node, Content(..), parse, name, contents, attributes, children)
import qualified Xeno.DOM.Robust as RDOM
import           Xeno.Types
import qualified Debug.Trace as Debug(trace)
import Xeno.DOM.Decoding

newtype Username = Username String
  deriving (AttrDecoder, Eq, Show)

data UserInfo = UserInfo Username Int
  deriving (Eq, Show)

data Friend = Friend Username Username
  deriving (Eq, Show)

data User = User UserInfo [Friend]
  deriving (Eq, Show)

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe "Xeno.DOM tests" $ do
    it "test 1" $ do
      xml <- BS.readFile "data/books-4kb.xml"
      let (Right dom) = parse xml
      (name dom) `shouldBe` "catalog"
      (length $ contents dom) `shouldBe` 25
      (length $ children dom) `shouldBe` 12
      (length $ allChildrens dom) `shouldBe` 84
      (length $ concatMap attributes $ allChildrens dom) `shouldBe` 12
      (concatMap attributes $ allChildrens dom) `shouldBe`
          [("id","bk101"),("id","bk102"),("id","bk103"),("id","bk104")
          ,("id","bk105"),("id","bk106"),("id","bk107"),("id","bk108")
          ,("id","bk109"),("id","bk110"),("id","bk111"),("id","bk112")]
      (map name $ allChildrens dom) `shouldBe`
          (replicate 12 "book" ++ (concat $
          replicate 12 ["author","title","genre","price","publish_date","description"]))
  describe "Xeno.DOM tests" $ do
    it "DOM from bytestring substring" $ do
      let substr = BS.drop 5 "5<8& <valid>xml<here/></valid>"
          parsedRoot = fromRightE $ parse substr
      name parsedRoot `shouldBe` "valid"

    it "Leading whitespace characters are accepted by parse" $
      isRight (parse "\n<a></a>") `shouldBe` True

    let doc =
              parse
                "<root><test id=\"1\" extra=\"2\" />\n<test id=\"2\" /><b><test id=\"3\" /></b><test id=\"4\" /><test /></root>"

    it "children test" $
      map name (children $ fromRightE doc) `shouldBe` ["test", "test", "b", "test", "test"]

    it "attributes" $
      attributes (head (children $ fromRightE doc)) `shouldBe` [("id", "1"), ("extra", "2")]

    it "xml prologue test" $ do
      let docWithPrologue = "<?xml version=\"1.1\"?>\n<greeting>Hello, world!</greeting>"
          parsedRoot = fromRightE $ Xeno.DOM.parse docWithPrologue
      name parsedRoot `shouldBe` "greeting"
  describe "DOM.Decoding tests" $ do
    describe "AttrDecoder tests" $ do
      it "ByteString" $
        decodeAttr @ByteString "abc" `shouldBe` Right "abc"

      it "String" $
        decodeAttr @String "abc" `shouldBe` Right "abc"

      it "Bool" $ do
        decodeAttr "true" `shouldBe` Right True
        decodeAttr "True" `shouldBe` Right True
        decodeAttr "1" `shouldBe` Right True
        decodeAttr "false" `shouldBe` Right False
        decodeAttr "False" `shouldBe` Right False
        decodeAttr "0" `shouldBe` Right False
        decodeAttr @Bool "foo" `shouldBe` Left "Invalid bool: foo"

      it "Int" $ do
        decodeAttr @Int "5" `shouldBe` Right 5
        decodeAttr @Int "-5" `shouldBe` Right (-5)
        decodeAttr @Int "abc" `shouldBe` Left "Invalid int: abc"

      it "Integer" $ do
        decodeAttr @Integer "5" `shouldBe` Right 5
        decodeAttr @Integer "-5" `shouldBe` Right (-5)
        decodeAttr @Integer "abc" `shouldBe` Left "Invalid integer: abc"

      it "Float" $ do
        decodeAttr @Float "5" `shouldBe` Right 5
        decodeAttr @Float "-5" `shouldBe` Right (-5)
        decodeAttr @Float "5.2" `shouldBe` Right 5.2
        decodeAttr @Float "abc" `shouldBe` Left "Invalid float: abc"

      it "Double" $ do
        decodeAttr @Double "5" `shouldBe` Right 5
        decodeAttr @Double "-5" `shouldBe` Right (-5)
        decodeAttr @Double "5.2" `shouldBe` Right 5.2
        decodeAttr @Double "abc" `shouldBe` Left "Invalid double: abc"

    describe "NodeDecoder tests" $ do
      let dummyNode = fromRightE $ parse "<foo bar=\"5\"/>"
      it "return" $
        decodeXML @String (return "abc") dummyNode `shouldBe` Right "abc"

      it "fail" $
        decodeXML @() (decodeFail "abc") dummyNode `shouldBe` Left [DecodingFailure RootCursor "abc"]

      it "contramapCursor" $
        decodeXML @() (contramapCursor (NodeCursor "foo") (decodeFail "abc")) dummyNode
        `shouldBe` Left [DecodingFailure (NodeCursor "foo" RootCursor) "abc"]

      it "zipHCons" $ do
        decodeXML (zipHCons @String @Int (return "abc") (return 5)) dummyNode
          `shouldBe` Right ("abc" `HCons` 5)

        decodeXML (zipHCons @String @Int (decodeFail "abc") (return 5)) dummyNode
          `shouldBe` Left [DecodingFailure RootCursor "abc"]

        decodeXML
          (zipHCons
            (zipHCons @String @Int (return "abc") (return 5))
            (return True)
          )
          dummyNode
          `shouldBe` Right (("abc" `HCons` 5) `HCons` True)

      it "findAttribute" $ do
        findAttribute "bar" dummyNode `shouldBe` Just "5"
        findAttribute "missing" dummyNode `shouldBe` Nothing

      it "mapOrFail" $ do
        decodeXML @Int (mapOrFail (Right . (2 *)) (return 2)) dummyNode
          `shouldBe` Right 4

        decodeXML @Int (mapOrFail (\_ -> Left "abc") (return (2 :: Int))) dummyNode
          `shouldBe` Left [DecodingFailure RootCursor "abc"]

      it "decodeField" $ do
        decodeXML @Int (decodeField "bar") dummyNode `shouldBe` Right 5

        decodeXML @Int (decodeField "baz") dummyNode
          `shouldBe` Left [DecodingFailure (FieldCursor "baz" RootCursor) "Field not found: baz"]

        decodeXML @Bool (decodeField "bar") dummyNode
          `shouldBe` Left [DecodingFailure (FieldCursor "bar" RootCursor) "Invalid bool: 5"]

      it "mapAll" $
        decodeXML
          (mapAll
            @[String, Int]
            (\a b -> a ++ show b)
            (return ("abc" `HCons` 5 `HCons` ()))
          )
          dummyNode
          `shouldBe` Right "abc5"

      it "decodeAssert" $ do
        decodeXML
          (decodeAssert (\n -> name n == "foo") (const "Wrong name"))
          dummyNode
          `shouldBe` Right ()

        decodeXML
          (decodeAssert (\n -> name n == "bar") (const "Wrong name"))
          dummyNode
          `shouldBe` Left [DecodingFailure RootCursor "Wrong name"]

      it "inNode" $ do
        decodeXML (inNode "foo" (return ())) dummyNode `shouldBe` Right ()
        decodeXML (inNode "bar" (return ())) dummyNode
          `shouldBe` Left [DecodingFailure (NodeCursor "bar" RootCursor) "Invalid node name: foo"]

      it "decodeAllAttributes" $ do
        let nodeOk = fromRightE $ parse "<foo a=\"1\" b=\"abc\" c=\"true\"/>"
        let nodeNotOk = fromRightE $ parse "<foo a=\"abc\" c=\"true\"/>"

        decodeXML
          (decodeAllAttributes
            @[Int, String, Bool]
            (,,)
            ("a" `HCons` "b" `HCons` "c" `HCons` ())
          )
          nodeOk
          `shouldBe` Right (1, "abc", True)

        decodeXML
          (decodeAllAttributes
            @[Int, String, Bool]
            (,,)
            ("a" `HCons` "b" `HCons` "c" `HCons` ())
          )
          nodeNotOk
          `shouldBe` Left
            [ DecodingFailure (FieldCursor "a" RootCursor) "Invalid int: abc"
            , DecodingFailure (FieldCursor "b" RootCursor) "Field not found: b"
            ]

      it "decodeChildren" $ do
        let nodeOk = fromRightE $ parse "<foo><bar/><bar/></foo>"
        let nodeEmpty = fromRightE $ parse "<foo></foo>"
        let nodeNotOk = fromRightE $ parse "<foo><bar/><baz/></foo>"

        let fooDecoder = inNode "foo" (decodeChildren (inNode "bar" (return ())))
        let bazCursor = NodeCursor "bar" (NodeCursor "foo" RootCursor)

        decodeXML fooDecoder nodeOk `shouldBe` Right [(), ()]
        decodeXML fooDecoder nodeEmpty `shouldBe` Right []
        decodeXML fooDecoder nodeNotOk
          `shouldBe` Left [DecodingFailure bazCursor "Invalid node name: baz"]

      it "fullyFledged" $
        let
          decodeFriend = inNode
            "friend"
            (decodeAllAttributes
              @[Username, Username]
              Friend
              ("name" `HCons` "nickname" `HCons` ())
            )

          decodeUser = inNode
            "user"
            (do
              info <- decodeAllAttributes
                @[Username, Int]
                UserInfo
                ("name" `HCons` "age" `HCons` ())
              friends <- decodeChildren decodeFriend
              return (User info friends)
            )

          xml1 = fromRightE $ parse "<user name=\"John\" age=\"30\"/>"
          xml2 = fromRightE $ parse "<user name=\"John\" age=\"30\"><friend name=\"Alice\" nickname=\"Sis\"/></user>"
          xml3 = fromRightE $ parse "<user age=\"abc\"/>"
          xml4 = fromRightE $ parse "<user name=\"John\" age=\"25\"><friend/></user>"
        in do
          decodeXML decodeUser xml1 `shouldBe` Right (User (UserInfo (Username "John") 30) [])

          decodeXML decodeUser xml2 `shouldBe` Right
            (User
              (UserInfo (Username "John") 30)
              [Friend (Username "Alice") (Username "Sis")]
            )

          decodeXML decodeUser xml3 `shouldBe` Left
            [ DecodingFailure (FieldCursor "name" (NodeCursor "user" RootCursor)) "Field not found: name"
            , DecodingFailure (FieldCursor "age" (NodeCursor "user" RootCursor)) "Invalid int: abc"
            ]

          let friendCursor = NodeCursor "friend" (NodeCursor "user" RootCursor)

          decodeXML decodeUser xml4 `shouldBe` Left
            [ DecodingFailure (FieldCursor "name" friendCursor) "Field not found: name"
            , DecodingFailure (FieldCursor "nickname" friendCursor) "Field not found: nickname"
            ]

  describe
    "hexml tests"
    (do mapM_
          (\(v, i) -> it (show i) (shouldBe (validate i) v)) $ concat
          [ hexml_examples_sax
          , extra_examples_sax



          ]
        mapM_
          (\(v, i) -> it (show i) (shouldBe (either (Left . show) (Right . id) (contents <$> parse i)) v))
          cdata_tests

       -- If this works without crashing we're happy.
        let nsdoc = ("<ns:tag os:attr=\"Namespaced attribute value\">Content.</ns:tag>" :: ByteString)
        it
          "namespaces" $
          validate nsdoc `shouldBe` True
    )
  describe "robust XML tests" $ do
    it "DOM from bytestring substring" $ do
        let substr = BS.drop 5 "5<8& <valid>xml<here/></valid>"
            parsedRoot = fromRightE $ RDOM.parse substr
        name parsedRoot `shouldBe` "valid"

    it "Leading whitespace characters are accepted by parse" $
      isRight (RDOM.parse "\n<a></a>") `shouldBe` True

    let doc =
              RDOM.parse
                "<root><test id=\"1\" extra=\"2\" />\n<test id=\"2\" /><b><test id=\"3\" /></b><test id=\"4\" /><test /></root>"

    it "children test" $
      map name (children $ fromRightE doc) `shouldBe` ["test", "test", "b", "test", "test"]

    it "attributes" $
      attributes (head (children $ fromRightE doc)) `shouldBe` [("id", "1"), ("extra", "2")]

    it "xml prologue test" $ do
      let docWithPrologue = "<?xml version=\"1.1\"?>\n<greeting>Hello, world!</greeting>"
          parsedRoot = fromRightE $ RDOM.parse docWithPrologue
      name parsedRoot `shouldBe` "greeting"
    it "html doctype test" $ do
      let docWithPrologue = "<!DOCTYPE html>\n<greeting>Hello, world!</greeting>"
          parsedRoot = fromRightE $ RDOM.parse docWithPrologue
      name parsedRoot `shouldBe` "greeting"

    describe
      "hexml tests"
      (do mapM_
            (\(v, i) -> it (show i) (shouldBe (validate i) v))
            (hexml_examples_sax  ++ extra_examples_sax)
          mapM_
            (\(v, i) -> it (show i) (shouldBe (either (Left . show) (Right . id) (contents <$> parse i)) v))
            cdata_tests

         -- If this works without crashing we're happy.
          let nsdoc = ("<ns:tag os:attr=\"Namespaced attribute value\">Content.</ns:tag>" :: ByteString)
          it
            "namespaces" $
            validate nsdoc `shouldBe` True
      )
    it "recovers unclosed tag" $ do
      let parsed = RDOM.parse "<a attr='a'><img></a>"
      Debug.trace (show parsed) $ do
        name (fromRightE parsed) `shouldBe` "a"
        RDOM.attributes (fromRightE parsed) `shouldBe` [("attr", "a")]
        map name (RDOM.children $ fromRightE parsed) `shouldBe` ["img"]
    it "ignores too many closing tags" $ do
      let parsed = RDOM.parse "<a></a></b></c>"
      isRight parsed `shouldBe` True
  describe "skipDoctype" $ do
    it "strips initial doctype declaration" $ do
      skipDoctype "<!DOCTYPE html><?xml version=\"1.0\" encoding=\"UTF-8\"?>Hello" `shouldBe` "<?xml version=\"1.0\" encoding=\"UTF-8\"?>Hello"
    it "strips doctype after spaces" $ do
      skipDoctype "  \n<!DOCTYPE html><?xml version=\"1.0\" encoding=\"UTF-8\"?>Hello" `shouldBe` "<?xml version=\"1.0\" encoding=\"UTF-8\"?>Hello"
    it "does not strip anything after or inside element" $ do
      let insideElt = "<xml><?xml version=\"1.0\" encoding=\"UTF-8\"?>Hello</xml>"
      skipDoctype  insideElt `shouldBe` insideElt

hexml_examples_sax :: [(Bool, ByteString)]
hexml_examples_sax =
    [(True, "<test id='bob'>here<extra/>there</test>")
    ,(True, "<test /><close />")
    ,(True, "<test /><!-- comment > --><close />")
    ,(True, "<test id=\"bob value\" another-attr=\"test with <\">here </test> more text at the end<close />")
    ,(True, "<test></more>") -- SAX doesn't care about tag balancing
    ,(False, "<test")
    ,(True, "<?xml version=\"1.1\"?>\n<greeting>Hello, world!</greeting>")
    ]

extra_examples_sax :: [(Bool, ByteString)]
extra_examples_sax =
    [(True, "<some-example/>")
    ,(True, "<a numeric1=\"attribute\"/>")
    ,(True, "<also.a.dot></also.a.dot>")
    ]

ws_around_equals_sax :: [(Bool, ByteString)]
ws_around_equals_sax =
    [(True, "<o  \nm   = \"100\"\n  gee =  \"0\">")
    ]

-- | We want to make sure that the parser doesn't jump out of the CDATA
-- area prematurely because it encounters a single ].
cdata_tests :: [(Either a [Content], ByteString)]
cdata_tests =
    [ ( Right [CData "Oneliner CDATA."]
      , "<test><![CDATA[Oneliner CDATA.]]></test>")
    , ( Right [CData "<strong>This is strong but not XML tags.</strong>"]
      , "<test><![CDATA[<strong>This is strong but not XML tags.</strong>]]></test>")
    , ( Right [CData "A lonely ], sad isn't it?"]
      , "<test><![CDATA[A lonely ], sad isn't it?]]></test>")
    ]

-- | Horrible hack. Don't try this at home.
fromRightE :: Either XenoException a -> a
fromRightE = either (error . show) id

mapLeft :: Applicative f => (a -> f b) -> Either a b -> f b
mapLeft f = either f pure

mapRight :: Applicative f => (b -> f a) -> Either a b -> f a
mapRight = either pure

allChildrens :: Node -> [Node]
allChildrens n = allChildrens' [n]
  where
    allChildrens' :: [Node] -> [Node]
    allChildrens' [] = []
    allChildrens' ns =
        let nextNodes = concatMap children ns
        in nextNodes ++ (allChildrens' nextNodes)

