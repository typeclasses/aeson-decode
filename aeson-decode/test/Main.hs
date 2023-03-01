module Main (main) where

import AesonDecode
import Essentials

import Control.Applicative (Alternative (..))
import Data.Aeson.QQ (aesonQQ)
import Data.Text (Text)
import Prelude (Integer, (+))
import System.IO (IO)
import Test.Hspec (describe, it, hspec, shouldBe)
import Data.Either (Either (..))
import Data.String (String)

import qualified Examples.Asset
import qualified Examples.Policy

main :: IO ()
main = hspec do

    describe "either" do
        let d :: Decoder (Either Text Integer) = (text <&> Left) <|> (integer <&> Right)
        it "Just Left"  $ ([aesonQQ| "x"  |] ^? d) `shouldBe` Just (Left "x")
        it "Just Right" $ ([aesonQQ| 5    |] ^? d) `shouldBe` Just (Right 5)
        it "Nothing"    $ ([aesonQQ| null |] ^? d) `shouldBe` Nothing

    describe "eitherTagged" do
        let d :: Decoder (Either Integer Integer) =
                (at "type" (textIs "x") *> at "value" integer <&> Left) <|>
                (at "type" (textIs "y") *> at "value" integer <&> Right)
        it "Just Left"  $ ([aesonQQ| {"type": "x", "value": 1} |] ^? d) `shouldBe` Just (Left 1)
        it "Just Right" $ ([aesonQQ| {"type": "y", "value": 2} |] ^? d) `shouldBe` Just (Right 2)
        it "Nothing"    $ ([aesonQQ| {"type": "z", "value": 3} |] ^? d) `shouldBe` Nothing

    describe "constSuccessDecoder" do
        it "is lazy" $ (undefined ^? constSuccessDecoder @Integer 6) `shouldBe` Just 6

    describe "failDecoder" do
        it "returns Nothing" $ (undefined ^? failDecoder @Integer) `shouldBe` Nothing

    describe "mapDecoder" do
        let d = mapDecoder (+ 1) integer
        it "Just" $ ([aesonQQ| 4 |] ^? d) `shouldBe` Just 5
        it "Nothing" $ ([aesonQQ| "x" |] ^? d) `shouldBe` Nothing

    describe "apDecoder" do
        let d = (,) `mapDecoder` (at "x" integer) `apDecoder` (at "y" text)
        it "works" $ ([aesonQQ| {"x": 4, "y": "abc"} |] ^? d) `shouldBe` Just (4, "abc")

    describe "composeDecoderFunctions" do
        let f x = at (textPath x) text
            d = composeDecoderFunctions f f "a"
        it "works" $ ([aesonQQ| {"a": "b", "b": "c"} |] ^? d) `shouldBe` Just "c"

    describe "orElse" do
        let d = orElse (Left <$> text) (Right <$> integer)
        it "Just Right" $ ([aesonQQ| 4 |] ^? d) `shouldBe` Just (Right 4)
        it "Just Left" $ ([aesonQQ| "x" |] ^? d) `shouldBe` Just (Left "x")
        it "Nothing" $ ([aesonQQ| null |] ^? d) `shouldBe` Nothing

    describe "defaultDecoder" do
        it "Integer" $ ([aesonQQ| 4 |] ^? defaultDecoder :: Maybe Integer) `shouldBe` Just 4
        it "String" $ ([aesonQQ| "x" |] ^? defaultDecoder :: Maybe String) `shouldBe` Just "x"
        it "[Integer]" $ ([aesonQQ| [4,5,6] |] ^? defaultDecoder :: Maybe [Integer]) `shouldBe` Just [4, 5, 6]

    describe "is" do
        it "Just" $ ([aesonQQ| 4 |] ^? is @Integer 4) `shouldBe` Just ()
        it "Nothing" $ ([aesonQQ| 5 |] ^? is @Integer 4) `shouldBe` Nothing

    describe "only" do
        it "0" $ ([aesonQQ| [] |] ^? at only integer) `shouldBe` Nothing
        it "1" $ ([aesonQQ| [4] |] ^? at only integer) `shouldBe` Just 4
        it "2" $ ([aesonQQ| [4,5] |] ^? at only integer) `shouldBe` Nothing

    describe "null" do
        it "null -> Just" $ ([aesonQQ| null |] ^? null) `shouldBe` Just ()
        it "[] -> Nothing" $ ([aesonQQ| [] |] ^? null) `shouldBe` Nothing
        it "number -> Nothing" $ ([aesonQQ| 4 |] ^? null) `shouldBe` Nothing

    describe "nullable" do
        it "Just Just" $ ([aesonQQ| 4 |] ^? nullable integer) `shouldBe` Just (Just 4)
        it "Just Nothing" $ ([aesonQQ| null |] ^? nullable integer) `shouldBe` Just Nothing
        it "Nothing" $ ([aesonQQ| "x" |] ^? nullable integer) `shouldBe` Nothing

    describe "text" do
        it "Just" $ ([aesonQQ| "x" |] ^? text) `shouldBe` Just "x"
        it "Nothing" $ ([aesonQQ| 4 |] ^? text) `shouldBe` Nothing

    describe "textIs" do
        it "Just" $ ([aesonQQ| "x" |] ^? textIs "x") `shouldBe` Just ()
        it "Nothing" $ ([aesonQQ| "a" |] ^? textIs "x") `shouldBe` Nothing

    describe "integer" do
        it "Just" $ ([aesonQQ| 4 |] ^? integer) `shouldBe` Just 4
        it "Nothing" $ ([aesonQQ| "x" |] ^? integer) `shouldBe` Nothing

    describe "integerIs" do
        it "Just" $ ([aesonQQ| 4 |] ^? integerIs 4) `shouldBe` Just ()
        it "Nothing" $ ([aesonQQ| 5 |] ^? integerIs 4) `shouldBe` Nothing

    describe "bool" do
        it "Just True" $ ([aesonQQ| true |] ^? bool) `shouldBe` Just True
        it "Just False" $ ([aesonQQ| false |] ^? bool) `shouldBe` Just False
        it "Nothing" $ ([aesonQQ| "x" |] ^? bool) `shouldBe` Nothing

    describe "boolIs" do
        it "Just" $ ([aesonQQ| true |] ^? boolIs True) `shouldBe` Just ()
        it "Nothing" $ ([aesonQQ| false |] ^? boolIs True) `shouldBe` Nothing

    describe "true" do
        it "Just" $ ([aesonQQ| true |] ^? true) `shouldBe` Just ()
        it "Nothing" $ ([aesonQQ| false |] ^? true) `shouldBe` Nothing

    describe "false" do
        it "Just" $ ([aesonQQ| false |] ^? false) `shouldBe` Just ()
        it "Nothing" $ ([aesonQQ| true |] ^? false) `shouldBe` Nothing

    describe "vectorOf" do
        it "Just empty" $ ([aesonQQ| [] |] ^? vectorOf integer) `shouldBe` Just []
        it "Just nonempty" $ ([aesonQQ| [4,5,6] |] ^? vectorOf integer) `shouldBe` Just [4, 5, 6]
        it "Nothing" $ ([aesonQQ| ["4","5","6"] |] ^? vectorOf integer) `shouldBe` Nothing

    describe "listOf" do
        it "Just empty" $ ([aesonQQ| [] |] ^? listOf integer) `shouldBe` Just []
        it "Just nonempty" $ ([aesonQQ| [4,5,6] |] ^? listOf integer) `shouldBe` Just [4, 5, 6]
        it "Nothing" $ ([aesonQQ| ["4","5","6"] |] ^? listOf integer) `shouldBe` Nothing

    describe "hashMapOf" do
        it "Just empty" $ ([aesonQQ| {} |] ^? hashMapOf integer) `shouldBe` Just []
        it "Just nonempty" $ ([aesonQQ| {"a": 4, "b": 5} |] ^? hashMapOf integer) `shouldBe` Just [("a", 4), ("b", 5)]
        it "Nothing" $ ([aesonQQ| 4 |] ^? hashMapOf integer) `shouldBe` Nothing

    describe "ordMapOf" do
        it "Just empty" $ ([aesonQQ| {} |] ^? ordMapOf integer) `shouldBe` Just []
        it "Just nonempty" $ ([aesonQQ| {"a": 4, "b": 5} |] ^? ordMapOf integer) `shouldBe` Just [("a", 4), ("b", 5)]
        it "Nothing" $ ([aesonQQ| 4 |] ^? ordMapOf integer) `shouldBe` Nothing

    Examples.Asset.spec
    Examples.Policy.spec
