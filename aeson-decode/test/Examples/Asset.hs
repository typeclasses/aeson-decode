module Examples.Asset (spec) where

import AesonDecode
import Essentials

import Control.Applicative (Alternative (..))
import Data.Aeson (Value)
import Data.Aeson.QQ (aesonQQ)
import Data.Text (Text)
import Test.Hspec (it, shouldBe, Spec)

data Asset
  = Asset'Image Text
  | Asset'Video Text Text
  deriving (Eq, Show)

spec :: Spec
spec = it "asset example" $
    (json ^? d) `shouldBe` Just
        [ Asset'Video "https://subscriber.typeclasses.com/video/js-operators-2/dash/manifest.mpd"
                      "/_/static/operators-video.jpg"
        , Asset'Image "/_/static/acme.jpg"
        ]

d'image, d'video :: Decoder Asset
d :: Decoder [Asset]

d'image = do
    at "type" (textIs "image")
    Asset'Image <$> at "url" text

d'video = do
    at "type" (textIs "video")
    Asset'Video <$> at "url" text
                <*> at "poster" text

d = at "assets" $ listOf (d'image <|> d'video)

json :: Value
json =
    [aesonQQ|
      {
        "assets": [
          {
            "type": "video",
            "url": "https://subscriber.typeclasses.com/video/js-operators-2/dash/manifest.mpd",
            "poster": "/_/static/operators-video.jpg"
          },
          {
            "type": "image",
            "url": "/_/static/acme.jpg"
          }
        ]
      }
    |]
