{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

import AesonDecode

-- aeson-qq
import Data.Aeson.QQ (aesonQQ)

-- base
import Control.Applicative (Alternative (..), optional)
import Control.Monad       (when)
import Data.Semigroup      ((<>))
import System.Exit         (exitFailure)
import System.IO           (hSetEncoding, stderr, stdout, utf8)

-- hedgehog
import Hedgehog

-- text
import Data.Text (Text)

-- time
import Data.Time.Clock.POSIX (POSIXTime)

(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
infixl 1 <&>

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  ok <- checkParallel $$(discover)
  when (not ok) exitFailure

prop_either :: Property
prop_either = withTests 1 $ property $ do
  let
    d :: Decoder (Either Text Integer) = (text <&> Left) <|> (integer <&> Right)

  [aesonQQ| "x"  |] ^? d === Just (Left "x")
  [aesonQQ| 5    |] ^? d === Just (Right 5)
  [aesonQQ| null |] ^? d === Nothing

prop_eitherTagged :: Property
prop_eitherTagged = withTests 1 $ property $ do
  let
    d :: Decoder (Either Integer Integer) =
      (at "type" (textIs "x") *> at "value" integer <&> Left) <|>
      (at "type" (textIs "y") *> at "value" integer <&> Right)

  [aesonQQ| {"type": "x", "value": 1} |] ^? d === Just (Left 1)
  [aesonQQ| {"type": "y", "value": 2} |] ^? d === Just (Right 2)
  [aesonQQ| {"type": "z", "value": 3} |] ^? d === Nothing

data Asset
  = Asset'Image Text
  | Asset'Video Text Text
  deriving (Eq, Show)

prop_asset :: Property
prop_asset = withTests 1 $ property $ do
  let
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

  json ^? d === Just
    [ Asset'Video "https://subscriber.typeclasses.com/video/js-operators-2/dash/manifest.mpd"
                  "/_/static/operators-video.jpg"
    , Asset'Image "/_/static/acme.jpg"
    ]

newtype Resource = Resource Text
  deriving (Eq, Show)

data StartTime = StartImmediately | StartTime POSIXTime
  deriving (Eq, Show)

newtype EndTime = EndTime POSIXTime
  deriving (Eq, Show)

data IpAddress = AnyIp | IpAddress Text
  deriving (Eq, Show)

data Policy =
  Policy
    { policyResource  :: Resource
    , policyStart     :: StartTime
    , policyEnd       :: EndTime
    , policyIpAddress :: IpAddress
    }
    deriving (Eq, Show)

prop_cloudFrontPolicy :: Property
prop_cloudFrontPolicy = withTests 1 $ property $ do
  let
    json =
      [aesonQQ|
        {
          "Statement": [
              {
                 "Resource": "http://d111111abcdef8.cloudfront.net/game_download.zip",
                 "Condition": {
                    "IpAddress": {"AWS:SourceIp": "192.0.2.0/24"},
                    "DateLessThan": {"AWS:EpochTime": 1357034400}
                 }
              }
           ]
        }
        |]

    d'time :: Decoder POSIXTime = at "AWS:EpochTime" integer <&> fromInteger

    d :: Decoder Policy =
      at ("Statement" <> only) $ do

        res   <- Resource <$> at "Resource" text

        start <- maybe StartImmediately StartTime <$>
                 (optional $ at ("Condition" <> "DateGreaterThan") d'time)

        end   <- EndTime <$> at ("Condition" <> "DateLessThan") d'time

        ip    <- maybe AnyIp IpAddress <$>
                 (optional $ at ("Condition" <> "IpAddress")
                                (at "AWS:SourceIp" text))

        pure $ Policy res start end ip

    p =
      Policy
        (Resource "http://d111111abcdef8.cloudfront.net/game_download.zip")
        StartImmediately
        (EndTime 1357034400)
        (IpAddress "192.0.2.0/24")

  json ^? d === Just p
