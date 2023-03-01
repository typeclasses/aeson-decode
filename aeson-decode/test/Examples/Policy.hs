module Examples.Policy (spec) where

import AesonDecode
import Essentials

import Control.Applicative (optional)
import Data.Aeson (Value)
import Data.Aeson.QQ (aesonQQ)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Prelude (fromInteger)
import Test.Hspec (it, shouldBe, Spec)

newtype Resource = Resource Text deriving (Eq, Show)

data StartTime = StartImmediately | StartTime POSIXTime deriving (Eq, Show)

newtype EndTime = EndTime POSIXTime deriving (Eq, Show)

data IpAddress = AnyIp | IpAddress Text deriving (Eq, Show)

data Policy = Policy
    { policyResource  :: Resource
    , policyStart     :: StartTime
    , policyEnd       :: EndTime
    , policyIpAddress :: IpAddress
    }
    deriving (Eq, Show)

spec :: Spec
spec = it "policy example" $ (json ^? d) `shouldBe` Just p

json :: Value
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

d'time :: Decoder POSIXTime
d'time :: Decoder POSIXTime = at "AWS:EpochTime" integer <&> fromInteger

d :: Decoder Policy
d :: Decoder Policy = at ("Statement" <> only) $ do

    res   <- Resource <$> at "Resource" text

    start <- maybe StartImmediately StartTime <$>
            (optional $ at ("Condition" <> "DateGreaterThan") d'time)

    end   <- EndTime <$> at ("Condition" <> "DateLessThan") d'time

    ip    <- maybe AnyIp IpAddress <$>
            (optional $ at ("Condition" <> "IpAddress")
                            (at "AWS:SourceIp" text))

    pure $ Policy res start end ip

p :: Policy
p = Policy
    (Resource "http://d111111abcdef8.cloudfront.net/game_download.zip")
    StartImmediately
    (EndTime 1357034400)
    (IpAddress "192.0.2.0/24")
