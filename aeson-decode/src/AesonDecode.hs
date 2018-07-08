{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module AesonDecode
  (
  -- * Decoder
    Decoder (..), constDecoder, constSuccessDecoder, failDecoder
  , mapDecoder, apDecoder, composeDecoderFunctions, orElse, defaultDecoder, is
  -- * Path
  , Path (..), here, stringPath, textPath, at, only
  -- * Text
  , text, textIs
  -- * Integer
  , integer, integerIs
  -- * Boolean
  , bool, boolIs, true, false
  -- * List
  , listOf
  -- * Vector
  , vectorOf
  -- * Ord map
  , ordMapOf
  -- * Hash map
  , hashMapOf
  -- * Null
  , null, nullable

  ) where

-- aeson
import           Data.Aeson       (FromJSON, Value (Object, Array, Null))
import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as Aeson

-- base
import Control.Applicative (Alternative ((<|>), empty))
import Control.Monad       (guard, (>=>))
import Data.Foldable       (toList)
import Data.Semigroup      (Semigroup ((<>)))
import Data.String         (IsString (fromString))
import Prelude             hiding (null)

-- containers
import           Data.Map (Map)
import qualified Data.Map as Map

-- data-default-class
import Data.Default.Class (Default (def))

-- text
import           Data.Text (Text)
import qualified Data.Text as Text

-- unordered-containers
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

-- vector
import Data.Vector (Vector)

-- $setup
--
-- >>> import Data.Aeson.QQ (aesonQQ)


--------------------------------------------------------------------------------
--  Decoder
--------------------------------------------------------------------------------

-- | A 'Decoder' is some way of interpreting a JSON value, with the
-- possibility of failure for some values.

newtype Decoder a = Decoder { decodeMaybe :: Value -> Maybe a }

-- | @'fmap' = 'mapDecoder'@

instance Functor Decoder where fmap = mapDecoder

-- |
-- @'pure' = 'constSuccessDecoder'@
--
-- @('<*>') = 'apDecoder'@

instance Applicative Decoder where pure = constSuccessDecoder; (<*>) = apDecoder

-- |
-- @('>=>') = 'composeDecoderFunctions'@

instance Monad Decoder where d >>= f = composeDecoderFunctions f (const d) ()

-- |
-- @'empty' = 'failDecoder'@
--
-- @('<|>') = 'orElse'@

instance Alternative Decoder where empty = failDecoder; (<|>) = orElse

-- | @'def' = 'defaultDecoder'@

instance FromJSON a => Default (Decoder a) where def = defaultDecoder

-- | A decoder that always produces the same result, no matter what JSON
-- it is given as input.

constDecoder
  :: Maybe a    -- ^ The result that the decoder always produces
  -> Decoder a  -- ^ A decoder that always produces the given result
constDecoder x = Decoder (const x)

-- | A decoder that always succeeds and produces the same value, no matter
-- what JSON it is given as input.
--
-- ==== Examples
--
-- >>> decodeMaybe (constSuccessDecoder 6) undefined
-- Just 6

constSuccessDecoder :: a -> Decoder a
constSuccessDecoder x = constDecoder (Just x)

-- | A decoder that always fails, no matter what JSON it is given as input.
--
-- This is the identity of the 'Alternative' for 'Decoder'.
--
-- ==== Examples
--
-- >>> decodeMaybe failDecoder undefined
-- Nothing

failDecoder :: Decoder a
failDecoder = constDecoder Nothing

-- |
-- ==== Examples
--
-- >>> decodeMaybe (mapDecoder (+ 1) integer) [aesonQQ| 4 |]
-- Just 5
--
-- >>> decodeMaybe (mapDecoder (+ 1) integer) [aesonQQ| "x" |]
-- Nothing

mapDecoder :: (a -> b) -> Decoder a -> Decoder b
mapDecoder f (Decoder d) = Decoder ((fmap . fmap) f d)

-- |
-- ==== Examples
--
-- >>> d = (,) `mapDecoder` (at "x" integer) `apDecoder` (at "y" text)
--
-- >>> decodeMaybe d [aesonQQ| {"x": 4, "y": "abc"} |]
-- Just (4,"abc")

apDecoder :: Decoder (a -> b) -> Decoder a -> Decoder b
apDecoder (Decoder ff) (Decoder fx) = Decoder $ \v ->
  ff v >>= \f -> fx v >>= \x -> Just (f x)

-- | Compose two decoder-producing functions.
--
-- ==== Examples
--
-- >>> f x = at (textPath x) text
-- >>> d = composeDecoderFunctions f f "a"
--
-- >>> decodeMaybe d [aesonQQ| {"a": "b", "b": "c"} |]
-- Just "c"

composeDecoderFunctions
  :: (b -> Decoder c)
  -> (a -> Decoder b)
  -> (a -> Decoder c)
composeDecoderFunctions f g a =
  Decoder $ \v ->
    case decodeMaybe (g a) v of
      Nothing -> Nothing
      Just b -> decodeMaybe (f b) v

-- |
-- ==== Examples
--
-- >>> d = orElse (Left <$> text) (Right <$> integer)
--
-- >>> decodeMaybe d [aesonQQ| 4 |]
-- Just (Right 4)
--
-- >>> decodeMaybe d [aesonQQ| "x" |]
-- Just (Left "x")
--
-- >>> decodeMaybe d [aesonQQ| null |]
-- Nothing

orElse :: Decoder a -> Decoder a -> Decoder a
orElse (Decoder a) (Decoder b) = Decoder $ \v ->
  a v <|> b v

-- |
-- ==== Examples
--
-- >>> decodeMaybe defaultDecoder [aesonQQ| 4 |] :: Maybe Integer
-- Just 4
--
-- >>> decodeMaybe defaultDecoder [aesonQQ| "x" |] :: Maybe String
-- Just "x"
--
-- >>> decodeMaybe defaultDecoder [aesonQQ| [4,5,6] |] :: Maybe [Integer]
-- Just [4,5,6]

defaultDecoder :: FromJSON a => Decoder a
defaultDecoder = Decoder $ \v -> Aeson.parseMaybe Aeson.parseJSON v

-- | @'is' x@ produces @'Just' ()@ if the JSON value decodes to @x@,
-- or 'Nothing' otherwise.
--
-- ==== Examples
--
-- >>> decodeMaybe (is 4) [aesonQQ| 4 |]
-- Just ()
--
-- >>> decodeMaybe (is 4) [aesonQQ| 5 |]
-- Nothing

is :: (Eq a, FromJSON a) => a -> Decoder ()
is x = defaultDecoder >>= \y -> guard (x == y)


--------------------------------------------------------------------------------
--  Path
--------------------------------------------------------------------------------

newtype Path = Path { getAt :: Value -> Maybe Value }

-- | ('<>') = 'pathConcat'@

instance Semigroup Path where (<>) = pathConcat

-- | @'mempty' = 'here'@
--
-- @'mappend' = 'pathConcat'@

instance Monoid Path where mappend = pathConcat; mempty = here

-- | @'fromString' = 'stringPath'@

instance IsString Path where fromString = stringPath

-- | The empty path.
--
-- This is the identity of the 'Monoid' for 'Path'.

here :: Path
here = Path Just

stringPath :: String -> Path
stringPath x = textPath (Text.pack x)

textPath :: Text -> Path
textPath x = Path $ \case
  Object m -> HashMap.lookup x m
  _ -> Nothing

pathConcat :: Path -> Path -> Path
pathConcat (Path a) (Path b) = Path (a >=> b)

at :: Path -> Decoder a -> Decoder a
at (Path f1) (Decoder f2) = Decoder (f1 >=> f2)

-- | Selects the only element from an array of length 1.
--
-- ==== Examples
--
-- >>> decodeMaybe (at only integer) [aesonQQ| [] |]
-- Nothing
--
-- >>> decodeMaybe (at only integer) [aesonQQ| [4] |]
-- Just 4
--
-- >>> decodeMaybe (at only integer) [aesonQQ| [4,5] |]
-- Nothing

only :: Path
only = Path $ \case
  Array (toList -> [x]) -> Just x
  _ -> Nothing


--------------------------------------------------------------------------------
--  Text
--------------------------------------------------------------------------------

-- | @'is' x@ produces @'Just' ()@ if the JSON value is the value @null@,
-- or 'Nothing' otherwise.
--
-- ==== Examples
--
-- >>> decodeMaybe null [aesonQQ| null |]
-- Just ()
--
-- >>> decodeMaybe null [aesonQQ| [] |]
-- Nothing
--
-- >>> decodeMaybe null [aesonQQ| 4 |]
-- Nothing

null :: Decoder ()
null = Decoder $ \case
  Null -> Just ()
  _ -> Nothing

-- | @'nullable' d@:
--
-- * Succeeds with @'Just' x@ if the decoder @d@ succeeds with value @x@.
-- * Succeeds with 'Nothing' if the JSON value is null.
-- * Fails otherwise.
--
-- ==== Examples
--
-- >>> decodeMaybe (nullable integer) [aesonQQ| 4 |]
-- Just (Just 4)
--
-- >>> decodeMaybe (nullable integer) [aesonQQ| null |]
-- Just Nothing
--
-- >>> decodeMaybe (nullable integer) [aesonQQ| "x" |]
-- Nothing

nullable :: Decoder a -> Decoder (Maybe a)
nullable d = (Just <$> d) <|> (Nothing <$ null)


--------------------------------------------------------------------------------
--  Text
--------------------------------------------------------------------------------

-- | Decodes a JSON string as 'Text'.
--
-- ==== Examples
--
-- >>> decodeMaybe text [aesonQQ| "x" |]
-- Just "x"
--
-- >>> decodeMaybe text [aesonQQ| 4 |]
-- Nothing

text :: Decoder Text
text = defaultDecoder

-- | @'is' x@ produces @'Just' ()@ if the JSON value is the string @x@,
-- or 'Nothing' otherwise.
--
-- ==== Examples
--
-- >>> decodeMaybe (textIs "x") [aesonQQ| "x" |]
-- Just ()
--
-- >>> decodeMaybe (textIs "x") [aesonQQ| "a" |]
-- Nothing

textIs :: Text -> Decoder ()
textIs = is


--------------------------------------------------------------------------------
--  Integer
--------------------------------------------------------------------------------

-- | Decodes a JSON number as an 'Integer'.
--
-- ==== Examples
--
-- >>> decodeMaybe integer [aesonQQ| 4 |]
-- Just 4
--
-- >>> decodeMaybe integer [aesonQQ| "x" |]
-- Nothing

integer :: Decoder Integer
integer = defaultDecoder

-- | @'is' x@ produces @'Just' ()@ if the JSON value is the integer @x@,
-- or 'Nothing' otherwise.
--
-- ==== Examples
--
-- >>> decodeMaybe (integerIs 4) [aesonQQ| 4 |]
-- Just ()
--
-- >>> decodeMaybe (integerIs 4) [aesonQQ| 5 |]
-- Nothing

integerIs :: Integer -> Decoder ()
integerIs = is


--------------------------------------------------------------------------------
--  Boolean
--------------------------------------------------------------------------------

-- | Decodes a JSON boolean as a 'Bool'.
--
-- ==== Examples
--
-- >>> decodeMaybe bool [aesonQQ| true |]
-- Just True
--
-- >>> decodeMaybe bool [aesonQQ| false |]
-- Just False
--
-- >>> decodeMaybe bool [aesonQQ| "x" |]
-- Nothing

bool :: Decoder Bool
bool = defaultDecoder

-- | @'is' x@ produces @'Just' ()@ if the JSON value is the boolean @x@,
-- or 'Nothing' otherwise.
--
-- ==== Examples
--
-- >>> decodeMaybe (boolIs True) [aesonQQ| true |]
-- Just ()
--
-- >>> decodeMaybe (boolIs True) [aesonQQ| false |]
-- Nothing

boolIs :: Bool -> Decoder ()
boolIs = is

-- | @'is' x@ produces @'Just' ()@ if the JSON value is the value @true@,
-- or 'Nothing' otherwise.
--
-- ==== Examples
--
-- >>> decodeMaybe true [aesonQQ| true |]
-- Just ()
--
-- >>> decodeMaybe true [aesonQQ| false |]
-- Nothing

true :: Decoder ()
true = is True

-- | @'is' x@ produces @'Just' ()@ if the JSON value is the value @false@,
-- or 'Nothing' otherwise.
--
-- ==== Examples
--
-- >>> decodeMaybe false [aesonQQ| false |]
-- Just ()
--
-- >>> decodeMaybe false [aesonQQ| true |]
-- Nothing

false :: Decoder ()
false = is False


--------------------------------------------------------------------------------
--  Vector
--------------------------------------------------------------------------------

-- |
-- ==== Examples
--
-- >>> decodeMaybe (vectorOf integer) [aesonQQ| [] |]
-- Just []
--
-- >>> decodeMaybe (vectorOf integer) [aesonQQ| [4,5,6] |]
-- Just [4,5,6]
--
-- >>> decodeMaybe (vectorOf integer) [aesonQQ| ["4","5","6"] |]
-- Nothing

vectorOf :: Decoder a -> Decoder (Vector a)
vectorOf d = Decoder $ \case
  Array xs -> traverse (decodeMaybe d) xs
  _ -> Nothing


--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

-- |
-- ==== Examples
--
-- >>> decodeMaybe (listOf integer) [aesonQQ| [] |]
-- Just []
--
-- >>> decodeMaybe (listOf integer) [aesonQQ| [4,5,6] |]
-- Just [4,5,6]
--
-- >>> decodeMaybe (listOf integer) [aesonQQ| ["4","5","6"] |]
-- Nothing

listOf :: Decoder a -> Decoder [a]
listOf d = toList <$> vectorOf d


--------------------------------------------------------------------------------
--  Hash map
--------------------------------------------------------------------------------

-- |
-- ==== Examples
--
-- >>> decodeMaybe (hashMapOf integer) [aesonQQ| {} |]
-- Just (fromList [])
--
-- >>> decodeMaybe (hashMapOf integer) [aesonQQ| {"a": 4, "b": 5} |]
-- Just (fromList [("a",4),("b",5)])
--
-- >>> decodeMaybe (hashMapOf integer) [aesonQQ| 4 |]
-- Nothing

hashMapOf :: Decoder a -> Decoder (HashMap Text a)
hashMapOf d = Decoder $ \case
  Object xs -> traverse (decodeMaybe d) xs
  _ -> Nothing


--------------------------------------------------------------------------------
--  Ord map
--------------------------------------------------------------------------------

-- |
-- ==== Examples
--
-- >>> decodeMaybe (ordMapOf integer) [aesonQQ| {} |]
-- Just (fromList [])
--
-- >>> decodeMaybe (ordMapOf integer) [aesonQQ| {"a": 4, "b": 5} |]
-- Just (fromList [("a",4),("b",5)])
--
-- >>> decodeMaybe (ordMapOf integer) [aesonQQ| 4 |]
-- Nothing

ordMapOf :: Decoder a -> Decoder (Map Text a)
ordMapOf d = Map.fromList . HashMap.toList <$> hashMapOf d
