{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module AesonDecode
  (
  -- * Decoder
    Decoder (..), defaultDecoder, is
  -- * Path
  , Path (..), here, at, only
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
  , null

  ) where

-- aeson
import           Data.Aeson       (FromJSON, Value (..))
import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as Aeson

-- base
import Control.Applicative (Alternative (..))
import Control.Monad       (guard, (>=>))
import Data.Foldable       (toList)
import Data.Semigroup      (Semigroup (..))
import Data.String         (IsString (..))
import Prelude             hiding (null)

-- containers
import           Data.Map (Map)
import qualified Data.Map as Map

-- data-default
import qualified Data.Default as Def

-- text
import           Data.Text (Text)
import qualified Data.Text as Text

-- unordered-containers
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

-- vector
import Data.Vector (Vector)


--------------------------------------------------------------------------------
--  Decoder
--------------------------------------------------------------------------------

newtype Decoder a = Decoder { decodeMaybe :: Value -> Maybe a }

instance Functor Decoder
  where
    fmap f (Decoder d) = Decoder $ (fmap . fmap) f d

instance Applicative Decoder
  where
    pure x = Decoder $ (pure . pure) x
    Decoder ff <*> Decoder fx = Decoder $ \v ->
      ff v >>= \f -> fx v >>= \x -> Just (f x)

instance Monad Decoder
  where
    Decoder f >>= df = Decoder $ \v ->
      f v >>= \x -> let Decoder g = df x in g v

instance Alternative Decoder
  where
    empty = Decoder $ const Nothing
    Decoder a <|> Decoder b = Decoder $ \v -> a v <|> b v

instance FromJSON a => Def.Default (Decoder a)
  where
    def = defaultDecoder

defaultDecoder :: FromJSON a => Decoder a
defaultDecoder = Decoder $ \v -> Aeson.parseMaybe Aeson.parseJSON v

-- | @'is' x@ produces @'Just' ()@ if the JSON value decodes to @x@,
-- or 'Nothing' otherwise.

is :: (Eq a, FromJSON a) => a -> Decoder ()
is x = defaultDecoder >>= \y -> guard (x == y)


--------------------------------------------------------------------------------
--  Path
--------------------------------------------------------------------------------

newtype Path = Path { getAt :: Value -> Maybe Value }

instance Semigroup Path
  where
    Path a <> Path b = Path (a >=> b)

instance Monoid Path
  where
    mappend = (<>)
    mempty = here

instance IsString Path
  where
    fromString x = Path $ \case
      Object m -> HashMap.lookup (Text.pack x) m
      _ -> Nothing

-- | The empty path.

here :: Path
here = Path Just

at :: Path -> Decoder a -> Decoder a
at (Path f1) (Decoder f2) = Decoder (f1 >=> f2)

-- | Selects the only element from an array of length 1.

only :: Path
only = Path $ \case
  Array (toList -> [x]) -> Just x
  _ -> Nothing


--------------------------------------------------------------------------------
--  Text
--------------------------------------------------------------------------------

-- | @'is' x@ produces @'Just' ()@ if the JSON value is the value @null@,
-- or 'Nothing' otherwise.

null :: Decoder ()
null = Decoder $ \case
  Null -> Just ()
  _ -> Nothing


--------------------------------------------------------------------------------
--  Text
--------------------------------------------------------------------------------

-- | Decodes a JSON string as 'Text'.

text :: Decoder Text
text = defaultDecoder

-- | @'is' x@ produces @'Just' ()@ if the JSON value is the string @x@,
-- or 'Nothing' otherwise.

textIs :: Text -> Decoder ()
textIs = is


--------------------------------------------------------------------------------
--  Integer
--------------------------------------------------------------------------------

-- | Decodes a JSON number as an 'Integer'.

integer :: Decoder Integer
integer = defaultDecoder

-- | @'is' x@ produces @'Just' ()@ if the JSON value is the integer @x@,
-- or 'Nothing' otherwise.

integerIs :: Integer -> Decoder ()
integerIs = is


--------------------------------------------------------------------------------
--  Boolean
--------------------------------------------------------------------------------

-- | Decodes a JSON boolean as a 'Bool'.

bool :: Decoder Bool
bool = defaultDecoder

-- | @'is' x@ produces @'Just' ()@ if the JSON value is the boolean @x@,
-- or 'Nothing' otherwise.

boolIs :: Bool -> Decoder ()
boolIs = is

-- | @'is' x@ produces @'Just' ()@ if the JSON value is the value @true@,
-- or 'Nothing' otherwise.

true :: Decoder ()
true = is True

-- | @'is' x@ produces @'Just' ()@ if the JSON value is the value @false@,
-- or 'Nothing' otherwise.

false :: Decoder ()
false = is False


--------------------------------------------------------------------------------
--  Vector
--------------------------------------------------------------------------------

vectorOf :: Decoder a -> Decoder (Vector a)
vectorOf d = Decoder $ \case
  Array xs -> traverse (decodeMaybe d) xs
  _ -> Nothing


--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

listOf :: Decoder a -> Decoder [a]
listOf d = toList <$> vectorOf d


--------------------------------------------------------------------------------
--  Hash map
--------------------------------------------------------------------------------

hashMapOf :: Decoder a -> Decoder (HashMap Text a)
hashMapOf d = Decoder $ \case
  Object xs -> traverse (decodeMaybe d) xs
  _ -> Nothing


--------------------------------------------------------------------------------
--  Ord map
--------------------------------------------------------------------------------

ordMapOf :: Decoder a -> Decoder (Map Text a)
ordMapOf d = Map.fromList . HashMap.toList <$> hashMapOf d
