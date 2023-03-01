module AesonDecode
  (
      {- * Decoder -}
          Decoder (..), constDecoder, constSuccessDecoder, failDecoder,
          mapDecoder, apDecoder, composeDecoderFunctions, orElse,
          defaultDecoder, is, (^?),
      {- * Path -} Path (..), here, stringPath, textPath, at, only,
      {- * Text -} text, textIs,
      {- * Integer -} integer, integerIs,
      {- * Boolean -} bool, boolIs, true, false,
      {- * List -} listOf,
      {- * Vector -} vectorOf,
      {- * Ord map -} ordMapOf,
      {- * Hash map -} hashMapOf,
      {- * Null -} null, nullable,
  )
   where

import Essentials

import Control.Applicative (Alternative ((<|>), empty))
import Control.Monad (guard)
import Data.Aeson (FromJSON, Value (Object, Array, Null))
import Data.Default.Class (Default (def))
import Data.Foldable (toList)
import Data.HashMap.Lazy (HashMap)
import Data.Map (Map)
import Data.String (IsString (fromString), String)
import Data.Text (Text)
import Data.Vector (Vector)
import Prelude (Integer)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import qualified Data.Text as Text

(^?) :: Value -> Decoder a -> Maybe a
val ^? Decoder f = f val
infixl 8 ^?


--------------------------------------------------------------------------------
--  Decoder
--------------------------------------------------------------------------------

{-| Some way of interpreting a JSON value, with the
    possibility of failure for some values -}
newtype Decoder a = Decoder { decodeMaybe :: Value -> Maybe a }

{-| @'fmap' = 'mapDecoder'@ -}
instance Functor Decoder where
    fmap = mapDecoder

{-| @'pure' = 'constSuccessDecoder'@, @'<*>' = 'apDecoder'@ -}
instance Applicative Decoder where
    pure = constSuccessDecoder
    (<*>) = apDecoder

{-| @'>=>' = 'composeDecoderFunctions'@ -}
instance Monad Decoder where
    d >>= f = composeDecoderFunctions f (\_ -> d) ()

{-| @'empty' = 'failDecoder'@, @'<|>' = 'orElse'@ -}
instance Alternative Decoder where
    empty = failDecoder
    (<|>) = orElse

{-| @'def' = 'defaultDecoder'@ -}
instance FromJSON a => Default (Decoder a) where
    def = defaultDecoder

{- | Always produces the same result -}
constDecoder
  :: Maybe a    -- ^ The result that the decoder always produces
  -> Decoder a  -- ^ A decoder that always produces the given result
constDecoder x = Decoder (\_ -> x)

{-| Always succeeds and produces the same result -}
constSuccessDecoder :: a -> Decoder a
constSuccessDecoder x = constDecoder (Just x)

{-| Always fails

This is the identity of the 'Alternative' for 'Decoder'. -}
failDecoder :: Decoder a
failDecoder = constDecoder Nothing

mapDecoder :: (a -> b) -> Decoder a -> Decoder b
mapDecoder f (Decoder d) = Decoder ((fmap . fmap) f d)

apDecoder :: Decoder (a -> b) -> Decoder a -> Decoder b
apDecoder (Decoder ff) (Decoder fx) = Decoder $ \v ->
  ff v >>= \f -> fx v >>= \x -> Just (f x)

{-| Compose two decoder-producing functions -}
composeDecoderFunctions
  :: (b -> Decoder c)
  -> (a -> Decoder b)
  -> (a -> Decoder c)
composeDecoderFunctions f g a =
  Decoder $ \v ->
    case decodeMaybe (g a) v of
      Nothing -> Nothing
      Just b -> decodeMaybe (f b) v

orElse :: Decoder a -> Decoder a -> Decoder a
orElse (Decoder a) (Decoder b) = Decoder $ \v ->
  a v <|> b v

defaultDecoder :: FromJSON a => Decoder a
defaultDecoder = Decoder $ \v -> Aeson.parseMaybe Aeson.parseJSON v

{-| @'is' x@ produces @'Just' ()@ if the JSON value decodes to @x@,
    or 'Nothing' otherwise -}
is :: (Eq a, FromJSON a) => a -> Decoder ()
is x = defaultDecoder >>= \y -> guard (x == y)


--------------------------------------------------------------------------------
--  Path
--------------------------------------------------------------------------------

newtype Path = Path { getAt :: Value -> Maybe Value }

{-| '<>' = 'pathConcat'@ -}
instance Semigroup Path where
    (<>) = pathConcat

{-| @'mempty' = 'here'@ -}
instance Monoid Path where
    mempty = here

{-| @'fromString' = 'stringPath'@ -}
instance IsString Path where
    fromString = stringPath

{-| The empty path

This is the identity of the 'Monoid' for 'Path'. -}
here :: Path
here = Path Just

stringPath :: String -> Path
stringPath x = textPath (Text.pack x)

textPath :: Text -> Path
textPath x = Path $ \case
    Object m -> KeyMap.lookup (Key.fromText x) m
    _ -> Nothing

pathConcat :: Path -> Path -> Path
pathConcat (Path a) (Path b) = Path (a >=> b)

at :: Path -> Decoder a -> Decoder a
at (Path f1) (Decoder f2) = Decoder (f1 >=> f2)

{-| Selects the only element from an array of length 1 -}
only :: Path
only = Path $ \case
  Array (toList -> [x]) -> Just x
  _ -> Nothing


--------------------------------------------------------------------------------
--  Text
--------------------------------------------------------------------------------

{-| @'Just' ()@ if the JSON value is the value @null@, 'Nothing' otherwise -}
null :: Decoder ()
null = Decoder $ \case
  Null -> Just ()
  _ -> Nothing

{- | Succeeds with @'Just' x@ if the decoder @d@ succeeds with value @x@,
     succeeds with 'Nothing' if the JSON value is null, fails otherwise -}
nullable :: Decoder a -> Decoder (Maybe a)
nullable d = (Just <$> d) <|> (Nothing <$ null)


--------------------------------------------------------------------------------
--  Text
--------------------------------------------------------------------------------

{-| Decodes a JSON string as 'Text' -}
text :: Decoder Text
text = defaultDecoder

{-| @'Just' ()@ if the JSON value is the given string, 'Nothing' otherwise -}
textIs :: Text -> Decoder ()
textIs = is


--------------------------------------------------------------------------------
--  Integer
--------------------------------------------------------------------------------

{-| Decodes a JSON number as an 'Integer' -}
integer :: Decoder Integer
integer = defaultDecoder

{- | @'Just' ()@ if the JSON value is the given integer, 'Nothing' otherwise -}
integerIs :: Integer -> Decoder ()
integerIs = is


--------------------------------------------------------------------------------
--  Boolean
--------------------------------------------------------------------------------

{-| Decodes a JSON boolean as a 'Bool' -}
bool :: Decoder Bool
bool = defaultDecoder

{-| @'Just' ()@ if the JSON value is the given boolean, 'Nothing' otherwise -}
boolIs :: Bool -> Decoder ()
boolIs = is

{-| @'Just' ()@ if the JSON value is @true@, 'Nothing' otherwise -}
true :: Decoder ()
true = is True

{-| @'Just' ()@ if the JSON value is @false@, 'Nothing' otherwise -}
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
    Object xs -> traverse (decodeMaybe d) xs <&> KeyMap.toHashMapText
    _ -> Nothing


--------------------------------------------------------------------------------
--  Ord map
--------------------------------------------------------------------------------

ordMapOf :: Decoder a -> Decoder (Map Text a)
ordMapOf d = Map.fromList . HashMap.toList <$> hashMapOf d
