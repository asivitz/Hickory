{- Reading/writing config files
 - Supports default values, so we don't error out if a field is missing
-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Hickory.Config where

import GHC.Generics
import qualified Data.Yaml as Yaml
import Data.Yaml (Value (..), FromJSON, withObject, (.:?))
import Data.Default (Default(..))
import Data.Text (pack, Text)
import Data.ByteString (ByteString, readFile)
import Data.Aeson.Key (fromText)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)

parseYamlDefault :: (Generic a, GFromYamlDefault (Rep a)) => ByteString -> Either String a
parseYamlDefault bs = do
  value <- first show $ Yaml.decodeEither' bs
  Yaml.parseEither parseJSONWithDefault value

readYamlDefault :: (Generic a, GFromYamlDefault (Rep a)) => FilePath -> IO (Either String a)
readYamlDefault = fmap parseYamlDefault . Data.ByteString.readFile

class FromYamlDefault a where
  parseJSONWithDefault :: Value -> Yaml.Parser a

  default parseJSONWithDefault ::
    ( Generic a
    , GFromYamlDefault (Rep a)
    ) => Value -> Yaml.Parser a
  parseJSONWithDefault val = to <$> gParseJsonWithDefault val

instance
  ( Generic a
  , GFromYamlDefault (Rep a)
  ) => FromYamlDefault a

-- Generic machinery to parse using yaml instances with the added feature
-- that missing fields are filled with default values (from Data.Default)
class GFromYamlDefault f where
  gParseJsonWithDefault :: Value -> Yaml.Parser (f p)

instance GFromYamlDefault U1 where
  gParseJsonWithDefault _ = pure U1

instance (GFromYamlDefault a, GFromYamlDefault b) => GFromYamlDefault (a :*: b) where
  gParseJsonWithDefault val = do
    aVal <- gParseJsonWithDefault val
    bVal <- gParseJsonWithDefault val
    return (aVal :*: bVal)

-- Metadata for the whole datatype (M1 D)
instance GFromYamlDefault f => GFromYamlDefault (M1 D d f) where
  gParseJsonWithDefault val = M1 <$> gParseJsonWithDefault val

-- Metadata for a constructor (M1 C)
instance GFromYamlDefault f => GFromYamlDefault (M1 C c f) where
  gParseJsonWithDefault val = M1 <$> gParseJsonWithDefault val

-- Metadata for a selector (i.e. a single field) (M1 S)
instance (Selector s, Default c, FromJSON c) => GFromYamlDefault (M1 S s (K1 i c)) where
  gParseJsonWithDefault = withObject "record" \obj -> do
    let fieldName :: Text
        fieldName = pack (selName (undefined :: M1 S s (K1 i c) p))

    mVal <- obj .:? fromText fieldName
    return $ M1 (K1 (fromMaybe def mVal))
