{- Reading/writing config files
 - Supports default values, so we don't error out if a field is missing
-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Hickory.Config where

import GHC.Generics
import qualified Data.Yaml as Yaml
import Data.Yaml (Value (..), withObject, (.:?))
import Data.Default (Default(..))
import Data.Text (pack, Text)
import Data.ByteString (ByteString, readFile)
import Data.Aeson.Key (fromText)
import Data.Bifunctor (first)

parseYamlDefault :: FromYamlDefault a => ByteString -> Either String a
parseYamlDefault bs = do
  value <- first show $ Yaml.decodeEither' bs
  Yaml.parseEither parseJSONWithDefault value

readYamlDefault :: FromYamlDefault a => FilePath -> IO (Either String a)
readYamlDefault = fmap parseYamlDefault . Data.ByteString.readFile

class FromYamlDefault a where
  parseJSONWithDefault :: Value -> Yaml.Parser a

  default parseJSONWithDefault ::
    ( Generic a
    , GFromYamlDefault (Rep a)
    ) => Value -> Yaml.Parser a
  parseJSONWithDefault val = to <$> gParseJsonWithDefault val

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
    pure (aVal :*: bVal)

-- Metadata for the whole datatype (M1 D)
instance GFromYamlDefault f => GFromYamlDefault (M1 D d f) where
  gParseJsonWithDefault val = M1 <$> gParseJsonWithDefault val

-- Metadata for a constructor (M1 C)
instance GFromYamlDefault f => GFromYamlDefault (M1 C c f) where
  gParseJsonWithDefault val = M1 <$> gParseJsonWithDefault val

-- Metadata for a selector (i.e. a single field) (M1 S)
instance (Selector s, Default c, FromYamlDefault c) => GFromYamlDefault (M1 S s (K1 i c)) where
  gParseJsonWithDefault = withObject "record" \obj -> do
    let fieldName :: Text
        fieldName = pack (selName (undefined :: M1 S s (K1 i c) p))

    mv <- obj .:? fromText fieldName
    v <- case mv of
      Just v -> parseJSONWithDefault v
      Nothing -> pure def
    pure $ M1 (K1 v)
