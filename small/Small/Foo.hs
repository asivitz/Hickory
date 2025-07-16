{-# LANGUAGE QuasiQuotes #-}

module Small.Foo where

import Data.String.QM (qt)
import Data.Text (Text)

foo :: Int
foo = 1

bar :: Text
bar = [qt|
bartext
|]
