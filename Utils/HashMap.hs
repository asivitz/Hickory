module Utils.HashMap where

import qualified Data.HashMap.Strict as Hash
import Data.Maybe
import Data.Hashable

type HM a b = Hash.HashMap a b

zipHashes2 :: (Hashable a, Eq a) => HM a b -> HM a c -> [(a, b, c)]
zipHashes2 hb hc = let f = \(k, v) -> do
                        c <- Hash.lookup k hc
                        return (k, v, c)
                       mlist = map f (Hash.toList hb) in
                           catMaybes mlist

zipHashes3 :: (Hashable a, Eq a) => HM a b -> HM a c -> HM a d -> [(a, b, c, d)]
zipHashes3 hb hc hd = let f = \(k, v) -> do
                            c <- Hash.lookup k hc
                            d <- Hash.lookup k hd
                            return (k, v, c, d)
                          mlist = map f (Hash.toList hb) in
                              catMaybes mlist

zipHashes4 :: (Hashable a, Eq a) => HM a b -> HM a c -> HM a d -> HM a e -> [(a, b, c, d, e)]
zipHashes4 hb hc hd he = let f = \(k, v) -> do
                                c <- Hash.lookup k hc
                                d <- Hash.lookup k hd
                                e <- Hash.lookup k he
                                return (k, v, c, d, e)
                             mlist = map f (Hash.toList hb) in
                                 catMaybes mlist
