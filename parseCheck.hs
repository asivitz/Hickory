import Data.List

ignore = ["Warning: the following files",
         "ghc: no input files",
         "Usage: "]

doParse :: String -> String
doParse = unlines . f . lines
    where f = filter (\x -> not . or $ map (\y -> (y `isPrefixOf` x)) ignore)

main = interact doParse

