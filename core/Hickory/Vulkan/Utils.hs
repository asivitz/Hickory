module Hickory.Vulkan.Utils where

import Control.Monad
import Control.Monad.Managed


-- |Acquire resources and loop. User function may signal:
-- Nothing    -> Exit loop
-- Just False -> Require resources and loop
-- Just True  -> Loop
loopWithResourceRefresh :: Managed a -> (Int -> a -> IO (Maybe Bool)) -> IO ()
loopWithResourceRefresh acquireResource f = main 0
  where
  main n = do
    outer n >>= \case
      Just n' -> void $ main (n' + 1)
      Nothing -> pure ()
  outer n = do
    with acquireResource \resource -> do
      let inner n' = do
            f n' resource >>= \case
              Just True  -> inner (n' + 1)
              Just False -> pure (Just n')
              Nothing    -> pure Nothing
      inner n
