module Layer.Layer where

import Data.List

type Layer s i = s -> [i] -> s

constructLayer :: (lay2 -> lay1 -> msg1 -> (lay1, [msg2])) ->
                  (lay2 -> lay1 -> lay1) ->
                  Layer lay2 msg2 ->
                  Layer (lay1, lay2) msg1
constructLayer inputf stepf nextLayer (lay1, lay2) msg1s =
        let (lay1', msgs) = mapAccumL (inputf lay2) lay1 msg1s
            lay2' = nextLayer lay2 (concat msgs)
            lay1'' = stepf lay2' lay1' in (lay1'', lay2')
