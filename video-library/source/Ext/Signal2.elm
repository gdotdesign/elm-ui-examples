module Ext.Signal2 where

import Signal

(>>>) : Signal.Address b -> (a -> b) -> Signal.Address a
(>>>) a b =
  Signal.forwardTo a b
