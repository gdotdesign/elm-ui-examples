module Ext.Signal where

import Signal

(>>>) : Signal.Address b -> (a -> b) -> Signal.Address a
(>>>) a b =
  Signal.forwardTo a b
