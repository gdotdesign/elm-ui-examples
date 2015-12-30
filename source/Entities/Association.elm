module Entities.Association where

import Maybe.Extra

type alias Id = String

type alias Entity a =
  { a | id : Id }

type alias Association a b =
  { accessor : a -> Id
  , children : List a
  , parents : List b
  }

init : (a -> Id) -> List (a) -> List (b) -> Association a b
init accessor children parents =
  { accessor = accessor
  , children = children
  , parents = parents
  }

parentOf : Id -> Association { a | id : Id } { b | id : Id }
           -> Maybe { b | id : Id }
parentOf id association =
  Maybe.map association.accessor (entityOf id association.children)
  |> Maybe.map (\id -> entityOf id association.parents)
  |> Maybe.Extra.join

childrenOf : Id -> Association { a | id : Id } { b | id : Id }
             -> List { a | id : Id }
childrenOf id association =
  List.filter (\item -> association.accessor item == id) association.children

entityOf : Id -> List { a | id : Id } -> Maybe { a | id : Id }
entityOf id items =
  List.filter (\item -> item.id == id) items
  |> List.head
