module Entities.Association where

import Maybe.Extra

type alias Id = String

type alias Entity a =
  { a | id : Id }

type alias Association a b =
  { accessor : Entity a -> Id
  , children : List (Entity a)
  , parents : List (Entity b)
  }

init : (Entity a -> Id) -> List (Entity a) -> List (Entity b) -> Association a b
init accessor children parents =
  { accessor = accessor
  , children = children
  , parents = parents
  }

parentOf : Id -> Association a b -> Maybe (Entity b)
parentOf id association =
  Maybe.map association.accessor (entityOf id association.children)
  |> Maybe.map (\id -> entityOf id association.parents)
  |> Maybe.Extra.join

childrenOf : Id -> Association a b -> List (Entity a)
childrenOf id association =
  List.filter (\item -> association.accessor item == id) association.children

entityOf : Id -> List (Entity a) -> Maybe (Entity a)
entityOf id items =
  List.filter (\item -> item.id == id) items
  |> List.head
