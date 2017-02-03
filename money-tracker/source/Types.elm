module Types exposing (..)

{-| Types that are used along the application.
-}
import Json.Decode as Json exposing (field)
import Json.Decode.Extra as JsonExtra
import Json.Encode as J

import Date.Extra.Config.Configs as DateConfigs
import Date.Extra.Format exposing (format)
import Date

{-| Encodes a date object.
-}
encodeDate : Date.Date -> Json.Value
encodeDate date =
  format (DateConfigs.getConfig "en_us") "%Y-%m-%d" date
    |> J.string


{-| Representation of a transaction.
-}
type alias Transaction =
  { id : String
  , amount : Int
  , comment : String
  , categoryId : String
  , accountId : String
  , date : Date.Date
  }


{-| Decodes a transaction.
-}
decodeTransaction : Json.Decoder Transaction
decodeTransaction =
  Json.map6
    Transaction
    (field "id" Json.string)
    (field "amount" Json.int)
    (field "comment" Json.string)
    (field "categoryId" Json.string)
    (field "accountId" Json.string)
    (field "date" JsonExtra.date)


{-| Encodes a transaction record.
-}
encodeTransaction : Transaction -> Json.Value
encodeTransaction transaction =
  J.object
    [ ( "id", J.string transaction.id )
    , ( "amount", J.int transaction.amount )
    , ( "comment", J.string transaction.comment )
    , ( "categoryId", J.string transaction.categoryId )
    , ( "accountId", J.string transaction.accountId )
    , ( "date", encodeDate transaction.date )
    ]


{-| Representation of a category.
-}
type alias Category =
  { icon : String
  , name : String
  , id : String
  }


{-| Decodes a category.
-}
decodeCategory : Json.Decoder Category
decodeCategory =
  Json.map3
    Category
    (field "icon" Json.string)
    (field "name" Json.string)
    (field "id" Json.string)


{-| Encodes a category record.
-}
encodeCategory : Category -> Json.Value
encodeCategory category =
  J.object
    [ ( "icon", J.string category.icon )
    , ( "name", J.string category.name )
    , ( "id", J.string category.id )
    ]


{-| Representation of an account.
-}
type alias Account =
  { initialBalance : Int
  , name : String
  , icon : String
  , id : String
  }


{-| Decodes an account.
-}
decodeAccount : Json.Decoder Account
decodeAccount =
  Json.map4
    Account
    (field "initialBalance" Json.int)
    (field "name" Json.string)
    (field "icon" Json.string)
    (field "id" Json.string)


{-| Encodes an account record.
-}
encodeAccount : Account -> Json.Value
encodeAccount account =
  J.object
    [ ( "initialBalance", J.int account.initialBalance )
    , ( "name", J.string account.name )
    , ( "icon", J.string account.icon )
    , ( "id", J.string account.id )
    ]


{-| Representation of a users session data.
-}
type alias Store =
  { transactions : List Transaction
  , categories : List Category
  , accounts : List Account
  , settings : Settings
  }


{-| Representation of a settings object.
-}
type alias Settings =
  { prefix : String
  , affix : String
  }


{-| Decodes a settings.
-}
decodeSettings : Json.Decoder Settings
decodeSettings =
  Json.map2
    Settings
    (field "prefix" Json.string)
    (field "affix" Json.string)


{-| Encodes a settings record.
-}
encodeSettings : Settings -> Json.Value
encodeSettings settings =
  J.object
    [ ( "prefix", J.string settings.prefix )
    , ( "affix", J.string settings.affix )
    ]


{-| Decodes a store.
-}
decodeStore : Json.Decoder Store
decodeStore =
  Json.map4
    Store
    (field "transactions" (Json.list decodeTransaction))
    (field "categories" (Json.list decodeCategory))
    (field "accounts" (Json.list decodeAccount))
    (field "settings" decodeSettings)


{-| Encodes a store-
-}
encodeStore : Store -> Json.Value
encodeStore store =
  J.object
    [ ( "transactions", J.list (List.map encodeTransaction store.transactions) )
    , ( "categories", J.list (List.map encodeCategory store.categories) )
    , ( "accounts", J.list (List.map encodeAccount store.accounts) )
    , ( "settings", encodeSettings store.settings )
    ]


{-| Updates a stores settings.
-}
updateStoreSettings : Settings -> Store -> Store
updateStoreSettings settings store =
  { store | settings = settings }
