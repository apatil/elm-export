module CommentEncoder exposing (..)

import CommentType exposing (..)
import Exts.Date exposing (..)
import Exts.Json.Encode exposing (..)
import Json.Encode as E


encodeComment : Comment -> E.Value
encodeComment x =
    E.object
        [ ( "postId", E.int x.postId )
        , ( "text", E.string x.text )
        , ( "mainCategories", (E.tuple2 E.string E.string) x.mainCategories )
        , ( "published", E.bool x.published )
        , ( "created", (E.string << toISOString) x.created )
        , ( "tags", (E.dict E.string E.int) x.tags )
        ]
