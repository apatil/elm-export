module PostEncoder exposing (..)

import CommentEncoder exposing (..)
import Json.Encode as E
import PostType exposing (..)


encodePost : Post -> E.Value
encodePost x =
    E.object
        [ ( "id", E.int x.id )
        , ( "name", E.string x.name )
        , ( "age", (Maybe.withDefault E.null << Maybe.map E.float) x.age )
        , ( "comments", (E.list << List.map encodeComment) x.comments )
        , ( "promoted", (Maybe.withDefault E.null << Maybe.map encodeComment) x.promoted )
        , ( "author", (Maybe.withDefault E.null << Maybe.map E.string) x.author )
        ]
