module PostEncoderWithOptions exposing (..)

import CommentEncoder exposing (..)
import Json.Encode as E
import PostType exposing (..)


encodePost : Post -> E.Value
encodePost x =
    E.object
        [ ( "postId", E.int x.id )
        , ( "postName", E.string x.name )
        , ( "postAge", (Maybe.withDefault E.null << Maybe.map E.float) x.age )
        , ( "postComments", (E.list << List.map encodeComment) x.comments )
        , ( "postPromoted", (Maybe.withDefault E.null << Maybe.map encodeComment) x.promoted )
        , ( "postAuthor", (Maybe.withDefault E.null << Maybe.map E.string) x.author )
        ]
