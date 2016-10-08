module CommentEncoderWithOptions exposing (..)

import CommentType exposing (..)
import Exts.Date exposing (..)
import Exts.Json.Encode exposing (..)
import Json.Encode as E


encodeComment : Comment -> E.Value
encodeComment x =
    E.object
        [ ( "commentPostId", E.int x.postId )
        , ( "commentText", E.string x.text )
        , ( "commentMainCategories", (E.tuple2 E.string E.string) x.mainCategories )
        , ( "commentPublished", E.bool x.published )
        , ( "commentCreated", (E.string << toISOString) x.created )
        , ( "commentTags", (E.dict E.string E.int) x.tags )
        ]
