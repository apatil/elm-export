{-# LANGUAGE OverloadedStrings #-}
module Elm.Encoder
  ( toElmEncoderRef
  , toElmEncoderRefWith
  , toElmEncoderSource
  , toElmEncoderSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Text
import           Elm.Common
import           Elm.Type
import           Formatting

class HasEncoder a where
  render :: a -> Reader Options Text

class HasEncoderRef a where
  renderRef :: a -> Reader Options Text

instance HasEncoder ElmDatatype where
    render (ElmDatatype name constructor) =
        sformat
            (stext % " : " % stext % " -> E.Value" % cr % stext % " x =" % stext)
            fnName
            name
            fnName <$>
        render constructor
      where
        fnName = sformat ("encode" % stext) name
    render (ElmPrimitive primitive) = renderRef primitive

instance HasEncoderRef ElmDatatype where
    renderRef (ElmDatatype name _) =
        pure $ sformat ("encode" % stext) name
    renderRef (ElmPrimitive primitive) = renderRef primitive

instance HasEncoder ElmConstructor where
    render (RecordConstructor _ value) =
      sformat (cr % "    E.object" % cr % "        [ " % stext % cr % "        ]") <$> render value

instance HasEncoder ElmValue where
    render (ElmField name value) = do
        fieldModifier <- asks fieldLabelModifier
        valueBody <- render value
        pure $
            sformat
                ("( \"" % stext % "\", " % stext % " x." % stext % " )")
                (fieldModifier name)
                valueBody
                name
    render (ElmPrimitiveRef primitive) = renderRef primitive
    render (ElmRef name) = pure $ sformat ("encode" % stext) name
    render (Values x y) = sformat (stext % cr % "        , " % stext) <$> render x <*> render y

instance HasEncoderRef ElmPrimitive where
    renderRef EDate = pure "(E.string << toISOString)"
    renderRef EUnit = pure "E.null"
    renderRef EInt = pure "E.int"
    renderRef EChar = pure "E.char"
    renderRef EBool = pure "E.bool"
    renderRef EFloat = pure "E.float"
    renderRef EString = pure "E.string"
    renderRef (EList (ElmPrimitive EChar)) = pure "E.string"
    renderRef (EList value) =
        sformat ("(E.list << List.map " % stext % ")") <$> renderRef value
    renderRef (EMaybe value) =
        sformat ("(Maybe.withDefault E.null << Maybe.map " % stext % ")") <$>
        renderRef value
    renderRef (ETuple2 x y) =
        sformat ("(E.tuple2 " % stext % " " % stext % ")") <$> renderRef x <*>
        renderRef y
    renderRef (EDict k v) =
        sformat ("(E.dict " % stext % " " % stext % ")") <$> renderRef k <*> renderRef v

toElmEncoderRefWith :: ElmType a => Options -> a -> Text
toElmEncoderRefWith options x = runReader (renderRef (toElmType x)) options

toElmEncoderRef :: ElmType a => a -> Text
toElmEncoderRef = toElmEncoderRefWith defaultOptions

toElmEncoderSourceWith :: ElmType a => Options -> a -> Text
toElmEncoderSourceWith options x = runReader (render (toElmType x)) options

toElmEncoderSource :: ElmType a => a -> Text
toElmEncoderSource = toElmEncoderSourceWith defaultOptions
