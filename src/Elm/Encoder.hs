{-# LANGUAGE OverloadedStrings #-}
module Elm.Encoder
  ( toElmEncoderName
  , toElmEncoderNameWith
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

class HasEncoderName a where
  renderName :: a -> Reader Options Text

instance HasEncoder ElmDatatype where
    render (ElmDatatype name constructor) =
        sformat
            (stext % " : " % stext % " -> Value" % cr % stext % " x =" % stext)
            fnName
            name
            fnName <$>
        render constructor
      where
        fnName = sformat ("encode" % stext) name
    render (ElmPrimitive primitive) = renderName primitive

instance HasEncoderName ElmDatatype where
    renderName (ElmDatatype name _) =
        pure $ sformat ("encode" % stext) name
    renderName (ElmPrimitive primitive) = renderName primitive

instance HasEncoder ElmConstructor where
    render (RecordConstructor _ value) =
      sformat (cr % "    object" % cr % "        [ " % stext % cr % "        ]") <$> render value

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
    render (ElmPrimitiveRef primitive) = renderName primitive
    render (ElmRef name) = pure $ sformat ("encode" % stext) name
    render (Values x y) = sformat (stext % cr % "        , " % stext) <$> render x <*> render y

instance HasEncoderName ElmPrimitive where
    renderName EDate = pure "(string << toISOString)"
    renderName EUnit = pure "null"
    renderName EInt = pure "int"
    renderName EChar = pure "char"
    renderName EBool = pure "bool"
    renderName EFloat = pure "float"
    renderName EString = pure "string"
    renderName (EList (ElmPrimitive EChar)) = pure "string"
    renderName (EList value) =
        sformat ("(list << List.map " % stext % ")") <$> renderName value
    renderName (EMaybe value) =
        sformat ("(Maybe.withDefault null << Maybe.map " % stext % ")") <$>
        renderName value
    renderName (ETuple2 x y) =
        sformat ("(tuple2 " % stext % " " % stext % ")") <$> renderName x <*>
        renderName y
    renderName (EDict k v) =
        sformat ("(dict " % stext % " " % stext % ")") <$> renderName k <*> renderName v

toElmEncoderNameWith :: ElmType a => Options -> a -> Text
toElmEncoderNameWith options x = runReader (renderName (toElmType x)) options

toElmEncoderName :: ElmType a => a -> Text
toElmEncoderName = toElmEncoderNameWith defaultOptions

toElmEncoderSourceWith :: ElmType a => Options -> a -> Text
toElmEncoderSourceWith options x = runReader (render (toElmType x)) options

toElmEncoderSource :: ElmType a => a -> Text
toElmEncoderSource = toElmEncoderSourceWith defaultOptions
