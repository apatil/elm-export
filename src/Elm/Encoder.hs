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
    render (ElmPrimitive primitive) = render primitive

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
    render (ElmPrimitiveRef primitive) = render primitive
    render (ElmRef name) = pure $ sformat ("encode" % stext) name
    render (Values x y) = sformat (stext % cr % "        , " % stext) <$> render x <*> render y

instance HasEncoder ElmPrimitive where
    render EDate = pure "(string << toISOString)"
    render EUnit = pure "null"
    render EInt = pure "int"
    render EChar = pure "char"
    render EBool = pure "bool"
    render EFloat = pure "float"
    render EString = pure "string"
    render (EList (ElmPrimitive EChar)) = pure "string"
    render (EList value) =
        sformat ("(list << List.map " % stext % ")") <$> renderName value
    render (EMaybe value) =
        sformat ("(Maybe.withDefault null << Maybe.map " % stext % ")") <$>
        renderName value
    render (ETuple2 x y) =
        sformat ("(tuple2 " % stext % " " % stext % ")") <$> render x <*>
        render y
    render (EDict k v) =
        sformat ("(dict " % stext % " " % stext % ")") <$> render k <*> renderName v

instance HasEncoderName ElmPrimitive where
    renderName = render

toElmEncoderNameWith :: ElmType a => Options -> a -> Text
toElmEncoderNameWith options x = runReader (renderName (toElmType x)) options

toElmEncoderName :: ElmType a => a -> Text
toElmEncoderName = toElmEncoderNameWith defaultOptions

toElmEncoderSourceWith :: ElmType a => Options -> a -> Text
toElmEncoderSourceWith options x = runReader (render (toElmType x)) options

toElmEncoderSource :: ElmType a => a -> Text
toElmEncoderSource = toElmEncoderSourceWith defaultOptions
