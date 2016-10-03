{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Elm.Decoder
  ( toElmDecoderName
  , toElmDecoderNameWith
  , toElmDecoderSource
  , toElmDecoderSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Text
import           Elm.Common
import           Elm.Type
import           Formatting

class HasDecoder a where
  render :: a -> Reader Options Text

class HasDecoderName a where
  renderName :: a -> Reader Options Text

instance HasDecoder ElmDatatype where
    render (ElmDatatype name constructor) =
        sformat
            (stext % " : Decoder " % stext % cr % stext % " =" % cr % stext)
            fnName
            name
            fnName <$>
        render constructor
      where
        fnName = sformat ("decode" % stext) name
    render (ElmPrimitive primitive) = renderName primitive


instance HasDecoderName ElmDatatype where
    renderName (ElmDatatype name _) = pure $ sformat ("decode" % stext) name
    renderName (ElmPrimitive primitive) = renderName primitive


instance HasDecoder ElmConstructor where
    render (NamedConstructor name value) =
        sformat ("    decode " % stext % cr % stext) name <$> render value
    render (RecordConstructor name value) =
        sformat ("    decode " % stext % cr % stext) name <$> render value


instance HasDecoder ElmValue where
    render (ElmRef name) = pure (sformat ("decode" % stext) name)
    render (ElmPrimitiveRef primitive) = renderName primitive
    render (Values x y) = sformat (stext % cr % stext) <$> render x <*> render y
    render (ElmField name value) = do
        fieldModifier <- asks fieldLabelModifier
        sformat
            ("        |> required \"" % stext % "\" " % stext)
            (fieldModifier name) <$>
            render value


instance HasDecoderName ElmPrimitive where
    renderName (EList (ElmPrimitive EChar)) = pure "string"
    renderName (EList value) =
        sformat ("(list " % stext % ")") <$> renderName value
    renderName (EDict key value) =
        sformat ("(map Dict.fromList " % stext % ")") <$>
        renderName (EList (ElmPrimitive (ETuple2 (ElmPrimitive key) value)))
    renderName (EMaybe value) =
        sformat ("(maybe " % stext % ")") <$> renderName value
    renderName (ETuple2 x y) =
        sformat ("(tuple2 (,) " % stext % " " % stext % ")")
        <$> renderName x <*> renderName y
    renderName EUnit = pure "(succeed ())"
    renderName EDate = pure "(customDecoder string Date.fromString)"
    renderName EInt = pure "int"
    renderName EBool = pure "bool"
    renderName EChar = pure "char"
    renderName EFloat = pure "float"
    renderName EString = pure "string"


toElmDecoderNameWith :: ElmType a => Options -> a -> Text
toElmDecoderNameWith options x = runReader (renderName (toElmType x)) options


toElmDecoderName :: ElmType a => a -> Text
toElmDecoderName = toElmDecoderNameWith defaultOptions


toElmDecoderSourceWith :: ElmType a => Options -> a -> Text
toElmDecoderSourceWith options x = runReader (render (toElmType x)) options


toElmDecoderSource :: ElmType a => a -> Text
toElmDecoderSource = toElmDecoderSourceWith defaultOptions
