{-# LANGUAGE OverloadedStrings #-}
module Elm.Record
  ( toElmTypeName
  , toElmTypeNameWith
  , toElmTypeSource
  , toElmTypeSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Text
import           Elm.Common
import           Elm.Type
import           Formatting

class HasType a where
  render :: a -> Reader Options Text

class HasTypeName a where
  renderName :: a -> Reader Options Text

instance HasType ElmDatatype where
    render (ElmDatatype typeName constructor@(RecordConstructor _ _)) =
        sformat ("type alias " % stext % " =" % cr % stext) typeName <$> render constructor
    render (ElmDatatype typeName constructor@(MultipleConstructors _)) =
        sformat ("type " % stext % cr % "    = " % stext) typeName <$> render constructor
    render (ElmDatatype typeName constructor@(NamedConstructor _ _)) =
        sformat ("type " % stext % cr % "    = " % stext) typeName <$> render constructor
    render (ElmDatatype typeName constructor@(NamedEmptyConstructor _ )) =
        sformat ("type " % stext % cr % "    = " % stext) typeName <$> render constructor
    render (ElmPrimitive primitive) = renderName primitive

instance HasTypeName ElmDatatype where
    renderName (ElmDatatype typeName _) = pure typeName
    renderName (ElmPrimitive primitive) = renderName primitive

instance HasType ElmConstructor where
    render (RecordConstructor _ value) =
        sformat ("    { " % stext % cr % "    }") <$> render value
    render (NamedConstructor constructorName value) =
        sformat (stext % " " % stext) constructorName <$> render value
    render (NamedEmptyConstructor constructorName ) =
        pure constructorName
    render (MultipleConstructors constructors) =
        fmap (Data.Text.intercalate "\n    | ") $ sequence $ render <$> constructors

instance HasType ElmValue where
    render (ElmRef name) = pure name
    render (Values x y) =
        sformat (stext % cr % "    , " % stext) <$> render x <*> render y
    render (ElmPrimitiveRef primitive) = renderName primitive
    render (ElmField name value) = do
        fieldModifier <- asks fieldLabelModifier
        sformat (stext % " : " % stext) (fieldModifier name) <$> render value

instance HasTypeName ElmPrimitive where
    renderName (EList (ElmPrimitive EChar)) = renderName EString
    renderName (EList value) =
        sformat ("List " % (parenthesize %. stext)) <$> renderName value
    renderName (ETuple2 x y) =
        sformat ("( " % stext % ", " % stext % " )") <$> renderName x <*> renderName y
    renderName (EMaybe value) =
        sformat ("Maybe " % (parenthesize %. stext)) <$> renderName value
    renderName (EDict k v) =
        sformat ("Dict " % (parenthesize %. stext) % " " % (parenthesize %. stext))
        <$> renderName k <*> renderName v
    renderName EInt = pure "Int"
    renderName EDate = pure "Date"
    renderName EBool = pure "Bool"
    renderName EChar = pure "Char"
    renderName EString = pure "String"
    renderName EUnit = pure "()"
    renderName EFloat = pure "Float"

toElmTypeNameWith :: ElmType a => Options -> a -> Text
toElmTypeNameWith options x = runReader (renderName (toElmType x)) options

toElmTypeName :: ElmType a => a -> Text
toElmTypeName = toElmTypeNameWith defaultOptions

toElmTypeSourceWith :: ElmType a => Options -> a -> Text
toElmTypeSourceWith options x = runReader (render (toElmType x)) options

toElmTypeSource :: ElmType a => a -> Text
toElmTypeSource = toElmTypeSourceWith defaultOptions
