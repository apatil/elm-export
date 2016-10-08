{-# LANGUAGE OverloadedStrings #-}
module Elm.Record
  ( toElmTypeRef
  , toElmTypeRefWith
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

class HasTypeRef a where
  renderRef :: a -> Reader Options Text

instance HasType ElmDatatype where
    render (ElmDatatype typeName constructor@(RecordConstructor _ _)) =
        sformat ("type alias " % stext % " =" % cr % stext) typeName <$> render constructor
    render (ElmDatatype typeName constructor@(MultipleConstructors _)) =
        sformat ("type " % stext % cr % "    = " % stext) typeName <$> render constructor
    render (ElmDatatype typeName constructor@(NamedConstructor _ _)) =
        sformat ("type " % stext % cr % "    = " % stext) typeName <$> render constructor
    render (ElmDatatype typeName constructor@(NamedEmptyConstructor _ )) =
        sformat ("type " % stext % cr % "    = " % stext) typeName <$> render constructor
    render (ElmPrimitive primitive) = renderRef primitive

instance HasTypeRef ElmDatatype where
    renderRef (ElmDatatype typeName _) = pure typeName
    renderRef (ElmPrimitive primitive) = renderRef primitive

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
    render (ElmPrimitiveRef primitive) = renderRef primitive
    render (ElmField name value) = do
        fieldModifier <- asks fieldLabelModifier
        sformat (stext % " : " % stext) (fieldModifier name) <$> render value

instance HasTypeRef ElmPrimitive where
    renderRef (EList (ElmPrimitive EChar)) = renderRef EString
    renderRef (EList value) =
        sformat ("List " % (parenthesize %. stext)) <$> renderRef value
    renderRef (ETuple2 x y) =
        sformat ("( " % stext % ", " % stext % " )") <$> renderRef x <*> renderRef y
    renderRef (EMaybe value) =
        sformat ("Maybe " % (parenthesize %. stext)) <$> renderRef value
    renderRef (EDict k v) =
        sformat ("Dict " % (parenthesize %. stext) % " " % (parenthesize %. stext))
        <$> renderRef k <*> renderRef v
    renderRef EInt = pure "Int"
    renderRef EDate = pure "Date"
    renderRef EBool = pure "Bool"
    renderRef EChar = pure "Char"
    renderRef EString = pure "String"
    renderRef EUnit = pure "()"
    renderRef EFloat = pure "Float"

toElmTypeRefWith :: ElmType a => Options -> a -> Text
toElmTypeRefWith options x = runReader (renderRef (toElmType x)) options

toElmTypeRef :: ElmType a => a -> Text
toElmTypeRef = toElmTypeRefWith defaultOptions

toElmTypeSourceWith :: ElmType a => Options -> a -> Text
toElmTypeSourceWith options x = runReader (render (toElmType x)) options

toElmTypeSource :: ElmType a => a -> Text
toElmTypeSource = toElmTypeSourceWith defaultOptions
