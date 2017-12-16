module Kancho where

import Prelude

import Data.Foldable (intercalate)
import Data.Foreign (Foreign)
import Data.List (List, (:))
import Data.Monoid (mempty)
import Type.Prelude (class IsSymbol, class RowToList, Proxy(..), RLProxy(..), SProxy(..), reflectSymbol)
import Type.Row (Cons, Nil, kind RowList)

toElmModel :: forall a. HasElmPortVersion a => a -> a
toElmModel = id

getElmRep :: forall a
   . HasElmPortVersion a
  => Proxy a
  -> String
getElmRep _ = toElmTypeRep (Proxy :: Proxy a)

class HasElmPortVersion ty where
  toElmTypeRep :: Proxy ty -> String

instance hasElmPortVersionInt :: HasElmPortVersion Int where
  toElmTypeRep _ = "Int"

instance hasElmPortVersionNumber :: HasElmPortVersion Number where
  toElmTypeRep _ = "Float"

instance hasElmPortVersionString :: HasElmPortVersion String where
  toElmTypeRep _ = "String"

instance hasElmPortVersionBoolean :: HasElmPortVersion Boolean where
  toElmTypeRep _ = "Bool"

instance hasElmPortVersionArray ::
  ( HasElmPortVersion inner
  ) => HasElmPortVersion (Array inner) where
  toElmTypeRep _ = "List " <> toElmTypeRep (Proxy :: Proxy inner)

instance hasElmPortVersionForeign :: HasElmPortVersion Foreign where
  toElmTypeRep _ = "Json.Encode.Value"

instance hasElmPortVersionRecord ::
  ( RowToList fields fieldList
  , HasElmPortVersionFields fieldList
  ) => HasElmPortVersion (Record fields) where
  toElmTypeRep _ =
    "{" <> contents <> "}"
    where
      contents = intercalate "\n  , " $ extractFields (RLProxy :: RLProxy fieldList)


class HasElmPortVersionFields (xs :: RowList) where
  extractFields :: RLProxy xs -> List String

instance hasElmPortVersionAndFieldsCons ::
  ( IsSymbol name
  , HasElmPortVersion ty
  , HasElmPortVersionFields tail
  ) => HasElmPortVersionFields (Cons name ty tail) where
  extractFields _ = field : rest
    where
      name = reflectSymbol (SProxy :: SProxy name)
      tyName = toElmTypeRep (Proxy :: Proxy ty)
      field = name <> " : " <> tyName
      rest = extractFields (RLProxy :: RLProxy tail)

instance hasElmPortVersionAndFieldsNil :: HasElmPortVersionFields Nil where
  extractFields _ = mempty
