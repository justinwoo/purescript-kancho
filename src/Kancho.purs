module Kancho where

import Prelude

import Data.Foldable (intercalate)
import Data.Foreign (Foreign)
import Data.List (List, (:))
import Data.Monoid (mempty)
import Type.Prelude (class IsSymbol, class ListToRow, class RowToList, Proxy(..), SProxy(..), reflectSymbol)
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
  , CheckElmPortVersionFields fieldList
  ) => HasElmPortVersion (Record fields) where
  toElmTypeRep proxy =
    "{ " <> contents <> "}"
    where
      contents = intercalate "\n, " $ extractFields proxy


class CheckElmPortVersionFields (xs :: RowList) where
  extractFields :: forall fields
     . RowToList fields xs
    => Proxy (Record fields)
    -> List String

instance checkElmPortVersionAndFieldsCons ::
  ( IsSymbol name
  , HasElmPortVersion ty
  , ListToRow tail tailRow
  , CheckElmPortVersionFields tail
  , RowToList tailRow tail
  , CheckElmPortVersionFields tail
  ) => CheckElmPortVersionFields (Cons name ty tail) where
  extractFields _ = field : rest
    where
      name = reflectSymbol (SProxy :: SProxy name)
      tyName = toElmTypeRep (Proxy :: Proxy ty)
      field = name <> " : " <> tyName
      rest = extractFields (Proxy :: Proxy (Record tailRow))

instance checkElmPortVersionAndFieldsNil :: CheckElmPortVersionFields Nil where
  extractFields _ = mempty
