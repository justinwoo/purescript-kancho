module Kancho.Generate where

import Prelude

import Data.Foldable (intercalate)
import Data.Foreign (Foreign)
import Data.List (List, (:))
import Data.Monoid (mempty)
import Kancho (class HasElmPortVersion)
import Type.Prelude (class IsSymbol, class ListToRow, class RowToList, Proxy(..), SProxy(..), reflectSymbol)
import Type.Row (Cons, Nil, kind RowList)

getElmRep :: forall a
   . HasElmPortVersion a
  => HasElmTypeRep a
  => Proxy a
  -> String
getElmRep _ = toElmTypeRep 1 (Proxy :: Proxy a)

class HasElmTypeRep r where
  toElmTypeRep :: Int -> Proxy r -> String

instance hasElmTypeRepInt :: HasElmTypeRep Int where
  toElmTypeRep _ _ = "Int"

instance hasElmTypeRepNumber :: HasElmTypeRep Number where
  toElmTypeRep _ _ = "Float"

instance hasElmTypeRepString :: HasElmTypeRep String where
  toElmTypeRep _ _ = "String"

instance hasElmTypeRepBoolean :: HasElmTypeRep Boolean where
  toElmTypeRep _ _ = "Bool"

instance hasElmTypeRepArray ::
  ( HasElmTypeRep inner
  ) => HasElmTypeRep (Array inner) where
  toElmTypeRep indent _ =
    "List " <> toElmTypeRep (indent + 1) (Proxy :: Proxy inner)

instance hasElmTypeRepForeign :: HasElmTypeRep Foreign where
  toElmTypeRep _ _ = "Json.Encode.Value"

instance hasElmTypeRepRecord ::
  ( HasElmTypeRepFields fieldList
  , RowToList fields fieldList
  ) => HasElmTypeRep (Record fields) where
  toElmTypeRep indent proxy =
    "\n" <> space <> "{ " <> contents <> "\n" <> space <> "}"
    where
      repeat 0 = ""
      repeat n = "  " <> repeat (n - 1)
      space = repeat indent
      contents = intercalate ("\n" <> space <> ", ") $ extractFields indent proxy

class HasElmTypeRepFields (xs :: RowList) where
  extractFields :: forall fields
     . RowToList fields xs
    => Int
    -> Proxy (Record fields)
    -> List String

instance hasElmTypeRepFieldsCons ::
  ( IsSymbol name
  , HasElmTypeRep ty
  , ListToRow tail tailRow
  , HasElmTypeRepFields tail
  , RowToList tailRow tail
  ) => HasElmTypeRepFields (Cons name ty tail) where
  extractFields indent _ = field : rest
    where
      name = reflectSymbol (SProxy :: SProxy name)
      tyName = toElmTypeRep (indent + 1) (Proxy :: Proxy ty)
      field = name <> " : " <> tyName
      rest = extractFields indent (Proxy :: Proxy (Record tailRow))

instance hasElmTypeRepFieldsNil :: HasElmTypeRepFields Nil where
  extractFields _ _ = mempty
