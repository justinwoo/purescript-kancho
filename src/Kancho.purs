module Kancho where

import Prelude

import Data.Foldable (intercalate)
import Data.List (List, (:))
import Foreign (Foreign)
import Prim.RowList as RL
import Type.Prelude (class IsSymbol, Proxy(..), RLProxy(..), SProxy(..), reflectSymbol)

toElmModel :: forall a. HasElmPortVersion a => a -> a
toElmModel = identity

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
  ( RL.RowToList fields fieldList
  , HasElmPortVersionFields fieldList
  ) => HasElmPortVersion (Record fields) where
  toElmTypeRep _ =
    "{" <> contents <> "}"
    where
      contents = intercalate "\n  , " $ extractFields (RLProxy :: RLProxy fieldList)


class HasElmPortVersionFields (xs :: RL.RowList) where
  extractFields :: RLProxy xs -> List String

instance hasElmPortVersionAndFieldsCons ::
  ( IsSymbol name
  , HasElmPortVersion ty
  , HasElmPortVersionFields tail
  ) => HasElmPortVersionFields (RL.Cons name ty tail) where
  extractFields _ = field : rest
    where
      name = reflectSymbol (SProxy :: SProxy name)
      tyName = toElmTypeRep (Proxy :: Proxy ty)
      field = name <> " : " <> tyName
      rest = extractFields (RLProxy :: RLProxy tail)

instance hasElmPortVersionAndFieldsNil :: HasElmPortVersionFields RL.Nil where
  extractFields _ = mempty
