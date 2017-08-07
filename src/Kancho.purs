module Kancho where

import Prelude

import Data.Foreign (Foreign)
import Type.Prelude (class RowToList)
import Type.Row (Cons, Nil, kind RowList)

toElmModel :: forall a. HasElmPortVersion a => a -> a
toElmModel = id

class HasElmPortVersion ty

instance hasElmPortVersionInt :: HasElmPortVersion Int

instance hasElmPortVersionNumber :: HasElmPortVersion Number

instance hasElmPortVersionString :: HasElmPortVersion String

instance hasElmPortVersionBoolean :: HasElmPortVersion Boolean

instance hasElmPortVersionArray ::
  ( HasElmPortVersion inner
  ) => HasElmPortVersion (Array inner)

instance hasElmPortVersionForeign :: HasElmPortVersion Foreign

instance hasElmPortVersionRecord ::
  ( RowToList fields fieldList
  , CheckElmPortVersionFields fieldList
  ) => HasElmPortVersion (Record fields)

class CheckElmPortVersionFields (xs :: RowList)

instance checkElmPortVersionAndFieldsCons ::
  ( HasElmPortVersion ty
  , CheckElmPortVersionFields tail
  ) => CheckElmPortVersionFields (Cons name ty tail)

instance checkElmPortVersionAndFieldsNil :: CheckElmPortVersionFields Nil
