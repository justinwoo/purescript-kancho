module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Newtype (class Newtype)
import Global.Unsafe (unsafeStringify)
import Kancho (class HasElmPortVersion, toElmModel)
import Kancho.Generate (class HasElmTypeRep, getElmRep)
import Type.Prelude (Proxy(..))

-- Simple Record example
type Coords =
  { x :: Int
  , y :: Int
  }

-- Nested Record example
type EtchSketch =
  { cursor :: Coords
  , points :: Array Coords
  }

-- Newtyped Record example
-- for when you want more control over the output (or if you just want to use a newtype)
newtype Thing = Thing
  { a :: Boolean
  , b :: Number
  , c :: String
  }

-- for newtype utilities
derive instance newtypeThing :: Newtype Thing _

-- verifying that this is port safe by inspecting the inner element
-- though you can also get this by generating the code anyway and seeing if there's an error
instance hasElmPortVersionThing ::
  ( HasElmPortVersion rec
  , Newtype Thing rec
  ) => HasElmPortVersion Thing

-- choose to short-circuit the inline generation with the name
instance hasElmTypeRepThing :: HasElmTypeRep Thing where
  toElmTypeRep _ _ = "Thing"

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log <<< unsafeStringify $ toElmModel coords
  log <<< unsafeStringify $ toElmModel etchSketch
  log <<< unsafeStringify $ toElmModel thing
  log ""
  log $ "type Coords = " <> getElmRep (Proxy :: Proxy Coords)
  log $ "type EtchSketch = " <> getElmRep (Proxy :: Proxy EtchSketch)
  log $ "type Thing =  " <> getElmRep thingRecProxy
  where
    coords :: Coords
    coords = {x: 1, y: 2}

    etchSketch :: EtchSketch
    etchSketch = {cursor: {x: 1, y: 2}, points: []}

    thing :: Thing
    thing = Thing {a: true, b: 0.1 + 0.2, c: "c"}

    -- here i then create a proxy for the underlying record type of Thing
    -- and generate its type accordingly
    thingRecProxy :: forall rec
       . Newtype Thing rec
      => HasElmPortVersion rec
      => HasElmTypeRep rec
      => Proxy rec
    thingRecProxy = Proxy
