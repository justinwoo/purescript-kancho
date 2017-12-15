module Test.Main where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Data.Newtype (class Newtype)
import Global.Unsafe (unsafeStringify)
import Kancho (class HasElmPortVersion, getElmRep, toElmModel)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Type.Prelude (Proxy(..))

-- Simple Record example
newtype Coords = Coords
  { x :: Int
  , y :: Int
  }
derive instance newtypeCoords :: Newtype Coords _
instance hasElmPortVersionCoords :: HasElmPortVersion Coords where
  toElmTypeRep _ = "Coords"

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

-- choose to short-circuit the inline generation with the name
instance hasElmPortVersionThing :: HasElmPortVersion Thing where
  toElmTypeRep _ = "Thing"

jsonStringify :: forall a. a -> String
jsonStringify = unsafeStringify

main :: Eff _ Unit
main = launchAff_ do
  let
    output =
      "module Generated exposing (..)"
      <> "\n"
      <> "\n" <> "-- coords JS: " <> (jsonStringify $ toElmModel coords)
      <> "\n" <> "-- etchSketch JS: "  <> (jsonStringify $ toElmModel etchSketch)
      <> "\n" <> "-- thing JS: " <> (jsonStringify $ toElmModel thing)
      <> "\n"
      <> "\n" <> "type alias Coords = " <> getElmRep (newtypeInnerProxy $ Proxy :: Proxy Coords)
      <> "\n" <> "type alias EtchSketch = " <> getElmRep (Proxy :: Proxy EtchSketch)
      <> "\n" <> "type alias Thing =  " <> getElmRep (newtypeInnerProxy $ Proxy :: Proxy Coords)
  writeTextFile UTF8 "./test/Generated.elm" output
  where
    coords :: Coords
    coords = Coords {x: 1, y: 2}

    etchSketch :: EtchSketch
    etchSketch = {cursor: Coords {x: 1, y: 2}, points: []}

    thing :: Thing
    thing = Thing {a: true, b: 0.1 + 0.2, c: "c"}

    -- here i then create a proxy for the underlying record type of any newtype
    -- and generate its type accordingly
    newtypeInnerProxy :: forall a rec
       . Newtype a rec
      => HasElmPortVersion rec
      => Proxy a
      -> Proxy rec
    newtypeInnerProxy _ = Proxy
