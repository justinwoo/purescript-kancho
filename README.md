# Purescript-Kancho

A helper library for constraining types to be Elm port-safe and for providing helpers to codegen Elm types.

Named for a nice Korean snack Kancho that wraps a chocolate core with biscuit.

![](http://i.imgur.com/iUqmQyv.png)

(Or for the Japanese prank of poking people in their butthole)

## tl;dr

```purs
newtype Coords = Coords
  { x :: Int
  , y :: Int
  }
derive instance newtypeCoords :: Newtype Coords _
instance hasElmPortVersionCoords :: HasElmPortVersion Coords where
  toElmTypeRep _ = "Coords"

type EtchSketch =
  { cursor :: Coords
  , points :: Array Coords
  }
```

outputs

```elm
type alias Coords =
    { x : Int
    , y : Int
    }


type alias EtchSketch =
    { cursor : Coords
    , points : List Coords
    }
```

## Example

The [tests](test/Main.purs) provide some examples to follow along with.

## Example Usage/Motivations

The major chunk of this library is from work done for my [Elm-in-Halogen integration demo](https://github.com/justinwoo/purescript-halogen-elm-etch-sketch/) -- specifically on the [row-to-list-ver branch](https://github.com/justinwoo/purescript-halogen-elm-etch-sketch/tree/row-to-list-ver). The demo repo generates Elm types to a file and runs the Elm compiler in the full build, and uses port-safe types.

An updated blog post abou thte RowToList version is available [here](https://qiita.com/kimagure/items/09b24ed22cfc596248b4).

A blog post about the old Generics-Rep version is available [here](http://qiita.com/kimagure/items/d12525d42516f95dd541).
