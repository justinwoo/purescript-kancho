# Purescript-Kancho

A helper library for constraining types to be Elm port-safe and for providing helpers to codegen Elm types.

Named for a nice Korean snack Kancho that wraps a chocolate core with biscuit.

![](http://i.imgur.com/iUqmQyv.png)

(Or for the Japanese prank of poking people in their butthole)

## tl;dr

```purs
type Coords =
  { x :: Int
  , y :: Int
  }

log <<< unsafeStringify $ toElmModel coords
  -- equivalent to JSON.stringify
  log <<< unsafeStringify $ toElmModel coords
  
  -- uses getElmRep to get the Elm type representation of Coords
  log $ "type Coords = " <> getElmRep (Proxy :: Proxy Coords)
  -- (Proxy lets us not have to provide a concrete value for Coords)

  where
    coords :: Coords
    coords = {x: 1, y: 2}
```

outputs

```
{"x":1,"y":2}

type Coords =
  { x : Int
  , y : Int
  }
```

## Example

The [tests](test/Main.purs) provide some examples to follow along with.

## Example Usage/Motivations

The major chunk of this library is from work done for my [Elm-in-Halogen integration demo](https://github.com/justinwoo/purescript-halogen-elm-etch-sketch/) -- specifically on the [row-to-list-ver branch](https://github.com/justinwoo/purescript-halogen-elm-etch-sketch/tree/row-to-list-ver). The demo repo generates Elm types to a file and runs the Elm compiler in the full build, and uses port-safe types.

A blog post about the Generics-Rep version is available [here](http://qiita.com/kimagure/items/d12525d42516f95dd541), but this library uses the newer `RowToList` 0.11.6 feature as a simpler way to constrain types.
