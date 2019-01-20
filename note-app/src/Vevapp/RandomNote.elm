module Vevapp.RandomNote exposing
    ( RandomNote
    , new
    , next
    , setSharpPercentage
    , toString
    )

import Random


type RandomNote
    = RandomNote
        { seed : Random.Seed
        , char : Char
        , isSharp : Bool
        , sharpPercentage : Int
        }


new : Int -> Random.Seed -> RandomNote
new sharpPercentage seed =
    let
        ( note, newSeed ) =
            Random.step (noteGenerator sharpPercentage) seed
    in
    RandomNote
        { seed = newSeed
        , char = note.char
        , isSharp = note.isSharp
        , sharpPercentage = sharpPercentage
        }


setSharpPercentage : Int -> RandomNote -> RandomNote
setSharpPercentage percentage (RandomNote note) =
    RandomNote { note | sharpPercentage = percentage }


next : RandomNote -> RandomNote
next (RandomNote note) =
    let
        ( newNote, newSeed ) =
            Random.step (noteGenerator note.sharpPercentage) note.seed
    in
    if newNote.char == note.char then
        next <| RandomNote { note | seed = newSeed }

    else
        RandomNote
            { note
                | seed = newSeed
                , char = newNote.char
                , isSharp = newNote.isSharp
            }


toString : RandomNote -> String
toString (RandomNote { char, isSharp }) =
    if hasSharp char && isSharp then
        String.fromChar char ++ "#"

    else
        String.fromChar char


noteGenerator : Int -> Random.Generator { char : Char, isSharp : Bool }
noteGenerator sharpPercentage =
    let
        toChar n =
            Char.fromCode (65 + n)

        charGenerator =
            Random.map toChar (Random.int 0 6)

        boolGenerator =
            Random.map (\n -> sharpPercentage >= n) (Random.int 1 100)

        toNote char isSharp =
            { char = char
            , isSharp = isSharp
            }
    in
    Random.map2 toNote charGenerator boolGenerator


hasSharp : Char -> Bool
hasSharp char =
    case char of
        'A' ->
            True

        'C' ->
            True

        'D' ->
            True

        'F' ->
            True

        'G' ->
            True

        _ ->
            False
