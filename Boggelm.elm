module Boggelm where

import Array exposing (get)
import Char
import Graphics.Element exposing (..)
import StartApp.Simple as StartApp
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import List exposing (all, drop, filter, head, indexedMap, length, map, member, take)
import Maybe exposing (andThen)
import String exposing (join)
import Random as R

type alias Row = List(String)

type alias Board = {
    rows: List(Row),
    currentPath: WordPath,
    words: List(String),
    seed: R.Seed
}

type alias TilePos = (Int, Int)

type alias WordPath = List TilePos

type Action
  = NoOp
  | SelectTile TilePos
  | RemoveTile TilePos
  | ClearBoard
  | CompleteWord
  | Regenerate
  | RemoveWord String

validPos : Pos -> Bool
validPos pos = True

zip xs ys =
  case (xs, ys) of
    (x :: xs', y :: ys') -> (x,y) :: zip xs' ys'
    (_, _) -> []

butlast xs =
  take (length xs - 1) xs

last xs =
  head (drop (length xs - 1) xs)

at xs i =
  head (drop i xs)

maybeEq mayx x =
  case mayx of
    Nothing -> False
    Just lastx -> lastx == x

isLast xs x =
  maybeEq (last xs) x

nth n xs =
  if n > 0 then
    take 1 (drop n xs)
  else
    nth ((length xs) + n) xs

makeList x = [x]

gameStyle =
  [ ("background-color", "lightgray")
  , ("height", "700px")
  , ("text-align", "center")
  , ("width", "100%" ) ]

tileStyle =
  [ ("width", "75px")
  , ("height", "75px")
  , ("text-align", "center")
  , ("line-height", "75px")
  , ("background-color", "white")
  , ("border-right", "1px solid black")
  , ("border-bottom", "1px solid black")
  , ("font-size", "20pt")
  , ("display", "inline-block")]

letterStyle =
  [ ("width", "50px")
  , ("height", "50px")
  , ("font-weight", "bold")
  , ("font-size", "14pt")
  , ("display", "inline-block")]

selectedStyle =
  tileStyle ++ [ ("background-color", "lightgray")
               , ("color", "red") ]

lastStyle =
  selectedStyle ++ [ ("box-shadow", "0 0 2px 2px black inset")
                   , ("color", "black")
                   , ("font-weight", "bold") ]

boardStyle =
  [ ("border-top", "1px solid black")
  , ("border-left", "1px solid black")
  , ("display", "inline-block")
  , ("margin", "auto")
  , ("margin-top", "10px")]

currentWordStyle =
  [ ("background-color", "white")
  , ("margin", "20px")
  , ("height", "30px")]

wordListStyle =
  [ ("display", "inline-block")
  , ("width", "300px") ]

wordStyle =
  [ ("border", "1px solid black")
  , ("border-radius", "5px")
  , ("margin-right", "2px")
  , ("padding", "2px")
  , ("display", "inline-block") ]

removeTile model pos =
  let
    {currentPath} = model
  in
    case last model.currentPath of
      Nothing -> model
      Just lastPos -> if pos == lastPos then
                        {model | currentPath = butlast currentPath}
                      else
                        model

validSuccessor : Maybe TilePos -> TilePos -> Bool
validSuccessor lastPos (x2, y2) =
  case lastPos of
    Nothing -> True
    Just (x1, y1) ->
      all ((<=) 0) [x1, y1, x2, y2, abs (x1-x2), abs (y1-y2)] &&
          abs (x1 - x2) < 2 &&
          abs (y1 - y2) < 2 &&
          (x1, y1) /= (x2, y2)

selectTile model pos =
  let
    {currentPath} = model
    lastPos = last currentPath
  in
    if not (member pos currentPath) &&
       validSuccessor lastPos pos
    then
      {model | currentPath = currentPath ++ [pos]}
    else
      model

tileArray rows =
  Array.fromList (map Array.fromList rows)

getLetter rowsArray (rowIdx, colIdx) =
  case get rowIdx rowsArray `andThen` get colIdx of
    Nothing -> ""
    Just letter -> fullLetter letter

getWord rows path =
  join "" (map (getLetter (tileArray rows)) path)

completeWord model =
  let
    {currentPath, rows, words} = model
    word = getWord rows currentPath
  in
    { model | words = if String.isEmpty word || member word words
                      then
                        words
                      else
                        words ++ [word],
              currentPath = []}

removeWord model word =
  { model | words = filter ((/=) word) model.words }

listGen l default =
  R.map ((at l) >> (Maybe.withDefault default)) (R.int 0 ((length l) - 1))

letterGen =
  R.map (Char.fromCode >> String.fromChar) (R.int 65 90)

consonantGen =
  listGen (String.toList "CDFGHJKLMNPQRSTVWXYZ") '!'

vowelGen =
  listGen (String.toList "AEIO") '!'

--letterGen =


letterListGen =
  R.list 4 letterGen

rowsGen =
  R.list 4 letterListGen

regenerateRows model =
  let
    (rows, newSeed) = R.generate rowsGen model.seed
  in
    { model | rows = rows,
              seed = newSeed,
              currentPath = [],
              words = [] }

fullLetter l =
  if l == "Q" then "Qu" else l

viewLetter address currentPath rowIdx colIdx letter =
  if member (rowIdx, colIdx) currentPath then
    a [ class "letter-box"
      , style (if isLast currentPath (rowIdx, colIdx) then
                 lastStyle else selectedStyle)
      , onClick address NoOp]
    [text (fullLetter letter)]
  else
    a [class "letter-box"
      , href "#"
      , style tileStyle
      , onClick address (SelectTile (rowIdx, colIdx))]
    [text (fullLetter letter)]

viewRow address currentPath rowIdx row =
  div [class "row"] (indexedMap (viewLetter address currentPath rowIdx) row)

viewBoard address {rows, currentPath} =
  indexedMap (viewRow address currentPath) rows

currentWordDiv address {rows, currentPath} =
  let
    rowsArray = tileArray rows
    letters = zip (map (getLetter rowsArray) currentPath) currentPath
  in
    div [ class "word"
        , style currentWordStyle]
    (map (\(letter, pos) ->
            div [class "letter"
                , style letterStyle
                , onClick address (RemoveTile pos)]
            [text (fullLetter letter)]) letters)

clearBoardLink address =
  button [ class "clear-board"
         , onClick address ClearBoard]
  [text "Clear"]

completeWordLink address =
  button [ class "complete-word"
         , onClick address CompleteWord]
  [text "Save"]

regenerateLink address =
  button [ onClick address Regenerate ] [ text "Regenerate" ]

workListItem address word =
  span [style wordStyle] [ text word
                      , a [ href "#"
                          , onClick address (RemoveWord word) ]
                        [ text " x" ]]

wordList address model =
  ul [style wordListStyle]
     (map (workListItem address) model.words)

model: Board
model = {rows = [["A", "B", "C", "D"]
                ,["E", "F", "G", "H"]
                ,["I", "J", "K", "L"]
                ,["M", "N", "O", "P"]],
         currentPath = [],
         words = [],
         seed = R.initialSeed 23}

view address model =
  div [style gameStyle]
      [ div [style boardStyle] (viewBoard address model)
      , currentWordDiv address model
      , completeWordLink address
      , clearBoardLink address
      , div [] [ wordList address model
               , br [] []
               , regenerateLink address]]

update action model =
  case action of
    NoOp -> model
    SelectTile pos -> selectTile model pos
    RemoveTile pos -> removeTile model pos
    ClearBoard -> {model | currentPath = []}
    CompleteWord -> completeWord model
    RemoveWord word -> removeWord model word
    Regenerate -> regenerateRows model

port title : String
port title = "Boggle"

main = StartApp.start { model = model
                      , view = view
                      , update = update }
