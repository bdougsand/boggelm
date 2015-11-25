import Array exposing (get)
import Graphics.Element exposing (..)
import StartApp.Simple as StartApp
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import List exposing (all, drop, filter, head, indexedMap, length, map, member, take)
import Maybe exposing (andThen)
import String exposing (join)

type alias Row = List(String)

type alias Board = {
    rows: List(Row),
    currentPath: WordPath,
    words: List(String)
}

type alias TilePos = (Int, Int)

type alias WordPath = List TilePos

type Action
  = NoOp
  | SelectTile TilePos
  | RemoveTile TilePos
  | ClearBoard
  | CompleteWord

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

nth n xs =
  if n > 0 then
    take 1 (drop n xs)
  else
    nth ((length xs) + n) xs

makeList x = [x]

tileStyle =
  [ ("width", "75px")
  , ("height", "75px")
  , ("text-align", "center")
  , ("line-height", "75px")
  , ("border-right", "1px solid black")
  , ("border-bottom", "1px solid black")
  , ("font-size", "20pt")
  , ("display", "inline-block")]

letterStyle =
  [ ("width", "50px")
  , ("height", "50px")
  , ("font-style", "bold")
  , ("font-size", "14pt")
  , ("display", "inline-block")]

selectedStyle =
  tileStyle ++ [ ("background-color", "lightgray")
               , ("color", "red") ]

boardStyle =
  [ ("border-top", "1px solid black")
  , ("border-left", "1px solid black")
  , ("margin", "1em")]

currentWordStyle =
  [ ("margin-top", "20px") ]

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
    Just letter -> letter

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

viewLetter address currentPath rowIdx colIdx letter =
  if member (rowIdx, colIdx) currentPath then
    a [ class "letter-box"
      , style selectedStyle
      , onClick address NoOp] [text letter]
   else
     a [class "letter-box"
       , href "#"
       , style tileStyle
       , onClick address (SelectTile (rowIdx, colIdx))]
     [text letter]

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
            [text letter]) letters)

clearBoardLink address =
  button [ class "clear-board"
         , onClick address ClearBoard]
  [text "Clear"]

completeWordLink address =
  button [ class "complete-word"
         , onClick address CompleteWord]
  [text "Save"]

wordList address model =
  ul [ class "word-list" ]
     (map (text >> makeList >> (li [ class "word" ])) model.words)

model: Board
model = {rows = [["A", "B", "C", "D"]
                ,["E", "F", "G", "H"]
                ,["I", "J", "K", "L"]
                ,["M", "N", "O", "P"]],
         currentPath = [],
         words = []}

view address model = div [class "game"]
                     [ div [class  "board", style boardStyle] (viewBoard address model)
                     , currentWordDiv address model
                     , clearBoardLink address
                     , completeWordLink address
                     , wordList address model]

update action model =
  case action of
    NoOp -> model
    SelectTile pos -> selectTile model pos
    RemoveTile pos -> removeTile model pos
    ClearBoard -> {model | currentPath = []}
    CompleteWord -> completeWord model

main = StartApp.start { model = model
                      , view = view
                      , update = update }
