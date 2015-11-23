import Array exposing (get)
import Graphics.Element exposing (..)
import StartApp.Simple as StartApp
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import List exposing (indexedMap, filter, map)
import String

type alias Row = List(String)

type alias Board = {
    rows: List(Row),
    currentPath: WordPath
}

type alias TilePos = (Int, Int)

type alias WordPath = List TilePos

type Action
  = NoOp
  | SelectTile TilePos
  | RemoveTile TilePos
  | ClearBoard

validPos : Pos -> Bool
validPos pos = True

zip xs ys =
  case (xs, ys) of
    (x :: xs', y :: ys') -> (x,y) :: zip xs' ys'
    (_, _) -> []

--validSuccessor : Pos -> Pos -> Bool
--validSuccessor last next =


tileStyle =
  [ ("width", "75px")
  , ("height", "75px")
  , ("text-align", "center")
  , ("line-height", "75px")
  , ("border-right", "1px solid black")
  , ("border-bottom", "1px solid black")
  , ("font-size", "20pt")
  , ("display", "inline-block")]

letterStyle = tileStyle

boardStyle =
  [ ("border-top", "1px solid black")
  , ("border-left", "1px solid black")
  , ("margin", "1em")]

currentWordStyle =
  [ ("margin-top", "20px") ]

viewLetter address currentPath rowIdx colIdx letter =
  a [class "letter-box"
    , style tileStyle
    , onClick address (SelectTile (rowIdx, colIdx))]
  [text letter]

viewRow address currentPath rowIdx row =
  div [class "row"] (indexedMap (viewLetter address currentPath rowIdx) row)

viewBoard address {rows, currentPath} =
  indexedMap (viewRow address currentPath) rows


getLetter rows (rowIdx, colIdx) =
  case get rowIdx (Array.fromList rows) of
    Nothing -> Nothing
    Just letters ->
      get colIdx (Array.fromList letters)

currentWordDiv address {rows, currentPath} =
  let
    letters = zip (map (getLetter rows) currentPath) currentPath
  in
    div [ class "word"
        , style currentWordStyle]
    (map (\(maybeLetter, pos) ->
            div [class "letter"
                , style letterStyle
                , onClick address (RemoveTile pos)]
            [text (case maybeLetter of
                     Nothing -> ""
                     Just letter -> letter)]) letters)

clearBoardLink address =
  button [ class "clear-board"
         , onClick address ClearBoard]
  [text "Clear"]

model: Board
model = {rows = [["A", "B", "C", "D"]
                ,["E", "F", "G", "H"]
                ,["I", "J", "K", "L"]
                ,["M", "N", "O", "P"]],
         currentPath = []}

view address model = div [class "game"]
                     [ div [class  "board", style boardStyle] (viewBoard address model)
                     , currentWordDiv address model
                     , clearBoardLink address]

update action model =
  case action of
    NoOp -> model
    SelectTile (x, y) -> {model | currentPath = model.currentPath ++ [(x, y)]}
    RemoveTile pos -> {model | currentPath = filter ((/=) pos) model.currentPath}
    ClearBoard -> {model | currentPath = []}

main = StartApp.start { model = model
                      , view = view
                      , update = update }
