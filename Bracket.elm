module Bracket where 

import Html exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import Html.Attributes exposing (..)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String exposing (..)
import Dict exposing (..)
import List.Extra as Listx exposing (find)

-- import BingoUtils as Utils 

onInput : Address a -> (String -> a) -> Attribute
onInput address f =
  on "input" targetValue (\v -> Signal.message address (f v))



-- MODEL

type alias BPair = {
  opener: Char, 
  closer: Char,      -- corresponding closer
  isEnabled: Bool,   -- is the bracket pair enabled?
  id: Int
}

type alias BMap = List BPair

type alias Model =
  {
    expression: String, 
    stack: SStack,
    bmap: BMap
  }

-- Constructor function for creating new pairs 
newPair : Char -> Char -> Bool -> Int -> BPair
newPair op cl en id =
  { opener = op,
    closer = cl,
    isEnabled = en,
    id = id
  }


-- UPDATE 

type Action
  = NoOp
  | UpdateExpression String
  | Mark Int



update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
  
    UpdateExpression contents ->
        { model | expression = contents }
    Mark id ->
        let
          updateEntry e =
            if e.id == id then { e | isEnabled = (not e.isEnabled) } else e
        in
          { model | bmap = List.map updateEntry model.bmap }

---- Validator Related 

{-
updateRec : Model -> String -> Maybe SStack -> Model 
updateRec r fields = 
  case fields of 
    [a] -> { r | expression = a }
    [a,b] -> { r | expression = a, stack = b }
    [a,b,c] -> { r | expression = a, stack = b, bmap = c }
    _ -> r

updateRec : Model -> Maybe String -> Maybe SStack -> Maybe BMap -> Model 
updateRec model expr ss bm = 
  {model |
    expression = expr |> Maybe.withDefault model.expression, 
    stack      = ss   |> Maybe.withDefault model.stack, 
    bmap       = bm   |> Maybe.withDefault model.bmap 
  }
-}

updateE : String -> Model -> Model
updateE e rec = { rec|expression = e }

updateS : SStack -> Model -> Model
updateS s rec = { rec|stack = s }


validate: Model -> Bool
validate model = 
  let 
    {expression, stack, bmap} = model 
  in     
    case (pop expression) of 
      Nothing -> 
        isEmpty stack

      Just (tok, restExpr) -> 
        case (getClosr tok bmap) of 
          Just (closer) -> 
            model
              |> updateE restExpr 
              |> updateS (pushC closer stack) 
              |> validate 
          Nothing -> 
            if (isClosr tok bmap) == True then
              case (pop stack) of 
                Just (ts, restOfStack) -> 
                  if ts == tok  then
                    model 
                      |> updateE restExpr |> updateS restOfStack 
                      |> validate 
                  else 
                    False 
                Nothing ->
                  False 
            else
                validate {model |expression = restExpr}


      
validateString: Model -> Bool 
validateString model  = 
  let 
    res = validate model  
    _= Debug.watch "Result " (res, s)
  in 
    res


type alias SStack = String

empty : SStack
empty =
  ""

push : String -> SStack -> SStack
push tok stacks = 
  tok ++ stacks


pop : SStack -> Maybe (Char, SStack)
pop stacks = 
  String.uncons stacks


peek: SStack -> String
peek stack = 
  String.slice 0 1 stack 


isEmpty: SStack -> Bool 
isEmpty s = 
  if String.isEmpty s then
    True
  else
    False 

pushC: Char -> SStack -> SStack
pushC c s = 
  push (String.fromChar c) s


isOpenr: Char -> List BPair -> Bool 
isOpenr o bmap = 
  bmap 
    |> List.filter .isEnabled
    |> List.map .opener
    |> List.member o
 

isClosr: Char -> List BPair -> Bool 
isClosr c bmap = 
  bmap 
    |> List.filter .isEnabled
    |> List.map .closer
    |> List.member c 


matchEnabledOpenr: Char -> BPair -> Maybe Char
matchEnabledOpenr o bp = 
  if bp.isEnabled then
    if bp.opener == o then 
        Just(bp.closer)
    else 
        Nothing 
  else 
    Nothing

-- Four variants of the Closure function 
getClosr: Char -> List BPair -> Maybe Char 
getClosr o bm =  
  List.head (List.filterMap (matchEnabledOpenr o) bm )


getClosr2 : Char -> List BPair -> Maybe Char 
getClosr2 o bmap = 
  let 
    getPair {opener, closer, isEnabled} = 
      case isEnabled of 
        True -> 
          (opener, closer)
        False ->
          ('\0', '\0')
  in 
    bmap 
      --|> List.filter .isEnabled 
      |> List.map getPair  
      |> Dict.fromList 
      |> Dict.get o
  --  |> Maybe.withDefault '0'


matchEnabledOpenrX : Char -> BPair -> Bool
matchEnabledOpenrX o bp = 
  bp.isEnabled && bp.opener == o 


getClosr3 : Char -> List BPair -> Maybe Char 
getClosr3 opener bmap = 
  bmap 
    |> Listx.find (matchEnabledOpenrX opener)
    |> Maybe.map .closer 

  
getClosr4 : Char -> List BPair -> Maybe Char 
getClosr4 o bmap = 
  bmap 
    |> List.filter (matchEnabledOpenrX o)
    |> List.map .opener
    |> List.head 
    


-- VIEW 

entryItem : Address Action -> BPair -> Html
entryItem address entry =
  li   
    [ classList [ ("highlight", entry.isEnabled) ],
      onClick address (Mark entry.id)
    ]
    [ span [][text "opener -   "], 
      span [ class "opener" ] [ text (String.fromChar entry.opener) ],
      span [ class "closer" ] [ text (String.fromChar entry.closer) ],
      span [][text "   - closer"] 
      -- button [ class "delete", onClick address (Delete entry.id) ] []
    ]
      
entryForm : Address Action -> Model -> Html
entryForm address model =
  div [ ]
    [ input
        [ type' "text",
          placeholder "{( () )}",
          value model.expression,
          name "phrase",
          autofocus True,
          onInput address UpdateExpression,
          strStyle
        ]
        [ ],
      --button [ class "change" ] [ text "Change" ],
      h2
        [ revStyle]
        [ text (model.expression ++ " is " ++ 
          toString (validateString model )) ]
    ]

entryList : Address Action -> List BPair -> Html
entryList address entries =
  let
    entryItems = List.map (entryItem address) entries
    items = entryItems -- ++ [ totalItem (totalPoints entries) ]
  in
    ul [ bracStyle] items

view : Address Action -> Model -> Html
view address model =
  div [] [
    div [ id "container" ]
      [ pageHeader,
        entryForm address model,
        bracketHeader,
        entryList address model.bmap
      ],
    div [] 
      [
        pageFooter
      ]
  ]



initialModel : Model
initialModel =
  { expression = "", 
    bmap = 
      [
        newPair '(' ')' True 1,
        newPair '{' '}' True 2, 
        newPair '<' '>' True 3
      ], 
    stack = empty 
  }


-- WIRE IT ALL TOGETHER!

main: Signal Html
main =
  StartApp.start
    { model = initialModel,
      view = view,
      update = update
    }


title : String -> Int -> Html
title message times =
  message ++ " "
    |> toUpper
    |> repeat times
    |> trimRight
    |> text

pageHeader : Html
pageHeader =
  h1 [ ] [ title "Validator" 1 ]

bracketHeader : Html
bracketHeader =
  h2 [ ] [ title "Bracket Map" 1 ]

pageFooter : Html
pageFooter =
  footer []
    [ a [ href "http://edu.kgisl.com" ]
        [ text "KGISL CampSite" ]
    ]


strStyle : Attribute
strStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
    
revStyle : Attribute
revStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    , ("color", "red")
    ]

bracStyle : Attribute
bracStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2")
    , ("text-align", "center")
    , ("color", "#f60")
    ]
