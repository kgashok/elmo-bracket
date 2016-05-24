module Bracket exposing (..) -- where 

import Html exposing (..)
import Html.Events exposing (on, onInput, targetValue, onClick)
import Html.Attributes exposing (..)
-- import Signal exposing (Address)
-- import StartApp.Simple as StartApp
import Html.App as Html

import String exposing (..)
import Dict exposing (fromList, get)
-- import List.Extra as Listx exposing (find)


import SStack as Stack exposing (..)
--import BingoUtils as Utils 
import Version exposing (version)

import Mouse
import Keyboard

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
    expression: String, -- what the user inputs
    stack: SStack,      -- integral ADT required for validation 
    bmap: BMap,         -- containing list of bracket pairs
    isBalanced: Bool,   -- intermediary result
    isValid: Bool,      -- the ultimate outcome! 
    showBracket: Bool,
    showStack: Bool
  }

-- Constructor function for creating new pairs 
newPair : Char -> Char -> Bool -> Int -> BPair
newPair op cl en id =
  { opener = op,
    closer = cl,
    isEnabled = en,
    id = id
  }


-- UPDATE (aka CONTROL)

-- type Action
type Msg 
  = NoOp
  | UpdateExpression String
  | Mark Int
  | MouseMsg Mouse.Position
  | KeyMsg Keyboard.KeyCode


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
  
    UpdateExpression contents ->
      ({ model | expression = contents }, Cmd.none)
    Mark id ->
      let
        updateEntry e =
          if e.id == id then { e | isEnabled = (not e.isEnabled) } else e
      in
        ({ model | bmap = List.map updateEntry model.bmap }, Cmd.none)

    MouseMsg position ->
      (model, Cmd.none)
      
    KeyMsg code ->
      case code of 
        2 ->  -- Ctrl-b
          ({model | showBracket = (not model.showBracket) }, Cmd.none)
        17 -> -- Ctrl-q
          ({model | showStack = (not model.showStack)}, Cmd.none)
        _ ->  
          (model, Cmd.none)




---- Validator Related 

updateE : String -> Model -> Model
updateE e rec = { rec|expression = e }

updateS : SStack -> Model -> Model
updateS s rec = { rec|stack = s }


validate: Model -> Model 
validate model = 
  let 
    {expression, stack, bmap} = model 
  in 
    case (pop expression) of 
      Nothing -> 
        {model| isBalanced = Stack.isEmpty stack }

      Just (tok, restExpr) -> 
        case (getClosr tok bmap) of 
          Just (closer) -> 
            model
              |> updateE restExpr |> updateS (pushC closer stack) 
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
                    { model| isValid = False} 
                Nothing ->
                  { model| isBalanced = False} 
            else
              validate {model |expression = restExpr}


      
validateString: Model -> Model
validateString model  = 
  let 
    res = validate model  
    --_= Debug.watch "Result " (res.isValid, res.stack, res.expression)
  in 
    res


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
  case (bp.isEnabled, bp.opener == o) of 
    (True, True) -> Just bp.closer 
    _ -> Nothing 


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
      |> List.map getPair  
      |> Dict.fromList 
      |> Dict.get o


matchEnabledOpenrX : Char -> BPair -> Bool
matchEnabledOpenrX o bp = 
  bp.isEnabled && bp.opener == o 


{-
getClosr3 : Char -> List BPair -> Maybe Char 
getClosr3 opener bmap = 
  bmap 
    |> Listx.find (matchEnabledOpenrX opener)
    |> Maybe.map .closer 
-}
  
getClosr4 : Char -> List BPair -> Maybe Char 
getClosr4 o bmap = 
  bmap 
    |> List.filter (matchEnabledOpenrX o)
    |> List.map .opener
    |> List.head 
    


-- VIEW 


stackItem : (Int, Char) -> Html Msg
stackItem (index, token) = 
  li []
    --[ classList [ ("highlight", entry.isEnabled) ],
    --  onClick address (Mark entry.id)
    --]
    [ span [ class "index" ] [ text (toString index) ],
      span [ class "token" ] [ text (String.fromChar token) ]
      -- span [][text "   - closer"] 
      -- button [ class "delete", onClick address (Delete entry.id) ] []
    ]


entryItem : BPair -> Html Msg
entryItem entry =
  li   
    [ classList [ ("highlight", entry.isEnabled) ],
      onClick (Mark entry.id)
    ]
    [ span [ class "phrase" ] [ text (String.fromChar entry.opener) ],
      span [ class "points" ] [ text (String.fromChar entry.closer) ]
      -- span [][text "   - closer"] 
      -- button [ class "delete", onClick (Delete entry.id) ] []
    ]
      
entryForm : Model -> Html Msg
entryForm model =
  let
    res = validateString model
  in
    div [ ] -- id "first" ]
      [ input
          [ type' "text",
            placeholder "{( () )}",
            value model.expression,
            name "phrase",
            autofocus True,
            onInput UpdateExpression,
            -- Utils.onInput UpdateExpression,
            strStyle
          ]
          [ ],
        --button [ class "change" ] [ text "Change" ],
        h2
          [ revStyle]
          [ text (model.expression ++ isValid res) ],
        stackHeader model.showStack res.stack,
        stackList model.showStack res.stack 
      ]

isStackEmpty : SStack -> String
isStackEmpty s = 
  if String.length s == 0 then 
    "Empty"
  else
    ""

isValid : Model -> String 
isValid bm = 
  let 
    {expression, stack, isValid, isBalanced} = bm
  in 
    case (isBalanced, isValid) of 
      (True, True)   -> " is valid"
      (False, _)     -> " is imbalanced"
      (_, False)     -> " is invalid"

getIndexedCharacters : String -> List (Int, Char)
getIndexedCharacters =
  List.indexedMap (,) << String.toList

stackList : Bool -> SStack -> Html Msg
stackList display stack = 
  let
    entryItems = 
      String.reverse stack 
        |> getIndexedCharacters 
        |> List.reverse
    items = 
      if display then 
        List.map stackItem (entryItems ++ [(-1, '-')] )
      else 
        []

  in
    div [ ] 
    [
      ul [ ] items,
      footer
        [ ] [a [href "https://github.com/kgashok/elmo-bracket/issues/new", 
                target "_blank", 
                rel "noopener noreferrer"] 
            [text version] ]
    ]

entryList : Bool -> List BPair -> Html Msg
entryList display entries =
  let
    entryItems = 
      if display then List.map entryItem entries else []
  in
    --ul [ bracStyle] items
     ul [ ] entryItems

-- view : Address Action -> Model -> Html
view : Model -> Html Msg
view model =
  
  div 
    [ id "container" ]
    [ pageHeader,

      div [id "wrapper"] 
      [ div [id "first"] 
          [ entryForm model ],

        div [id "second"]
          [ bracketHeader model.showBracket,
            entryList model.showBracket model.bmap,
            pageFooter ]
      ]

    ]


initialModel : Model
initialModel =
  { stack      = Stack.empty,
    isBalanced = True, 
    isValid    = True,
    
    expression = "", 
    
    bmap = 
      [
        newPair '(' ')' True 1,
        newPair '{' '}' True 2, 
        newPair '<' '>' True 3
      ], 
    showStack = True,
    showBracket = True
  }


-- WIRE IT ALL TOGETHER!

init : (Model, Cmd Msg)
init =
  (initialModel , Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Mouse.clicks MouseMsg
    , Keyboard.presses KeyMsg
    ]

main : Program Never
main =
  Html.program
    { init = init, update = update, view = view, subscriptions = subscriptions}
  -- Html.beginnerProgram
    -- { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


title : String -> Int -> Html Msg
title message times =
  message ++ " "
    |> toUpper
    |> repeat times
    |> trimRight
    |> text


pageHeader : Html Msg
pageHeader =
  h1 [ ] [ title "Validator" 1 ]


bracketHeader : Bool -> Html Msg
bracketHeader display =
  if display then 
    h2 [ ] [ title "Bracket Map" 1 ]
  else 
    h2 [ ] [ ] 

stackHeader : Bool -> SStack -> Html Msg         
stackHeader display stack =
  if display then 
    h3 [ ] [text ( "Stack " ++ (isStackEmpty stack) )]
  else 
    h3 [ ] [ ]

pageFooter : Html Msg
pageFooter =
  footer
    [ ] 
    [ a [ href "http://edu.kgisl.com", target "_blank", rel "noopener noreferrer" ] 
      [ text "The Campus Inside" ] 
    ]


strStyle : Attribute x
strStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
    
revStyle : Attribute x
revStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    , ("color", "red")
    ]

bracStyle : Attribute x
bracStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2")
    , ("text-align", "center")
    , ("color", "#f60")
    ]
