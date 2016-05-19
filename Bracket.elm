module Bracket where 

import Html exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import Html.Attributes exposing (..)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String exposing (..)
import Dict exposing (fromList, get)
import List.Extra as Listx exposing (find)


import SStack as Stack exposing (..)
import BingoUtils as Utils 

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
    isValid: Bool       -- the ultimate outcome! 
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
    _= Debug.watch "Result " (res.isValid, res.stack, res.expression)
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

stackItem : (Int, Char) -> Html 
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


entryItem : Address Action -> BPair -> Html
entryItem address entry =
  li   
    [ classList [ ("highlight", entry.isEnabled) ],
      onClick address (Mark entry.id)
    ]
    [ span [ class "phrase" ] [ text (String.fromChar entry.opener) ],
      span [ class "points" ] [ text (String.fromChar entry.closer) ]
      -- span [][text "   - closer"] 
      -- button [ class "delete", onClick address (Delete entry.id) ] []
    ]
      
entryForm : Address Action -> Model -> Html
entryForm address model =
  let
    res = validateString model
  in
    div [id "second" ]
      [ input
          [ type' "text",
            placeholder "{( () )}",
            value model.expression,
            name "phrase",
            autofocus True,
            Utils.onInput address UpdateExpression,
            strStyle
          ]
          [ ],
        --button [ class "change" ] [ text "Change" ],
        h2
          [ revStyle]
          [ text (model.expression ++ isValid res) ],
        h3 
          [] [text ( "Stack " ++ (isStackEmpty res.stack) )],
        stackList res.stack 
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

stackList : SStack -> Html 
stackList stack = 
  let 
    entryItems = 
      String.reverse stack 
        |> getIndexedCharacters 
        |> List.reverse
    items = List.map stackItem (entryItems ++ [(-1, '-')] ) 
  in
    div [ ] 
    [
      ul [ ] items
    ]

entryList : Address Action -> List BPair -> Html
entryList address entries =
  let
    entryItems = List.map (entryItem address) entries
    items = entryItems 
  in
    --ul [ bracStyle] items
     ul [ ] items

view : Address Action -> Model -> Html
view address model =
  
  div 
    [ id "container" ]
    [ pageHeader,

      div [id "wrapper"] 
      [ div [id "first"] 
          [ entryForm address model ],

        div [id "second"]
          [ bracketHeader,
            entryList address model.bmap,
            pageFooter ]
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
    stack = Stack.empty,
    isBalanced = True, 
    isValid = True
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
    --|> toUpper
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
  footer
    [ ]
    [ a [ href "http://edu.kgisl.com" ] [ text "The Campus Inside" ] ]


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
