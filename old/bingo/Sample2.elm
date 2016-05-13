module Sample2 where

import SStack as Stack exposing (..)
import Html exposing (..)

reverseString : String -> String
reverseString str =
  Stack.reverse str 


main : Html.Html
main =
  reverseString "Hello World !" 
    |> Html.text
