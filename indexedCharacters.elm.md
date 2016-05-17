```elm
import Html exposing (text)
import String exposing (toList)
import List exposing (indexedMap)

sample = "abcdefghijklmnopqrstuvwxyz"
alist  = String.split "" sample 
ailist = List.indexedMap (,) alist

getIndexedCharacters : String -> List (Int, Char)
getIndexedCharacters =
  indexedMap (,) << toList

main =
  sample 
    |> getIndexedCharacters 
    |> toString 
    |> text 
    
-- text ("Hello, World!\n" ++ (toString ailist) )


```
