module Datum where 

type alias BPair = {
  opener: Char, 
  closer: Char,
  isEnabled: Bool,
  id: Int
}

type alias BMap = List BPair

type alias Model =
  {
    expression: String, 
    bmap: BMap 
  }

 
newPair : Char -> Char -> BPair
newPair op cl =
  { opener = op,
    closer = cl,
    isEnabled = True,
    id = 0
  } 

initialModel : Model
initialModel =
  { expression = "", 
    bmap = 
      [
        newPair '(' ')',
        newPair '{' '}' 
      ]
  }