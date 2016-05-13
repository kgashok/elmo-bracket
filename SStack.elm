module SStack where 

import String 


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
