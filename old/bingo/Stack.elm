module Stack where

type alias Stack a =
  { items : List a }


empty : Stack a
empty =
  Stack []


push : a -> Stack a -> Stack a
push tok stack =
  { stack | items = tok :: stack.items }


pop : Stack a -> Maybe (a, Stack a)
pop stack =
  case stack.items of
    (h :: r) ->
      Just (h, { stack | items = r })

    [] ->
      Nothing


reverse : Stack a -> Stack a
reverse stack =
  let
    listify s =
      case (pop s) of
        Nothing ->
          []

        Just (x, r) ->
          x :: listify r

    stackify newStack lst =
      case lst of
        (h :: r) ->
          stackify (push h newStack) r

        [] ->
          newStack
  in
    listify stack |> stackify empty


fromList : List a -> Stack a
fromList lst =
  Stack lst


toList : Stack a -> List a
toList stack =
  stack.items

