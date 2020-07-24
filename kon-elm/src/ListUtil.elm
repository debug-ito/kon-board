module ListUtil exposing
    ( replaceOrAdd
    , join
    , last
    , blocks
    , changes
    , listToMaybe
    )

import List exposing (foldr)
import List

{- | Extra utility about List -}

{- | Insert a new item to the list. If the original list already
  contains similar items (the ones that the predicate returns 'True'),
  it replaces those items with the new item. If none of the elements
  in the list are not similar to the new item, it prepends the new
  item to the list.
-}
replaceOrAdd : (a -> Bool) -> a -> List a -> List a
replaceOrAdd pred new_item old_list =
    let result = finalize <| foldr f ([], False) old_list
        f cur_item (acc, replaced) =
            if pred cur_item
            then (new_item :: acc, True)
            else (cur_item :: acc, replaced)
        finalize (ret, replaced) =
            if replaced
            then ret
            else (new_item :: ret)
    in result

join : String -> List String -> String
join sep words = List.foldr (++) "" <| List.intersperse sep words

last : List a -> Maybe a
last l =
    case l of
        [] -> Nothing
        [x] -> Just x
        (_ :: rest) -> last rest

{- | Group elements in the given list with fixed size.
-}
blocks : Int -> List a -> List (List a)
blocks size input =
    if size <= 0
    then []
    else let result = finalize <| List.foldr f ([], []) input
             f cur_elem (cur_group, acc) =
                 if List.length cur_group == size
                 then ([cur_elem], cur_group :: acc)
                 else (cur_elem :: cur_group, acc)
             finalize (group, acc) = if List.length group == 0
                                     then acc
                                     else group :: acc
         in result

{- | Traverse the list items from the head and returns items that are
different from the previous one.
-}
changes : List a -> List a
changes input =
    let result =
            case input of
                [] -> []
                (head_a :: rest) -> List.reverse <| go head_a rest []
        go head_a rest acc =
            case rest of
                [] -> acc
                (next_a :: next_rest) ->
                    if head_a == next_a
                    then go next_a next_rest acc
                    else go next_a next_rest (next_a :: acc)
    in result

{- | If the list has at least one element, it returns the head. If the
list is empty, it returns 'Nothing'.
-}
listToMaybe : List a -> Maybe a
listToMaybe l =
    case l of
        [] -> Nothing
        (a :: _) -> Just a
