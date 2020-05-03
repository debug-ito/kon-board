module ListUtil exposing
    ( replaceOrAdd
    )

import List exposing (foldr)

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
