module BoundedSelection exposing (BoundedSet, empty, insert, remove, member)


type BoundedSet a
    = BoundedSet (List a)


empty : BoundedSet a
empty =
    BoundedSet []


insert : a -> BoundedSet a -> BoundedSet a
insert value (BoundedSet entries) =
    BoundedSet (List.take 2 (value :: entries))


remove : String -> BoundedSet -> BoundedSet
remove value (BoundedSet entries) =
    BoundedSet (List.filter (\entry -> entry /= value) entries)


member : String -> BoundedSet -> Bool
member value (BoundedSet entries) =
    List.member value entries
