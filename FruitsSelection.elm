module FruitsSelection exposing (..)


type SelectedFruits
    = SelectedFruits Int (List String)


empty : Int -> SelectedFruits
empty maxSize =
    SelectedFruits maxSize []


insert : String -> SelectedFruits -> SelectedFruits
insert fruit (SelectedFruits maxSize fruits) =
    SelectedFruits maxSize (List.take maxSize (fruit :: fruits))


remove : String -> SelectedFruits -> SelectedFruits
remove fruit (SelectedFruits maxSize fruits) =
    SelectedFruits maxSize (List.filter (\f -> f /= fruit) fruits)


member : String -> SelectedFruits -> Bool
member fruit (SelectedFruits _ fruits) =
    List.member fruit fruits
