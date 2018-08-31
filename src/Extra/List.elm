module Extra.List exposing (cons, dropLast, last, partition, penultimate, zip)

import List exposing (append, drop, head, isEmpty, map2, reverse, take)


dropLast : Int -> List a -> List a
dropLast n =
    reverse >> drop n >> reverse


penultimate : List a -> Maybe a
penultimate =
    reverse >> drop 1 >> head


last : List a -> Maybe a
last =
    reverse >> head


zip : List a -> List b -> List ( a, b )
zip =
    map2 (\a b -> ( a, b ))


cons : a -> List a -> List a
cons =
    (::)


partition : Int -> List a -> List (List a)
partition n =
    let
        partition_ parts list =
            if isEmpty list then
                parts

            else
                partition_ (append parts [ take n list ]) (drop n list)
    in
    partition_ []
