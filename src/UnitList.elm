module UnitList exposing (..)


type UnitList a
    = UnitList (List a) a (List a)


singleton : a -> UnitList a
singleton a =
    UnitList [] a []


isSingleton : UnitList a -> Bool
isSingleton (UnitList a _ b) =
    List.isEmpty a && List.isEmpty b


fromList : a -> List a -> UnitList a
fromList =
    UnitList []


toList : UnitList a -> List a
toList (UnitList a b c) =
    a ++ [ b ] ++ c


current : UnitList a -> a
current (UnitList _ a _) =
    a


history : UnitList a -> List a
history (UnitList a _ _) =
    a


future : UnitList a -> List a
future (UnitList _ _ a) =
    a


updateCurrent : (a -> a) -> UnitList a -> UnitList a
updateCurrent f (UnitList a b c) =
    UnitList a (f b) c


updateHistory : (List a -> List a) -> UnitList a -> UnitList a
updateHistory f (UnitList a b c) =
    UnitList (f a) b c


updateFuture : (List a -> List a) -> UnitList a -> UnitList a
updateFuture f (UnitList a b c) =
    UnitList a b (f c)


initialize : UnitList a -> UnitList a
initialize ((UnitList a b c) as ulist) =
    case a of
        [] ->
            ulist

        x :: xs ->
            UnitList [] x (xs ++ [ b ] ++ c)


next : UnitList a -> UnitList a
next ((UnitList past curr future) as ulist) =
    case future of
        [] ->
            ulist

        x :: xs ->
            UnitList (past ++ [ curr ]) x xs


previous : UnitList a -> UnitList a
previous ((UnitList past curr future) as ulist) =
    let
        past_ =
            List.take (List.length past - 1) past

        cur_ =
            List.drop (List.length past_ - 1) past
    in
        case cur_ of
            a :: x ->
                UnitList past_ a (curr :: future)

            _ ->
                ulist


step : Int -> UnitList a -> UnitList a
step n ulist =
    if n > 0 then
        step (n - 1) <| next ulist
    else if n < 0 then
        step (n + 1) <| previous ulist
    else
        ulist


map : (a -> b) -> UnitList a -> UnitList b
map f (UnitList a b c) =
    UnitList (List.map f a) (f b) (List.map f c)


map2 : (a -> b -> c) -> UnitList a -> UnitList b -> UnitList c
map2 f (UnitList a b c) (UnitList d e f_) =
    UnitList (List.map2 f a d) (f b e) (List.map2 f c f_)


map3 :
    (a -> b -> c -> d)
    -> UnitList a
    -> UnitList b
    -> UnitList c
    -> UnitList d
map3 f (UnitList a b c) (UnitList d e f_) (UnitList g h i) =
    UnitList
        (List.map3 f a d g)
        (f b e h)
        (List.map3 f c f_ i)


map4 :
    (a -> b -> c -> d -> e)
    -> UnitList a
    -> UnitList b
    -> UnitList c
    -> UnitList d
    -> UnitList e
map4 f (UnitList a b c) (UnitList d e f_) (UnitList g h i) (UnitList j k l) =
    UnitList
        (List.map4 f a d g j)
        (f b e h k)
        (List.map4 f c f_ i l)


map5 :
    (a -> b -> c -> d -> e -> f)
    -> UnitList a
    -> UnitList b
    -> UnitList c
    -> UnitList d
    -> UnitList e
    -> UnitList f
map5 f (UnitList a b c) (UnitList d e f_) (UnitList g h i) (UnitList j k l) (UnitList m n o) =
    UnitList
        (List.map5 f a d g j m)
        (f b e h k n)
        (List.map5 f c f_ i l o)
