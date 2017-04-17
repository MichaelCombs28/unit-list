module UnitList
    exposing
        ( UnitList
        , singleton
        , isSingleton
        , push
        , fromList
        , toList
        , current
        , history
        , future
        , next
        , previous
        , step
        , initialize
        , archive
        , updateCurrent
        , updateHistory
        , updateFuture
        , map
        , map2
        , map3
        , map4
        , map5
        )

{-| An Elm library for lists with at least a single element.

# Type
A UnitList consists of a focus, history, and past.
The focus is the current element selected, the history is a set of previous elements and the future
are elements that have yet to be traversed.
@docs UnitList

# Basics
@docs singleton, push, isSingleton, fromList, toList

# Running
@docs current, history, future

# Traversing
@docs next, previous, step

# Manipulation
@docs initialize, archive, updateCurrent, updateHistory, updateFuture

# Mapping
@docs map, map2, map3, map4, map5

-}


{-| The main type.
-}
type UnitList a
    = UnitList (List a) a (List a)


{-| Create a unitlist for a given type taking the
argument as the current focus.

```
init : UnitList Int
init =
    singleton 0
```

-}
singleton : a -> UnitList a
singleton a =
    UnitList [] a []

{-| Checks if the UnitList contains a focus with an empty history and past.

```
isSingleton (singleton 5) == True
```

-}
isSingleton : UnitList a -> Bool
isSingleton (UnitList a _ b) =
    List.isEmpty a && List.isEmpty b


{-| Pushes an element onto the queue.

```
singleton 1
    |> push 2 == Unitlist [] 1 [2]
```

-}
push : a -> UnitList a -> UnitList a
push a (UnitList h f f_) =
    UnitList h f (f_ ++ [a])


{-| Creates a unitlist from an initial focus and a list.
-}
fromList : a -> List a -> UnitList a
fromList =
    UnitList []


{-| Combines past focus and future into a List
-}
toList : UnitList a -> List a
toList (UnitList a b c) =
    a ++ [ b ] ++ c


{-| Retrieves the current focus.

```
current (singleton 1) == 1
```

-}
current : UnitList a -> a
current (UnitList _ a _) =
    a


{-| Retrieves history.

```
history (singleton 1) == []
```

-}
history : UnitList a -> List a
history (UnitList a _ _) =
    a


{-| Retrieves the future.

```
future (push 5 <| singleton 1) == [5]
```

-}
future : UnitList a -> List a
future (UnitList _ _ a) =
    a

{-| Initializes the list changing the focus to the head of
    the history if there is a history.

```
initialize (push 5 <| singleton 1) == UnitList [] 1 [5]
```

-}
initialize : UnitList a -> UnitList a
initialize ((UnitList a b c) as ulist) =
    case a of
        [] ->
            ulist

        x :: xs ->
            UnitList [] x (xs ++ [ b ] ++ c)


{-| Sets a new focus and archives the current unitlist
into the history

```
archive 5 (singleton 1) == Unitlist [5] 1 []
```
-}
archive : a -> UnitList a -> UnitList a
archive a (UnitList h f f_) =
    UnitList (h ++ [f] ++ f_) a []


{-| Lift an endomorphism function to the focus.

```
updateCurrent ((+) 5) (singleton 1) == singleton 6
```

-}
updateCurrent : (a -> a) -> UnitList a -> UnitList a
updateCurrent f (UnitList a b c) =
    UnitList a (f b) c


{-| Lift a function to the history.

```
singleton 1
    |> push 2
    |> archive 3
    |> updateHistory (List.map ((+) 1))
    |> history == [2,3]
```

-}
updateHistory : (List a -> List a) -> UnitList a -> UnitList a
updateHistory f (UnitList a b c) =
    UnitList (f a) b c


{-| Lift a function to the future.

```
singleton 1
    |> push 2
    |> updateFuture ((+) 1)
    |> future == [3]
```

-}
updateFuture : (List a -> List a) -> UnitList a -> UnitList a
updateFuture f (UnitList a b c) =
    UnitList a b (f c)


{-| Steps to the head of the future list unless it's
empty which case returns Nothing.

```
singleton 1
    |> push 2
    |> next == UnitList [1] 2 []
```

-}
next : UnitList a -> Maybe(UnitList a)
next ((UnitList past curr future) as ulist) =
    case future of
        [] ->
            Nothing

        x :: xs ->
            UnitList (past ++ [ curr ]) x xs
                |> Just


{-| Cycles to the last element of the history
and adds the focus to the future unless it's empty
which case the identity is returned.

```
singleton 1
    |> push 2
    |> archive
    |> previous == UnitList [] 1 [2]
```

-}
previous : UnitList a -> Maybe(UnitList a)
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
                    |> Just

            _ ->
                Nothing


{-| Cycles forward or backward through the Unitlist,
    if n == 0 then the result is returned. If there
    are more elements than cycles, nothing is returned.
-}
step : Int -> UnitList a -> Maybe(UnitList a)
step n ulist =
    if n > 0 then
        next ulist
            |> Maybe.andThen (step (n - 1))
    else if n < 0 then
        previous ulist
            |> Maybe.andThen (step (n + 1))
    else
        Just ulist


{-| Transforms the model leaving the structure intact.
-}
map : (a -> b) -> UnitList a -> UnitList b
map f (UnitList a b c) =
    UnitList (List.map f a) (f b) (List.map f c)


{-| Combines 2 unitlists with a function.
-}
map2 : (a -> b -> c) -> UnitList a -> UnitList b -> UnitList c
map2 f (UnitList a b c) (UnitList d e f_) =
    UnitList (List.map2 f a d) (f b e) (List.map2 f c f_)


{-|
-}
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


{-|
-}
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


{-|
-}
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
