module Crdt exposing (..)

-- Conflict Free Replicated Data Types (CRDTs) are the fundamental data
-- structure used to represent data transferred over the network. CRDTs are
-- special structures! This is because they are able to be transformed by
-- separate parties and then recombined later without losing any
-- information. This is clearly useful when building distributed applications.
--
-- So, how do they work?
--
-- The key properties of the structures are commutativity, associativity, and
-- idempotence. Commutativity and associativity ensure that no matter what order
-- updates come in, the resulting state is the same. Idempotence ensures that if
-- an update is seen twice it does not change the result.
--
-- Our structures are built around a particular CRDT type: the append-only
-- set. This is a special case of the general Set, where elements can only be
-- added; we will see how to combine these to get back some of the more general
-- Set properties.

import Set exposing (Set)
import Tuple exposing (first, second)



-- We define a type wrapper to give more control over what operations can be
-- used on our restricted Set type.


type AppendOnlySet comparable
    = AppendOnlySet (Set comparable)


emptyAOS : AppendOnlySet comparable
emptyAOS =
    AppendOnlySet Set.empty



-- Inserting a value is just the standard Set operation.


insertAOS : comparable -> AppendOnlySet comparable -> AppendOnlySet comparable
insertAOS elt set =
    case set of
        AppendOnlySet s ->
            AppendOnlySet (Set.insert elt s)



-- We can do the same with most of the other main operations on Set.


memberAOS : comparable -> AppendOnlySet comparable -> Bool
memberAOS elt set =
    case set of
        AppendOnlySet s ->
            Set.member elt s


sizeAOS : AppendOnlySet comparable -> Int
sizeAOS set =
    case set of
        AppendOnlySet s ->
            Set.size s


isEmptyAOS : AppendOnlySet comparable -> Bool
isEmptyAOS set =
    case set of
        AppendOnlySet s ->
            Set.isEmpty s


foldlAOS : (comparable -> b -> b) -> b -> AppendOnlySet comparable -> b
foldlAOS f acc set =
    case set of
        AppendOnlySet s ->
            Set.foldl f acc s



-- The elements of a CRDT set must be comparable, such that if two elements are
-- "equal" they are also equivalent to each other. This is important to get
-- right, but it's not enough: one of the things that we need to be sure about
-- is that we can identify updates uniquely. To do so we will define a Sequence
-- of updates, where the items are differentiated by a unique, monotonic integer
-- identifier.


type alias Sequence comparable =
    ( Int, comparable )



-- We can reduce an AppendOnlySet of Sequenced values to the latest value:


latest : AppendOnlySet (Sequence comparable) -> Maybe comparable
latest s =
    let
        ltst =
            foldlAOS
                (\x ->
                    \acc ->
                        case acc of
                            Nothing ->
                                Just x

                            Just y ->
                                if first y > first x then
                                    Just y

                                else
                                    Just x
                )
                Nothing
                s
    in
    case ltst of
        Nothing ->
            Nothing

        Just a ->
            Just (second a)


seqNum : AppendOnlySet (Sequence comparable) -> Maybe Int
seqNum s =
    let
        ltst =
            foldlAOS
                (\x ->
                    \acc ->
                        case acc of
                            Nothing ->
                                Just x

                            Just y ->
                                if first y > first x then
                                    Just y

                                else
                                    Just x
                )
                Nothing
                s
    in
    case ltst of
        Nothing ->
            Nothing

        Just a ->
            Just (first a)



-- Assuming that our sets start empty and only have one writer, we can then use
-- an append-only set of a sequence type as a CRDT. If there are multiple
-- writers they may have a collision in the sequence numbers, so watch out! This
-- type works best for items where there is a natural ordering, like for string
-- values. For other applications, such as counters, it makes more sense to look
-- at the number of elements in a set and ignore the actual values.


type alias Counter comparable =
    { up : AppendOnlySet comparable
    , down : AppendOnlySet comparable
    }


zero : Counter comparable
zero =
    { up = emptyAOS, down = emptyAOS }


increment : comparable -> Counter comparable -> Counter comparable
increment elt c =
    { c | up = insertAOS elt c.up }


decrement : comparable -> Counter comparable -> Counter comparable
decrement elt c =
    { c | down = insertAOS elt c.down }


count : Counter comparable -> Int
count c =
    sizeAOS c.up - sizeAOS c.down
