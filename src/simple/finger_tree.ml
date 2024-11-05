(* Simple example. It does not yet contain sizes. *)


type 'a node =
    | Node2 of 'a * 'a
    | Node3 of 'a * 'a * 'a


type 'a digit = 'a list (* A buffer of one to four elements *)


type 'a tree =
    | Empty
    | Single of 'a
    | Deep of
          'a digit
          * 'a node tree
          * 'a digit


let rec push_front: type a. a -> a tree -> a tree =
    fun a -> function
        | Empty ->
            Single a

        | Single b ->
            Deep ([a], Empty, [b])

        | Deep ([b; c; d; e], m, sf) ->
            Deep (
                [a; b],
                (push_front (Node3 (c, d, e)) m),
                sf
            )

        | Deep (pr, m, sf) ->
            Deep ( a :: pr, m, sf)
