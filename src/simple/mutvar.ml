(*
 *   DOES NOT YET WORK
 *
 *   See bolow
 *
 *)

module ST:
sig
  type 'a mv
  type 'a t

  val return:   'a -> 'a t
  val (>>=):    'a t -> ('a -> 'b t) -> 'b t
  val ( let* ): 'a t -> ('a -> 'b t) -> 'b t

  val var:    'a    -> 'a mv t
  val get:   'a mv -> 'a t
  val put:  'a mv -> 'a -> unit t

  val run: 'a t -> 'a
end
=
struct
  type 'a mv = 'a ref


  type 'a t = unit -> 'a


  let return (a: 'a): 'a t =
    fun _ -> a


  let (>>=) (m: 'a t) (f: 'a -> 'b t): 'b t =
    fun _ ->
    let a = m () in
    f a ()


  let ( let* ) = (>>=)


  let var (a: 'a): 'a mv t =
    fun _ -> ref a


  let get (mv: 'a mv): 'a t =
    return !mv


  let put (mv: 'a mv) (a: 'a): unit t =
    return (mv := a)


  let run (m: 'a t): 'a =
    m ()



  (* Unit Test *)

  let swap (v1: 'a mv) (v2: 'a mv): unit t =
    let* a = get v1 in
    let* b = get v2 in
    let* _ = put v1 b in
    put v2 a

  let%test _ =
    run (
      let* v1 = var 1 in
      let* v2 = var 2 in
      let* _  = swap v1 v2 in
      let* a  = get v1 in
      let* b  = get v2 in
      return (a, b)
    )
    =
    (2, 1)


  let%test _ =
    (*let open Printf in *)
    let v1 = run (var 1) in
    let a  = run (get v1) in
    let _  = run (put v1 2) in
    let b  = run (get v1) in
    (* printf "a %d, b %d\n" a b;*)
    (a, b) = (1, 2)     (* WRONG!!! It should be [(1, 1)] *)
end
