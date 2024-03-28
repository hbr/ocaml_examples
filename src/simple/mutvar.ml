module ST:
sig
  type 'a mv
  type 'a t

  val return:   'a -> 'a t
  val (>>=):    'a t -> ('a -> 'b t) -> 'b t
  val ( let* ): 'a t -> ('a -> 'b t) -> 'b t

  val new_var:    'a    -> 'a mv t
  val read_var:   'a mv -> 'a t
  val write_var:  'a mv -> 'a -> unit t

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


  let new_var (a: 'a): 'a mv t =
    return (ref a)


  let read_var (mv: 'a mv): 'a t =
    return !mv


  let write_var (mv: 'a mv) (a: 'a): unit t =
    return (mv := a)


  let run (m: 'a t): 'a =
    m ()



  (* Unit Test *)

  let swap (v1: 'a mv) (v2: 'a mv): unit t =
    let* a = read_var v1 in
    let* b = read_var v2 in
    let* _ = write_var v1 b in
    write_var v2 a

  let%test _ =
    run (
      let* v1 = new_var 1 in
      let* v2 = new_var 2 in
      let* _  = swap v1 v2 in
      let* a  = read_var v1 in
      let* b  = read_var v2 in
      return (a, b)
    )
    =
    (2, 1)
end
