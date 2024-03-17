module String_map = Fmlib_std.Btree.Map (String)


module Option =
struct
  include Fmlib_std.Option


  let (>=>) (fab: 'a -> 'b t) (fbc: 'b -> 'c t): ('a -> 'c t) =
    fun a -> fab a >>= fbc
end


module Expr =
struct
  type t =
    | Var of String.t
    | App of t * t
end


module Expr_map =
struct
  type 'a t =
    'a t1 option

  and  'a t1 =
    {
      vars: 'a String_map.t;
      apps: 'a t t
    }


  let vars (n: 'a t1): 'a String_map.t =
    n.vars


  let apps (n: 'a t1): 'a t t =
    n.apps


  let empty: 'a t =
    None



  let rec find_opt: type a. Expr.t -> a t -> a option =
    let open Option
    in
    function
    | Var s ->
      map vars >=> String_map.find_opt s

    | App (f, a) ->
      map apps >=> find_opt f >=> find_opt a
end
