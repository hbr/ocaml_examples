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
  type 'a xt =
    'a option -> 'a option


  type 'a t =
    'a t1 option

  and  'a t1 =
    {
      vars: 'a String_map.t;
      apps: 'a t t
    }


  (* 'a t t = 'a t t1 option
            = 'a t1 option t1 option
   *)


  let vars (n: 'a t1): 'a String_map.t =
    n.vars


  let apps (n: 'a t1): 'a t t =
    n.apps


  let update_vars (f: 'a String_map.t -> 'a String_map.t) (n: 'a t1): 'a t1 =
    {n with vars = f n.vars}


  let update_apps (f: 'a t t -> 'a t t) (n: 'a t1): 'a t1 =
    {n with apps = f n.apps}


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


  let (>.>) (f: 'a -> 'b) (g: 'b -> 'c): 'a -> 'c =
    fun a -> a |> f |> g


  let rec update: type a.  Expr.t -> a xt -> a t -> a t =
    let open Option
    in
    function
    | Var s ->
      String_map.update s >.> update_vars >.> map

    | App (f, a) ->
      update a >.> map >.> update f >.> update_apps >.> map
end
