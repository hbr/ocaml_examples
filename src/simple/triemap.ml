




(*
    ============================================================
    Basics
    ============================================================
*)

module String_map =
  Fmlib_std.Btree.Map (String)


module Int_map =
  Fmlib_std.Btree.Map (Int)


module Option =
struct
  include Fmlib_std.Option


  let (>=>) (fab: 'a -> 'b t) (fbc: 'b -> 'c t): ('a -> 'c t) =
    fun a -> fab a >>= fbc
end


type 'a upd =
  'a -> 'a


type 'a xt =
  'a option upd


let (>.>) (f: 'a -> 'b) (g: 'b -> 'c): 'a -> 'c =
  fun a -> a |> f |> g





(*
    ============================================================
    Triemap with Terms with Variables and Applications
    ============================================================
*)
module Simple =
struct
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


    let update_vars (f: 'a String_map.t upd): 'a t1 upd =
      fun n -> {n with vars = f n.vars}


    let update_apps (f: 'a t t upd): 'a t1 upd =
      fun n -> {n with apps = f n.apps}


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


    let rec update: type a.  Expr.t -> a xt -> a t upd =
      let open Option
      in
      function
      | Var s ->
        String_map.update s >.> update_vars >.> map

      | App (f, a) ->
        update a >.> map >.> update f >.> update_apps >.> map
  end
end










(*
    ============================================================
    Triemap with Terms with Binders
    ============================================================
*)
module With_binders =
struct
  module Term =
  struct
    type t =
      | Var of int
      | App of t * t
      | Lam of t
  end


  module TMap =
  struct
    type 'a t =
      'a t1 option

    and 'a t1 =
      {
        bvars: 'a Int_map.t;
        fvars: 'a Int_map.t;
        apps:  'a t t;
        lams:  'a t;
      }
  end
end
