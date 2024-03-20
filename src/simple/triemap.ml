




(*
================================================================================
Basics
================================================================================
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


  let lift (f: 'a t -> 'b t): 'a t t -> 'b t t = function
    | None ->
      Some (f None)

    | Some a ->
      Some (f a)
end



type 'a upd =
  'a -> 'a


type 'a xt =
  'a option upd


let (>.>) (f: 'a -> 'b) (g: 'b -> 'c): 'a -> 'c =
  (* (f >.> g) x  =  g (f x) *)
  fun x -> g (f x)


let (>=>) = Option.(>=>)


let omap = Option.map


let lift = Option.lift


let de_bruijn (n: int): int upd =
  fun i ->
  assert (i < n);
  n - i - 1






(*
================================================================================
Triemap with Terms with Variables and Applications
================================================================================
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


    let empty: 'a t =
      None


    let empty1: 'a t1 =
      {vars = String_map.empty; apps = None}


    let vars (n: 'a t1): 'a String_map.t =
      n.vars


    let apps (n: 'a t1): 'a t t =
      n.apps


    let update_vars (f: 'a String_map.t upd): 'a t upd =
      function
      | None ->
        Some {empty1 with vars = f String_map.empty}

      | Some n ->
        Some {n with vars = f n.vars}


    let update_apps (f: 'a t t upd): 'a t upd =
      function
      | None ->
        Some {empty1 with apps = f None}

      | Some n ->
        Some {n with apps = f n.apps}


    let rec find_opt: type a. Expr.t -> a t -> a option =
      function
      | Var s ->
        omap vars >=> String_map.find_opt s

      | App (f, a) ->
        omap apps >=> find_opt f >=> find_opt a


    let rec update: type a.  Expr.t -> a xt -> a t upd =
      function
      | Var s ->
        String_map.update s >.> update_vars

      | App (f, a) ->
        update a >.> lift >.> update f >.> update_apps
  end
end










(*
================================================================================
Triemap with Terms with Binders
================================================================================
*)
module With_binders =
struct
  module Term =
  struct
    type t =
      | Var of int
      | App of t * t
      | Lam of t


    let lam_true: t =
      Lam (Lam (Var 1))


    let lam_false: t =
      Lam (Lam (Var 0))

    let pair: t =
      Lam (Lam (Lam (App (App (Var 0, Var 2), Var 1))))
  end


  module TMap =
  struct
    type 'a t =
      'a t1 option

    and 'a t1 =
      {
        vars: 'a Int_map.t;
        apps:  'a t t;
        lams:  'a t;
      }


    let empty: 'a t =
      None


    let empty1: 'a t1 =
      { vars = Int_map.empty; apps = empty; lams = empty }


    let vars (n: 'a t1): 'a Int_map.t =
      n.vars


    let apps (n: 'a t1): 'a t t =
      n.apps


    let lams (n: 'a t1): 'a t =
      n.lams


    let update_vars (f: 'a Int_map.t upd): 'a t upd =
      function
      | None ->
        Some {empty1 with vars = f Int_map.empty}

      | Some n ->
        Some {n with vars = f n.vars}


    let update_apps (f: 'a t t upd): 'a t upd =
      function
      | None ->
        Some {empty1 with apps = f empty}

      | Some n ->
        Some {n with apps = f n.apps}


    let update_lams (f: 'a t upd): 'a t upd =
      function
      | None ->
        Some {empty1 with lams = f empty}

      | Some n ->
        Some {n with lams = f n.lams}


    let rec find_opt: type a. Term.t -> a t -> a option =
      function
      | Var i ->
        omap vars >=> Int_map.find_opt i

      | App (f, a) ->
        omap apps >=> find_opt f >=> find_opt a

      | Lam t ->
        omap lams >=> find_opt t


    let rec update: type a. Term.t -> a xt -> a t upd =
      function
      | Var i ->
        Int_map.update i >.> update_vars

      | App (f, a) ->
        update a >.> lift >.> update f >.> update_apps

      | Lam t ->
        update t >.> update_lams


    let insert (t: Term.t) (v: 'a): 'a t -> 'a t =
      update t (fun _ -> Some v)




    (* Test Support
     * --------------------
     *)
    let map: 'a t =
      empty
      |> insert Term.lam_true "true"
      |> insert Term.lam_false "false"
      |> insert Term.pair "pair"


    let check_lam (t: Term.t) (expect: string option) (map: string t): bool =
      let res = find_opt t map in
      (*begin
        let open Printf in
        match res with
        | None ->
          printf "None\n"
        | Some s ->
          printf "%s\n" s
      end;*)
      res = expect


    let check: bool =
      check_lam Term.lam_true (Some "true") map
      &&
      check_lam Term.lam_false (Some "false") map
      &&
      check_lam Term.pair (Some "pair") map
      &&
      check_lam Term.(Var 0) None map


    let%test _ =
      check
  end
end










(*
================================================================================
Triemap with Matching
================================================================================
*)

module Simple_match =
struct
  module Term =
  struct
    type t =
      | Var of int
      | App of t * t
  end


  type key = int * Term.t (* number of pattern variables, term
                             all variables above n are free variables *)


  module TMap =
  struct
    type 'a t

    let find: type a. Term.t -> a t -> (Term.t Int_map.t * a) list =
      function
      | Var _ ->
        assert false

      | App (_, _) ->
        assert false
  end
end










(*
================================================================================
Proofs
================================================================================
*)


(* Composition
================================================================================

    1. The composition operator is associative

        (f >.> g) >.> h   =   f >.> (g >.> h)


        Proof

            ((f >.> g) >.> h) x   =   (h >.> g) (f x)
                                  =   h (g (f x))

            (f >.> (g >.> h)) x   =  h ((g >.> f) x)
                                  =  h (g (f x))


    2. Kleisli composition does not associate with composition

        Note: h has to map the monad i.e. h: 'x m -> 'y m

        Counterexample: Consider

            (f >=> g) >.> h

        [f x] might return [None]. [g] is not executed and [None] is fed
        directly into [h] which might return something different from [None].

        Now consider

            f >=> (g >.> h)

        If [f x] returns [None], the complete result is [None] and [h] is not
        used.
*)
