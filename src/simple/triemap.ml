




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


    let vars (m: 'a t): 'a String_map.t option =
      Option.map (fun n -> n.vars) m


    let apps (m: 'a t): 'a t t option =
      Option.map (fun n -> n.apps) m


    let update_vars (xt: 'a String_map.t upd): 'a t upd =
      function
      | None ->
        Some {empty1 with vars = xt String_map.empty}

      | Some n ->
        Some {n with vars = xt n.vars}


    let update_apps (xt: 'a t t upd): 'a t upd =
      function
      | None ->
        Some {empty1 with apps = xt None}

      | Some n ->
        Some {n with apps = xt n.apps}


    let rec find_opt: type a. Expr.t -> a t -> a option =
      function
      | Var s ->
        vars >=> String_map.find_opt s

      | App (f, a) ->
        apps >=> find_opt f >=> find_opt a


    let rec update: type a.  Expr.t -> a xt -> a t upd =
      function
      | Var s ->
        String_map.update s >.> update_vars

      | App (f, a) ->
        update a >.> lift >.> update f >.> update_apps


    (* just for type checking *)
    let lhs (e: Expr.t) (xt: 'a xt): 'a t -> 'a option =
      update e xt >.> find_opt e

    let rhs (e: Expr.t) (xt: 'a xt): 'a t -> 'a option =
      find_opt e >.> xt
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


    let vars (m: 'a t):  'a Int_map.t option=
      Option.map (fun n -> n.vars) m


    let apps (m: 'a t): 'a t t option =
      Option.map (fun n -> n.apps) m


    let lams (m: 'a t): 'a t option =
      Option.map (fun n -> n.lams) m


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
        vars >=> Int_map.find_opt i

      | App (f, a) ->
        apps >=> find_opt f >=> find_opt a

      | Lam t ->
        lams >=> find_opt t


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


    3. Function compositioin associates with Kleisli composition

            f >.> (g >=> h)  =  (f >.> g) >=> h

        Proof

            (f >.> (g >=> h)) x  =  (g >=> h) (f x)
                                 =  g (f x) >>= h

            ((f >.> g) >=> h) x  =  (f >.> g) x >>= h
                                 =  g (f x) >>= h
*)


(* Correctness of [find] and [update]
================================================================================

    We use the simple triemap with variables and applications.

    We want to prove that an update operation followed by a find returns the
    expected result

        upd e xt >.> fnd e   =   fnd e >.> xt

    and assume the correctness of the string map

        SM.(upd s xt >.> fnd s)   =   SM.(fnd s >.> xt)


    Proof by induction on the structure of [e]:


    1. Base case [e = Var s]


        Left hand side:

        (upd (Var s) xt >.> fnd e) m

            | definition of (>.>)

        = fnd (Var s) (upd (Var s) xt m)

            | definition of [upd]

        = fnd (Var s) (upd_vars (SM.upd s xt) m)

            | definition of [fnd]

        = omap vars (upd_vars (SM.upd s xt) m) >>= SM.find s

            |    omap vars (upd_vars (SM.upd s xt) m
            |    = Some(SM.upd s xt sm)
            |
            | where sm is either the empty string map or the string map within
            | m

        = SM.(upd s xt >.> find s) sm

            | Correctness of string map

        = SM.(find s >.> xt) sm


        Right hand side

        (fnd (Var s) >.> xt) m

            | definition of [fnd]

        = (omap vars >=> SM.find s >.> xt) m

            | definition Kleisi composition

        = xt (omap vars m >>= SM.find s)

            | If [m] is empty then we get [xt None]
            | Otherwise we get [xt (SM.find s sm)]
            | In both case the result can be represented by

        = SM.(fnd s >.> xt) sm

            | where [sm] is either empty for empty [m] or the string map of [m].



    2. Inductive case [e = App (f, a)]


        Left hand side:

        (upd (App (f, a)) xt >.> fnd (App (f, a)) m

            | definition [upd, fnd]

        = ((upd a >.> lift >.> upd f >.> upd_apps) xt
           >.> (map apps >=> fnd f >=> fnd a)) m

            | xt' := lift (upd a xt)
            | composition associates with Kleisli composition

        = ((upd f >.> upd_apps) xt' >.> map apps >=> fnd f) m >>= fnd a

            | definition Kleisli composition and function composition

        = map apps (upd_apps (upd f xt')) m >>= fnd f >>= fnd a

            | [af] is either [None] if [m] is [None] or the apps field of [m]

        = Some (upd f xt' af) >>= fnd f >>= fnd a

            | definition of (>>=)

        = fnd f (upd f xt' af) >>= fnd a

            | definition of composition

        = (upd f xt' >.> fnd f) af >>= fnd a

            | induction hypothesis of [f]

        = (fnd f >.> xt') af >>= fnd a

            | defintion xt'

        = lift (upd a xt) (fnd f af) >>= fnd a

            | definition of [lift]

        = Some (upd a xt (fnd f af)) >>= fnd a

            | definition of (>>=)

        = fnd a (upd a xt (fnd f af))

            | induction hypothesis of [a]

        = xt (fnd a (fnd f af))



        Right hand side:

        (fnd (App (f, a)) >.> xt) m

        = ((omap apps >=> fnd f >=> fnd a) >.> xt) m

        = xt (fnd f af >>= fnd a)



        PROBLEM: lhs and rhs are not exactly the same. lhs has one level of
        option less!!!
*)
