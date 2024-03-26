(*
================================================================================
Basics
================================================================================
*)

module type ANY = Fmlib_std.Interfaces.ANY


module String_map =
  Fmlib_std.Btree.Map (String)


module Int_map =
  Fmlib_std.Btree.Map (Int)


module Unit_map =
  Fmlib_std.Btree.Map (Unit)


module Array =
  Fmlib_std.Array


module Option =
struct
  include Fmlib_std.Option


  let (>=>) (fab: 'a -> 'b t) (fbc: 'b -> 'c t): ('a -> 'c t) =
    fun a -> fab a >>= fbc


  let get: 'a option -> 'a =
    function
    | None ->
      assert false (* Illegal call *)

    | Some a ->
      a

  let to_list: 'a option -> 'a list =
    function
    | None   -> []
    | Some a -> [a]


  let elim (def: 'r) (f: 'a -> 'r): 'a option -> 'r =
    function
    | None ->
      def

    | Some a ->
      f a
end



module StateM (S: ANY) =
struct
  type 'a t =
    S.t -> 'a * S.t


  let return: 'a -> 'a t =
    fun a s -> (a, s)


  let bind (m: 'a t) (f: 'a -> 'b t): 'b t =
    fun s ->
    let a, s = m s in
    f a s


  let (>>=) = bind


  let ( let* ) = bind


  let run (s: S.t) (m: 'a t): 'a * S.t =
    m s
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
      {
        vars: 'a String_map.t;
        apps: 'a t t option
      }


    let empty: 'a t =
      {vars = String_map.empty; apps = None}


    let is_empty (m: 'a t): bool =
      String_map.is_empty m.vars
      &&
      m.apps = None


    let vars (m: 'a t): 'a String_map.t =
      m.vars


    let apps (m: 'a t): 'a t t option =
      m.apps


    let update_vars (xt: 'a String_map.t upd): 'a t upd =
      fun m -> {m with vars = xt m.vars}


    let update_apps (xt: 'a t t xt): 'a t upd =
      fun m -> {m with apps = xt m.apps}


    let to_option (m: 'a t): 'a t option =
      if is_empty m then
        None
      else
        Some m


    let lift (xt: 'a t upd): 'a t xt =
      function
      | None ->
        xt empty |> to_option

      | Some m ->
        xt m |> to_option


    let rec find_opt: type a. Expr.t -> a t -> a option =
      function
      | Var s ->
        vars >.> String_map.find_opt s

      | App (f, a) ->
        apps >=> find_opt f >=> find_opt a


    let rec update: type a.  Expr.t -> a xt -> a t upd =
      function
      | Var s ->
        String_map.update s >.> update_vars

      | App (f, a) ->
        update a >.> lift >.> update f >.> lift >.> update_apps


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


    (* Some example terms: *)

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
      {
        vars: 'a Int_map.t;
        apps:  'a t t Unit_map.t;
        lams:  'a t Unit_map.t;
      }


    let empty: 'a t =
      {
        vars = Int_map.empty;
        apps = Unit_map.empty;
        lams = Unit_map.empty
      }


    let is_empty (m: 'a t): bool =
      Int_map.is_empty m.vars
      &&
      Unit_map.is_empty m.apps
      &&
      Unit_map.is_empty m.lams


    let vars (m: 'a t):  'a Int_map.t =
      m.vars


    let apps (m: 'a t): 'a t t Unit_map.t =
      m.apps


    let lams (m: 'a t): 'a t Unit_map.t =
      m.lams


    let update_vars (xt: 'a Int_map.t upd): 'a t upd =
      fun m -> {m with vars = xt m.vars}


    let update_apps (xt: 'a t t Unit_map.t upd): 'a t upd =
      fun m -> {m with apps = xt m.apps}


    let update_lams (xt: 'a t Unit_map.t upd): 'a t upd =
      fun m -> {m with lams = xt m.lams}


    let to_option (m: 'a t): 'a t option =
      if is_empty m then
        None
      else
        Some m


    let lift (xt: 'a t upd): 'a t xt =
      function
      | None ->
        xt empty |> to_option

      | Some m ->
        xt m |> to_option


    let rec find_opt: type a. Term.t -> a t -> a option =
      function
      | Var i ->
        vars >.> Int_map.find_opt i

      | App (f, a) ->
        apps >.> Unit_map.find_opt () >=> find_opt f >=> find_opt a

      | Lam t ->
        lams >.> Unit_map.find_opt () >=> find_opt t


    let rec update: type a. Term.t -> a xt -> a t upd =
      function
      | Var i ->
        Int_map.update i >.> update_vars

      | App (f, a) ->
        update a >.> lift
        >.>
        update f >.> lift
        >.>
        Unit_map.update ()
        >.>
        update_apps

      | Lam t ->
        update t >.> lift >.> Unit_map.update () >.> update_lams


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
      | Var  of int
      | App  of t * t
  end


  type key = int * Term.t (* number of pattern variables, term
                             all variables above n are free variables *)


  module TMap =
  struct
    type keysub = Term.t array


    type 'a sub = (int * Term.t) list * 'a


    type var_keys = (int * int) list (* pairs of variables and keys *)


    type 'a cont = var_keys * 'a (* content with var/keys correspondence *)


    module Map =
    struct
      type 'a t =
        {
          pvar0: 'a cont option;   (* first occurrence of a new pattern var *)
          pvar1: 'a cont Int_map.t;(* repeated occurrence of a pattern var  *)
          vars:  'a cont Int_map.t;
          apps:  'a t t option;
        }


      let empty: 'a t = {
        pvar0 = None;
        pvar1 = Int_map.empty;
        vars  = Int_map.empty;
        apps  = None
      }


      let is_empty (m: 'a t): bool =
        m.pvar0 = None
        &&
        Int_map.is_empty m.pvar1
        &&
        Int_map.is_empty m.vars
        &&
        m.apps = None
    end


    module FindM =
    struct
      type 'a res =
        (keysub * var_keys * 'a) list


      type 'a t =
        keysub -> 'a res


      let return: 'a -> 'a t =
        fun a ks -> [ks, [], a]


      let fail: 'a t =
        fun _ -> []


      let (>>=) (m: 'a t) (f: 'a -> 'b t): 'b t =
        fun ks ->
        List.concat_map
          (fun (ks, vks1, a) ->
             List.map
               (fun (ks, vks2, a) -> (ks, vks1 @ vks2, a))
               (f a ks)
          )
          (m ks)


      let (>=>) (m1: 'a -> 'b t) (m2: 'b -> 'c t): 'a -> 'c t =
        fun a -> m1 a >>= m2


      let find_vars (i: int) (tm: 'a Map.t): 'a t =
        fun ks ->
        Option.(
          map
            (fun (vks, a) -> ks, vks, a)
            (Int_map.find_opt i tm.vars)
          |>
          to_list
        )

      let first_occurrence (e: Term.t) (tm: 'a Map.t): 'a t =
        fun ks ->
        List.map
          (fun (vks, a) -> Array.push e ks, vks, a)
          (Option.to_list tm.pvar0)


      let repeated_occurrence (e: Term.t) (tm: 'a Map.t): 'a t =
        fun ks ->
        List.filter_map
          (fun (i, (vks, a)) ->
             assert (i < Array.length ks);
             if ks.(i) = e then
               Some (ks, vks, a)
             else
               None
          )
          (Int_map.bindings tm.pvar1)


      let apps (tm: 'a Map.t): 'a Map.t Map.t t =
        match tm.apps with
        | None ->
          fail

        | Some tm ->
          return tm


      let rec find0: type a. Term.t -> a Map.t -> a t =
        fun e map ks ->
        first_occurrence e map ks
        @
        repeated_occurrence e map ks
        @
        look_at e map ks


      and look_at: type a. Term.t -> a Map.t -> a t =
        function
        | Var i ->
          find_vars i

        | App (f, a) ->
          apps >=> find0 f >=> find0 a
    end


    let update0:
      type a. Term.t -> (int -> int option) -> int array -> a xt -> a Map.t upd
      =
      function
      | Var i ->
        (fun p pvs ->
          match p i with
          | None ->
            assert false
          | Some pv ->
            let _ = Array.push pv pvs in
            assert false
        )

      | App (_, _) ->
        assert false
  end
end











(*
================================================================================
Triemap with General Trees
================================================================================
*)

module type TREE =
sig
  type info

  type t

  val info: t -> info

  val nchildren: t -> int

  val child: int -> t -> t
end

module General_tree (T: TREE) =
struct
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
