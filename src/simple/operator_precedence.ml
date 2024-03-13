module type COMBINATOR =
sig
    include Fmlib_std.Interfaces.MONAD

    val (</>): 'a t -> 'a t -> 'a t
end

module Make (M: COMBINATOR) =
struct
    open M

    type exp

    type op

    let binary (_: exp) (_: op) (_: exp): exp =
        assert false

    let left_stronger (_: op) (_: op): bool =
        assert false

    let right_stronger (o1: op) (o2: op): bool =
        not (left_stronger o1 o2)

    let primary (): exp t =
        assert false

    let operator (): op t =
        assert false



    type buffer =
        | First of exp
        | Add   of buffer * op * exp
            (* Invariant: In [Add (b, o, e)] the operator binds stronger than
               all previous operators in the buffer [b].
             *)



    let operator_expression (): exp t =
        let rec start (): exp t =
            let* e = primary () in
            more (First e)

        and more (b: buffer): exp t =
            (* Look for more operations in the remaining input *)
            (
                let* o = operator () in
                let* e = primary () in
                next b o e
            )
            </>
            (* If there is no more operator input then reduce the buffer. *)
            reduce b

        and next (b: buffer) (o_new: op) (e_new: exp): exp t =
            (* Check the next operator, expression pair. *)
            match b with
            | First _ as b ->
                (* Not yet any operator present in the buffer *)
                more (Add (b, o_new, e_new))

            | Add (b1, o2, e2) as b2 ->
                (* At least one operator expression pair in the buffer. *)
                if
                    right_stronger o2 o_new
                then
                    more (Add (b2, o_new, e_new))

                else begin
                    match b1 with
                    | First e1 ->
                        (* buffer: e1 o2 e2 and o2 binds stronger than o_new *)
                        more (Add (First (binary e1 o2 e2), o_new, e_new))

                    | Add (b0, o1, e1) ->
                        (* buffer: ... o1 e1 o2 e2 and o2 binds stronger than
                           o_new.

                           In this case we can reduce the buffer to
                           ... o1 (e1 o2 e2) and recheck the pair o_new e_new in
                           this buffer.
                        *)
                        next (Add (b0, o1, binary e1 o2 e2)) o_new e_new
                end

        and reduce: buffer -> exp t =
            (* Reduce the buffer in case that there is no more input for the
               operator expression. *)
            function
            | First e ->
                return e

            | Add (First e, o1, e1) ->
                return (binary e o1 e1)

            | Add (Add (b, o1, e1), o2, e2) ->
                assert (right_stronger o1 o2);
                reduce (Add (b, o1,  binary e1 o2 e2))
        in
        start ()
end
