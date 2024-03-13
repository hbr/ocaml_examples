(* Helper functions *)



let list_of_string (s: string): char list =
    let len = String.length s
    in
    let rec list i accu =
        if i = 0 then
            accu
        else
            let i = i - 1 in
            list i (s.[i] :: accu)
    in
    list len []



let string_of_list (lst: char list): string =
    let open Buffer in
    let buf = create 20 in
    List.iter (fun c -> add_char buf c) lst;
    contents buf



let rec rev_prepend (a: 'a list) (b: 'a list): 'a list =
    match a with
    | [] -> b
    | hd :: tl ->
        hd :: rev_prepend tl b








(* Parser *)


type 'a t =
    | Done of 'a option
    | More of (string -> 'a t)


type ('a, 'r) mon =
    int -> bool -> char list -> char list
    -> ('a option -> int -> bool -> char list -> char list -> 'r t)
    -> 'r t


let make (m: ('a, 'a) mon): 'a t =
    m 0 false [] [] (fun o _ _ _ _ -> Done o)


let put (p: 'a t) (s: string): 'a t =
    match p with
    | Done _ as p -> p
    | More f      -> f s


let run_on_string (p: 'a t) (n: int) (s: string): 'a t =
    assert (0 < n);
    let len = String.length s in
    let rec run p i =
        if i >= len then
            p
        else
            let m = min n (len - i) in
            run (put p String.(sub s i m)) (i + m)
    in
    run p 0






(* Monad *)


let return (a: 'a): ('a, 'r) mon =
    fun n flg buf la k ->
    assert (flg || buf = []);
    k (Some a) n flg buf la


let (>>=) (p: ('a, 'r) mon) (f: 'a -> ('b, 'r) mon): ('b, 'r) mon =
    fun n flg buf la k ->
    assert (flg || buf = []);
    p n flg buf la (fun o n flg buf la ->
        match o with
        | None as o   -> k o n flg buf la
        | Some a      -> f a n flg buf la k)


let ( let* ) = (>>=)


let (</>) (p: ('a, 'r) mon) (q: ('a, 'r) mon): ('a, 'r) mon =
    fun n1 flg buf la k ->
    assert (flg || buf = []);
    p n1 flg buf la
        (fun o n2 flg buf la ->
             match o with
             | None when n1 = n2 ->
                 q n1 flg buf la k
             | o ->
                 k o n2 flg buf la)


let rec char (d: char -> bool): (char, 'r) mon =
    fun n flg buf la k ->
    assert (flg || buf = []);
    match la with
    | [] ->
        More (fun s -> char d n flg buf (list_of_string s) k)
    | c :: la2 ->
        if d c then
            let buf =
                if flg then c :: buf else buf
            in
            k (Some c) (n + 1) flg buf la2
        else
            k None n flg buf la


let backtrack (p: ('a, 'r) mon): ('a, 'r) mon =
    fun n flg buf la k ->
    assert (flg || buf = []);
    p n true [] la 
        (fun o n2 flg2 buf2 la2 ->
             (*let open Printf in*)
             match o with
             | None as o when n = n2 ->
                 (* [p] failed without consumption *)
                 assert (buf2 = []);
                 assert flg2;
                 (*printf "backtrack: p failed without consumption\n";*)
                 k o n2 flg buf la2
             | None as o ->
                 (* [p] failed with consumption *)
                 assert (buf2 <> []);
                 assert flg2;
                 assert (n2 = n + List.length buf2);
                 (*printf "backtrack: p failed with %d consumption\n"
                     (List.length buf2);*)
                 let la = rev_prepend buf2 la2 in
                 k o n flg buf la
             | Some _ as o ->
                 (* [p] succeeded *)
                 assert flg2;
                 (*printf "backtrack: p succeeded\n";*)
                 let buf =
                     if flg then
                         buf2 @ buf
                     else
                         buf
                 in
                 k o n2 flg buf la2)








(* Convenience functions *)


let charc (c: char): (char, 'r) mon =
    char (fun c0 -> c = c0)


let map (f: 'a -> 'b) (m: ('a, 'r) mon): ('b, 'r) mon =
    let* a = m in return (f a)




let many (p: ('a, 'r) mon): ('a list, 'r) mon =
    let rec many (): ('a list, 'r) mon =
        (
            let* a  = p in
            let* lst = many () in
            return (a :: lst)
        )
        </>
        return []
    in
    many ()


let many1 (p: ('a, 'r) mon): ('a list, 'r) mon =
    let* a = p in
    let* lst = many p in
    return (a :: lst)







(* UNIT TESTS *)


let%test _ =
    String.length "\x00" = 1
    &&
    ("\x00").[0] = '\x00'



let all_as: string t =
    make (
        let* lst = many (charc 'a') in
        let* _   = charc '\x00' in
        return (string_of_list lst)
    )

let all_as_or_bs: string t =
    let two_as =
        let* _ = charc 'a' in
        let* _ = charc 'a' in
        return ['a'; 'a']
    in
    let two_as_and_as =
        let* two = two_as in
        let* many_as = many1 (charc 'a') in
        return (two @ many_as)
    and two_as_and_bs =
        let* two = two_as in
        let* many_bs = many1 (charc 'b') in
        return (two @ many_bs)
    in
    make (
        let* lst =
            backtrack two_as_and_as
            </>
            two_as_and_bs
        in
        let* _ = charc '\x00'
        in
        return (string_of_list lst)
    )


let run_check (p: string t) (n: int) (s: string) (more_flg: bool) (expect: string option): bool =
    let open Printf in
    let string_of_o = function
        | None ->   "None"
        | Some s -> sprintf "Some %s" s
    in
    match run_on_string p n s with 
    | Done o ->
        if more_flg then
            printf "more expected but found result <%s>\n"
                (string_of_o o)
        else if o <> expect then
            printf "expected %s, found %s\n"
                (string_of_o expect)
                (string_of_o o);
        not more_flg && o = expect
    | More _ ->
        if not more_flg then
            printf "expected %s, found more\n" (string_of_o expect);
        more_flg

let%test _ = run_check all_as 100 "" true None


let%test _ = run_check all_as 1 "aaaaa\x00" false (Some "aaaaa")


let%test _ = run_check all_as_or_bs 1 "aaaa\x00" false (Some "aaaa")


let%test _ = run_check all_as_or_bs 1 "aabb\x00" false (Some "aabb")


let len_all_as (s: string) (n: int): int =
    match run_on_string all_as n s with
    | Done None          -> -1
    | Done (Some s)      -> String.length s
    | More _             -> - 2


let%test _ =
    len_all_as "" 100 = -2


let%test _ =
    let len = len_all_as "aaaaa\x00" 1
    in
    len = 5


let%test _ =
    let len = len_all_as "\x00" 100
    in
    len = 0
