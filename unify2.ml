open Format
open Utils

module Term = struct
  type t =
      V of string | F of string * (t list)

  let f s l = F (s, l)
  let (.%(;..)) s l = f s [l]
  let c s = F (s, [])
  let v s = V s
                                  
  let rec pp f = function
    | V s -> fprintf f "%s" (String.uppercase_ascii s)
    | F (s, l) -> fprintf f "%s(%a)" (String.lowercase_ascii s) pp_list l
  and pp_list f = function 
    | [] -> ()
    | [t] -> fprintf f "%a" pp t
    | hd :: tl -> fprintf f "%a, %a" pp hd pp_list tl

  let rec compare a b =
    match a, b with
    | V s, V s2 -> String.compare s s2
    | F (s, tl), F (s2, tl2) ->
      Compare.(
        String.compare s s2 >>
        (List.combine tl tl2
         |> List.fold_left
           (fun acc (a, b) -> 
              acc >> compare a b)
           0))
    | _, _ -> -1
end

module Subst = struct
  include Set.Make(struct 
      type t = string * Term.t
      let compare s1 s2 =
        let (v1, t1) = s1 in
        let (v2, t2) = s2 in
        Compare.(
          String.compare v1 v2 >>
          Term.compare t1 t2)
    end)

  let pp f ss =
    if is_empty ss
    then fprintf f "{}"
    else 
      (fprintf f "{"; 
       iter (fun (x, t) -> fprintf f "%s/%a; " x Term.pp t) ss;
       fprintf f "}")

  let rec subst : t -> Term.t -> Term.t =
    fun subs term ->
    match term with
    | Term.V x -> 
      fold
        (fun (v, tsub) term ->
           if String.equal x v
           then tsub
           else term)
        subs
        term
    | Term.F (fname, args) ->
      Term.F (fname, (List.map (subst subs) args))
end

module Unify = struct
  type 'a result =
    | Ok of 'a
    | Error of string
       
  let (let*) r f =
    match r with 
    | Error e  -> Error e 
    | Ok a -> f a

  let rec unify : Term.t -> Term.t -> Subst.t result  =
    fun t1 t2 ->
      let open Term in
      match t1, t2 with
      | F (f1, args1), F (f2, args2) ->
        if not (String.equal f1 f2)
        then Error (sprintf "Error : %s <> %s" f1 f2)
        else List.combine args1 args2 |>
             List.fold_left
               (fun rslt (arg1, arg2) ->
                  let* unifier1 = rslt in
                  let* unifier2 = unify arg1 arg2 in
                  Ok (Subst.union unifier1 unifier2))
               (Ok (Subst.empty))
      | V x, V y ->
        Ok (Subst.of_list [x, V y])
      | _, _ -> Error "distinct term types"

  let pp ppf = function
    | Ok s -> fprintf ppf "Success : %a" Subst.pp s
    | Error e -> fprintf ppf "Error : %s" e
end

let () =
  let open Term in
  let print = printf "%a\n" pp in 
  let t1 = f "x" [v "A"; v "B"] in
  let t2 = Subst.subst (Subst.of_list [("A", v "B"); ("B", v "A")]) t1 in
  t1 |> print;
  t2 |> print;

  let unf = Unify.unify t1 t2 in
  printf "%a" Unify.pp unf


