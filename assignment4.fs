type id = string

type term =
  | Var of id
  | Const of int 
  | Term of id * term list

(* invariant for substitutions: *)
(* no id on a lhs occurs in any term earlier in the list *)
type substitution = (id * term) list

(* check if a variable occurs in a term *)
let rec occurs (x : id) (t : term) : bool = 
  match t with
  |Var (a) ->  (x=a)
  |Const (c) -> false
  |Term (a,lst)->  List.exists (fun tm -> occurs x tm) lst

(* substitute term s for all occurrences of variable x in term t *)
let rec subst (s : term) (x : id) (t : term) : term = 
  match t with 
  |Var (a) -> if (a=x) then s else Var a
  |Const (c) -> Const c 
  |Term (a,lst) -> Term (a, List.map (fun tm -> (subst s x tm) ) lst)


(* apply a substitution right to left; use foldBack *)
let apply (s : substitution) (t : term) : term = 
  List.foldBack (fun (x,tm) acc -> subst tm x acc) s t


(* unify one pair *)


(* can I assume they have the same length*)

let rec unify (s : term) (t : term) : substitution = 
  match (s,t) with
  | (Term (a,lsta),Term (b,lstb)) -> 
    if (a<>b) then failwith "not unifiable: Head symbol conflict"
    else List.foldBack (fun (t1,t2) acc -> (unify (apply acc t1 ) (apply acc t2))@acc ) (List.zip lsta lstb) [] 
  | (Var v, Var v2) -> if (v=v2) then [] else [(v,Var v2)]
  | (Var v, tm) -> if (occurs v tm) then failwith "not unifiable: Circularity" else [(v,tm)]
  | (tm, Var v) -> if (occurs v tm) then failwith "not unifiable: Circularity" else [(v,tm)]
  |(Term (fname, l ), Const c ) ->  failwith "not unifiable: Term constant clash"
  |(Const c,Term (fname, l )) ->  failwith "not unifiable: Term constant clash"
  |(Const c1 , Const c2) -> if (c1=c2) then [] else failwith "not unifiable: Clashing constants"
and unify_list (s : (term * term) list) : substitution =
  match s with
  | [] -> []
  | (t1,t2) ::xs -> (unify t1 t2) @ (unify_list xs)

unify_list [(Term ("f",[Var "x"; Var "y"; Term ("h",[Var "x"])]),Term("f", [Term ("g",[Var "z"]);Term ("h",[Var "x"]); Var "y" ]) )]
unify (Term ("f",[Var "x"; Var "y"; Term ("h",[Var "x"])])) (Term("f", [Term ("g",[Var "z"]);Term ("h",[Var "x"]); Var "y" ]) )
unify (Term ("f",[Var "x"])) (Term("f",[Var "x"]))
unify (Term("f",[Var "x";Const 3; Const 3])) (Term("f",[Var "y";Var "y"; Var "x"]))
unify (Term("f",[Var "x"; Term ("g",[Var "q"])])) (Term("f",[Term ("a",[Var "b"]);Var "y"]))
unify (Term("f",[Var "x";Const 3])) (Term("f",[Const 3 ;Var "x"]))
unify (Term("f",[Var "x";Var "y"; Var "z"])) (Term("f",[Var "x";Term ("g",[Var "z"]); Const 5]))
unify (Term("f",[Term ("g",[Var "x"]); Var "y"])) (Term("f",[Var "z";Term ("h", [Var "x"])]))
unify (Term ("f", [ Term ("g",[Term ("h",[Var "x"]); Term ("f",[Var "y"])]) ; Var "z" ])) (Term ("f",[Term ("g",[Var "p"; Var "u"]) ; Term ("k",[Var "w"])]))

unify (Term ("g",[Var "y"; Var "x"])) (Term ("g", [Term ("f",[Var "x"]);Term ("f",[Var "y"])]))
unify (Term("f",[Var "x";Const 3])) (Term("f",[Const 2;Var "x"]))
unify (Term("f",[Var "x"])) (Term("f",[Term("h",[Var "x"])]))
unify (Term ("g",[Var "y"; Var "x"])) (Term ("g", [Term ("f",[Var "x"]);Term ("f",[Var "y"])]));;
let t1 = Term("f",[Var "x";Var "y"; Term("h",[Var "x"])]);;
let t2 = Term("f", [Term("g",[Var "z"]); Term("h",[Var "x"]); Var "y"]);;
unify t1 t2;;
apply  [("x", Term ("g",[Var "z"])); ("y", Term ("h",[Var "x"]))] t1
let t3 = Term("f", [Var "x"; Var "y"; Term("g", [Var "u"])]);;
let t4 = Term("f", [Var "x"; Term("h", [Var "z"]); Var "x"]);;
let t5 = Term("f", [Term("k", [Var "y"]); Var "y"; Var "x"]);;
unify t4 t5;;
apply [("x", Term ("k",[Term ("h",[Var "z"])])); ("y", Term ("h",[Var "z"]))] t4
unify t5 t4;;
apply it t4;;
let t6 = Term("f", [Const 2; Var "x"; Const 3]);;
let t7 = Term("f", [Const 2; Const 3; Var "y"]);;
unify t6 t7;;
apply it t7;; 
unify t1 t7;;
(*
Examples
> let t1 = Term("f",[Var "x";Var "y"; Term("h",[Var "x"])]);;
val t1 : term = Term ("f",[Var "x"; Var "y"; Term ("h",[Var "x"])])
> let t2 = Term("f", [Term("g",[Var "z"]); Term("h",[Var "x"]); Var "y"]);;
val t2 : term =
  Term ("f",[Term ("g",[Var "z"]); Term ("h",[Var "x"]); Var "y"])
> let t3 = Term("f", [Var "x"; Var "y"; Term("g", [Var "u"])]);;
val t3 : term = Term ("f",[Var "x"; Var "y"; Term ("g",[Var "u"])])
> unify t1 t2;;
val it : substitution =
  [("x", Term ("g",[Var "z"])); ("y", Term ("h",[Var "x"]))]
> let t4 = Term("f", [Var "x"; Term("h", [Var "z"]); Var "x"]);;
val t4 : term = Term ("f",[Var "x"; Term ("h",[Var "z"]); Var "x"])
>  let t5 = Term("f", [Term("k", [Var "y"]); Var "y"; Var "x"]);;
val t5 : term = Term ("f",[Term ("k",[Var "y"]); Var "y"; Var "x"])
> unify t4 t5;;
val it : substitution =
  [("x", Term ("k",[Term ("h",[Var "z"])])); ("y", Term ("h",[Var "z"]))]
> unify t5 t4;;
val it : substitution =
  [("x", Term ("k",[Term ("h",[Var "z"])])); ("y", Term ("h",[Var "z"]))]
> apply it t4;;
val it : term =
  Term
    ("f",
     [Term ("k",[Term ("h",[Var "z"])]); Term ("h",[Var "z"]);
      Term ("k",[Term ("h",[Var "z"])])])
> let t6 = Term("f", [Const 2; Var "x"; Const 3]);;
val t6 : term = Term ("f",[Const 2; Var "x"; Const 3])
> let t7 = Term("f", [Const 2; Const 3; Var "y"]);;
val t7 : term = Term ("f",[Const 2; Const 3; Var "y"])
> unify t6 t7;;
val it : substitution = [("x", Const 3); ("y", Const 3)]
> apply it t7;;
val it : term = Term ("f",[Const 2; Const 3; Const 3])
> unify t1 t7;;
System.Exception: not unifiable: term constant clash
....... junk removed .............
Stopped due to error
*)
