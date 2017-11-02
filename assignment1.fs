(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Bowen Tian, Id Number: 260707233 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGMENT even if the problem is only with one
question.  If you are not able to get the code to compile and run do not
submit it.  *)

(* module hw1_sol.  Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)
let rec sumlist l = 
    match l with
    | [] -> 0.0
    | x::xs -> x+sumlist xs
let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) ->  (x,y)::pairlists (xs,ys)
let w_mean weights data = 
    let sums = sumlist weights
    let pairs = pairlists (weights,data)
    let rec helper l=
        match l with
        | [] -> []
        | (x:float,y:float)::xys -> x*y :: helper xys
    let l = helper pairs
    (sumlist l)/sums
(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair = 
    match pair with 
    | (a,[]) -> false
    | (a,x::xs)-> if (x=a) then true
                  else memberof (a,xs)
let rec remove(item, lst) = 
    match lst with
    |[] -> []
    |x::xs -> if (x=item) then remove(item,xs)
                else x::remove(item, xs)
(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
  let rec helper(l,m) = 
    match l with
    | [] -> m
    | x::xs -> if (x>m) then helper(xs,x)
                else helper(xs,m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l = 
  match l with
  | [] -> []
  | _ -> let m = (findMax l)
         m::(selsort(remove(m,l)))
(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
    match twolists with
    | ([],[]) -> []
    | ([],l2) -> []
    | (l1,[]) -> []
    | (x::xs,ys) -> if (memberof (x,ys))then 
                    let r_ys = remove(x,ys) in
                    x::common(xs,r_ys)
                        else common(xs,ys)
common ([1;2],[2;1])
(* Question 6. *)   (* Do not edit this line. *)
(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)
let rec split l = 
    match l with
    | [] -> ([],[])
    | [y] -> ([y],[])
    | x::y::xs -> let (p1,p2) = split xs in
                    (x::p1 ,y::p2)
let rec merge twolists = 
    match twolists with
    | ([],[]) -> []
    |([],ys) -> ys
    |(xs,[]) -> xs
    |(x::xs,y::ys) -> if (x<y) then x::merge(xs,y::ys)
                        else y::merge(x::xs,ys)
let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | xs -> let (p1,p2) = split xs in  
            merge (mergesort (p1), mergesort (p2) )