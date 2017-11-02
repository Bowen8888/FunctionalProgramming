module Hw2

(* Assignment 2 *) (* Do not edit this line. *)
(* Student name: Bowen Tian, Id Number:  *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  It is OK to change a "rec" declaration and put the
recursive function inside a helper if you want to.  Your code MUST compile
and must NOT go into infinite loops.  An assignment like that means you
have not tested it.  You will get ZERO FOR THE ENTIRE ASSIGMENT even if the
problem is only with one question.  If you are not able to get the code to
compile and run do not submit it.  *)

(* Question 1 *) 
let deriv (f, dx: float) = fun x -> ((f(x + dx) - f(x))/dx)

let rec newton(f,guess:float,tol:float,dx:float) = 
    let x = guess -  (f(guess))/(deriv (f,dx)(guess)) in
    if abs(f(x))<tol then x
    else newton(f,x,tol,dx)

let make_cubic(a:float,b,c) = fun x -> (x*x*x + a * x*x + b*x + c)
newton(make_cubic(2.0,-3.0,1.0),0.0,0.0001,0.0001)
(* For testing 
let make_cubic(a:float,b,c) = fun x -> (x*x*x + a * x*x + b*x + c)
newton(make_cubic(2.0,-3.0,1.0),0.0,0.0001,0.0001)

let root = newton(sin,5.0,0.0001,0.0001)
*)

(* Question 2 *)

type term = Term of float * int
type poly = Poly of (float * int) list

exception EmptyList

let multiplyPolyByTerm(Term (c,e):term, Poly p:poly):poly = if (p=[]) then raise EmptyList 
                                                                else if (c=0.0) then Poly [ (0.0,0)] 
                                                                        else  Poly (List.map (fun (a:float,b:int) ->  if (a=0.0) then (0.0,0) else (c*a,e+b) ) p)                                     

let addTermToPoly(Term (c,e):term, Poly p:poly):poly =
  if (p=[]) then raise EmptyList
  else 
    let rec add((c,e),p) = 
      match p with
      |[]->[]
      |(a,b)::xs-> if (b=e) then (if ((a+c)=0.0) then xs else (a+c,e)::xs)
                    elif (b<e) then (c,e)::(a,b)::xs
                    else if (c=0.0) then (a,b)::xs else(a,b)::add((c,e),xs)
    let p2 = (add((c,e),p))
    if (p2=[]) then Poly [(0.0,0)] else Poly p2


let addPolys(Poly p1:poly, Poly p2:poly):poly =
  if (p1=[] || p2 =[]) then raise EmptyList
  else
    let rec helper(Poly l1:poly,Poly l2:poly):poly=
      match l1 with
      |[] -> Poly l2
      |x::xs -> let y = Term x in
                  helper(Poly xs,addTermToPoly(y,Poly l2))
    helper((Poly p1),(Poly p2))

let multPolys(Poly p1:poly, Poly p2:poly) = 
  if (p1=[] || p2 =[]) then raise EmptyList
  else
    let rec helper(Poly l1:poly,Poly l2:poly):poly=
      match l1 with
      |[] -> Poly []
      |[x] -> let y = Term x in
                  multiplyPolyByTerm(y,Poly l2)
      |x::xs -> let y = Term x in
                  addPolys(multiplyPolyByTerm(y,Poly l2),helper(Poly xs,Poly l2))
    helper((Poly p1),(Poly p2))


let exp(b:float, e:int) =
  let rec helper(b:float, e:int, a: float) =
    if (b = 0.0) then 0.0
    elif (e = 0) then a
    elif (e % 2 = 1) then helper(b,e-1, b*a)
    else helper(b*b,e/2,a)
  helper(b,e,1.0)

let evalTerm (v:float) (Term (c,e):term) = if (c=0.0) then c else  c*exp(v,e)

let evalPoly(Poly p:poly,v:float):float = if (p=[]) then raise EmptyList else List.fold (fun sum x -> sum+(evalTerm v (Term x)) ) 0.0 p

let diffPoly (Poly p) = 
  if (p=[]) then raise EmptyList 
  else
    let rec helper p=
      match p with
      |[]-> []
      |(a,b)::xs-> if (b>0) then  (a*(float b),b-1) :: helper xs
                    else xs
    Poly (helper p)                

(* Question 3 *)
type Exptree =
  | Const of int 
  | Var of string 
  | Add of Exptree * Exptree 
  | Mul of Exptree * Exptree

type Bindings = (string * int) list

(* exception notFound *)

let rec lookup(name:string, env: Bindings) = 
  match env with
  |[] -> None
  |(a,b)::xs -> if (a=name) then Some b 
                  else lookup(name,xs)

let rec insert(name:string, value: int, b: Bindings) = 
  match b with
  |[]-> [(name,value)]
  |(a,b)::xs -> if (a>=name) then (name,value)::(a,b)::xs
                  else (a,b)::insert(name,value,xs)

let rec eval(exp : Exptree, env:Bindings) = 
  match exp with
  |Const a -> Some a
  |Var x -> lookup(x,env)
  |Add(x,y) -> 
    match ((eval(x,env)),(eval(y,env))) with
    |(Some a, Some b) ->  Some (a+b)
    | _ -> None
  |Mul(x,y) -> 
    match ((eval(x,env)),(eval(y,env))) with
    |(Some a, Some b) ->  Some (a*b)
    | _ -> None

let env:Bindings = [("a",3);("b",4);("c",5)]                                
let exp1 = Add(Const 3, Const 4)
let exp2 = Add(Const 3, Var "b")
let exp3 = Add(Var "c", Var "b")
let exp4 = Mul(exp3,exp2)
let exp5 = Add(Var "d",exp3)
let env2 = insert("b",10,env)
lookup ("b" ,env)
eval(exp5,env2)
(* For testing 

let env:Bindings = [("a",3);("b",4);("c",5)]                                

let exp1 = Add(Const 3, Const 4)
let exp2 = Add(Const 3, Var "b")
let exp3 = Add(Var "c", Var "b")
let exp4 = Mul(exp3,exp2)
let exp5 = Add(Var "d",exp3)
let env2 = insert("b",10,env)

*)


(* Question 4 *)

type Team = string
type Goals = Goals of int
type Points = Points of int
type Fixture = Team * Team  
type Result = ((Team * Goals) * (Team * Goals))
type Table = Map<Team,Points>
    
let league =["Chelsea"; "Spurs"; "Liverpool"; "ManCity"; "ManUnited"; "Arsenal"; "Everton"; "Leicester"]

let pointsMade (r: Result) = 
  match r with 
  | ((t1,Goals g1),(t2, Goals g2)) -> 
         if (g1 > g2) then ((t1, Points 3),(t2, Points 0))
         elif (g2 > g1) then ((t1, Points 0),(t2, Points 3))
         else ((t1, Points 1),(t2, Points 1))

let initEntry (name:Team) = (name, Points 0)
let initializeTable l = Map.ofList (List.map initEntry l)

let table = initializeTable league


let weekend1:Result list = [(("Chelsea", Goals 2),("Spurs", Goals 1)); (("Liverpool", Goals 3),("ManCity", Goals 2));(("ManUnited", Goals 1),("Arsenal", Goals 4));(("Everton", Goals 1),("Leicester", Goals 5))]

let weekend2:Result list = [(("Chelsea", Goals 5),("Arsenal", Goals 0)); (("Spurs", Goals 3),("ManCity",Goals 2)); (("ManUnited", Goals 1),("Liverpool", Goals 0));(("Everton", Goals 3),("Leicester", Goals 5))]

let s = [weekend2;weekend1]

let updateTable(t:Table,r:Result):Table = 
  let ((t1, Points p1),(t2,Points p2)) = pointsMade r
  Map.map (fun t0 (Points p0) -> if (t0 = t1) then Points (p0+p1)
                                    elif (t0 = t2) then Points (p0+p2)
                                    else  Points p0) t


let rec weekendUpdate(t:Table,rl: Result list): Table = 
  match rl with
  |[] -> t
  |r::rs -> weekendUpdate(updateTable(t,r),rs)

let rec seasonUpdate(t:Table, sll:Result list list) : Table = 
  match sll with
  |[] -> t
  |rl::rll -> seasonUpdate(weekendUpdate(t,rl),rll)

let less((s1,n1):Team * Points, (s2,n2):Team * Points) =  if (n1<n2) then true else false

let rec myinsert item lst =
  match lst with
  | [] -> [item]
  | x::xs -> if less(item,x) then x::(myinsert item xs) else item::lst

let rec isort lst =
  match lst with
  | [] -> []
  | x::xs -> myinsert x (isort xs)

let showStandings (t:Table) = isort (Map.toList t)

                                                  
(* Question 5 *)

type Destination = City of string
type RoadMap = Roads of Map<Destination, Set<Destination>>

let roadData = [
  "Andulo", ["Bibala"; "Cacolo"; "Dondo"]
  "Bibala", ["Andulo"; "Dondo"; "Galo"]
  "Cacolo", ["Andulo"; "Dondo"]
  "Dondo",  ["Andulo"; "Bibala"; "Cacolo"; "Ekunha"; "Funda"]
  "Ekunha", ["Dondo"; "Funda"]
  "Funda",  ["Dondo"; "Ekunha"; "Galo"; "Kuito"]
  "Galo",   ["Bibala"; "Funda"; "Huambo"; "Jamba"]
  "Huambo", ["Galo"]
  "Jamba",  ["Galo"]
  "Kuito",  ["Ekunha"; "Funda"]
]
let makeRoadMap data = 
  let rec helper d = 
    match d with 
    |[] -> []
    |(name,names)::ds -> (City name, Set.ofList (List.map (City) names))::(helper ds)
  Roads (Map.ofList (helper data))

let rec upToManySteps (Roads r) n startCity =
  if (n=0 || (Map.tryFind startCity r)=None) then Set ([]:Destination list)
  else 
      let cities = Map.find startCity r
      if (n=1)  then cities 
      else 
        Set.union cities (Set.unionMany (Set.map (upToManySteps (Roads r) (n-1) ) cities))

