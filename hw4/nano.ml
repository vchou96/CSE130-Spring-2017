exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

let lookup (x,evn) = 
	match listAssoc(x, evn) with
	| Some a -> a 
	| _-> raise(MLFailure("Not found"))

let rec eval (evn,e) =
	(*evaluate e in evn*)
	match e with
	| Const a-> Int a 
	| True-> Bool true
	| False-> Bool false
	| NilExpr-> Nil
	| Var b-> lookup(b, evn)
	| Bin (x, op, z)-> 
		(let x1= eval(evn, x) in 
		let y1= eval(evn, z) in
		match op with
			| Plus-> (match (x1, y1) with 
				| Int x1, Int y1-> Int (x1+y1)
				| _-> raise(MLFailure("Wrong operators")))
			| Minus-> (match (x1, y1) with
				| Int x1, Int y1-> Int(x1-y1)
				| _-> raise(MLFailure("Wrong operators")))
			| Mul-> (match (x1, y1) with
				| Int x1, Int y1-> Int(x1*y1)
				| _-> raise(MLFailure("Wrong operators")))
			| Div-> (match (x1, y1) with
				| Int x1, Int y1-> Int(x1/y1)
				| _-> raise(MLFailure("Wrong operators")))
			| Eq->  (match (x1, y1) with
				| Int x1, Int y1-> Bool(x1=y1)
				| Bool x1, Bool y1-> Bool(x1=y1)
				| _-> raise(MLFailure("Wrong operators")))	
			| Ne->  (match (x1, y1) with
				| Int x1, Int y1-> Bool(x1!=y1)
				| Bool x1, Bool y1-> Bool(x1!=y1)
				| _-> raise(MLFailure("Wrong operators")))
			| Lt->  (match (x1, y1) with
				| Int x1, Int y1-> Bool(x1<y1)
				| _-> raise(MLFailure("Wrong operators")))
			| Le-> (match (x1, y1) with
				| Int x1, Int y1-> Bool(x1<=y1)
				| _-> raise(MLFailure("Wrong operators")))
			| And-> (match (x1, y1) with
				| Bool x1, Bool y1-> Bool(x1 && y1)
				| _-> raise(MLFailure("Wrong operators")))
			| Or-> (match (x1, y1) with
				| Bool x1, Bool y1-> Bool(x1 || y1)
				| _-> raise(MLFailure("Wrong operators")))
			| Cons-> (match eval (evn,x) with
				| Int i-> (match eval(evn, z) with
					| Nil-> Pair(Int i, Nil)
					| Pair(a,b)-> Pair(Int i, Pair(a,b))
					| _-> raise(MLFailure("error")))	
				| Pair(c,d)->(match eval(evn, z) with
					| Pair(a, b)->Pair(Pair(c,d), Pair(a,b))
					| _-> raise(MLFailure("error")))
				| _-> raise(MLFailure("error")))
			| _-> raise(MLFailure "no such operation"))
	| If (x, y, z)-> 
		(match eval(evn, x) with
			| Bool b-> 
				if b=true then eval (evn, y) 
				else eval(evn, z)
			| _-> raise(MLFailure "not a Boolean"))
	| Let (x,y,z)-> eval((x, eval(evn,y))::evn, z) (*let x= y in z*) 
	| App (x, y)->
		(let Closure(evn1, name, formal, body)= eval(evn, x) in
			match name with
				| Some name1->
					eval((name1, Closure(evn1,name,formal,body))::(formal,eval(evn, y))::evn1, body)		
				| None->
					eval((formal,eval(evn, y))::evn1, body))

	| Fun (x,y)-> Closure(evn, None, x, y)     
	| Letrec(x,y,z)->
		match eval(evn, y) with
		| Closure(newevn, None, formal, body)->
			eval((x, Closure(newevn, Some x, formal, body))::evn,z)
		| _-> 
			eval((x, eval(evn, y))::evn,z)
	| _-> raise(MLFailure "variable not bound: x") 

(**********************     Testing Code  ******************************)

(* Uncomment to test part (a) 
  *)
let evn = [("z1",Int 0);("x",Int 1);("y",Int 2);("z",Int 3);("z1",Int 4)]
 
let e1  = Bin(Bin(Var "x",Plus,Var "y"), Minus, Bin(Var "z",Plus,Var "z1"))

let _   = eval (evn, e1)        (* EXPECTED: Nano.value = Int 0 *)
(*
let _   = eval (evn, Var "p")   (* EXPECTED:  Exception: Nano.MLFailure "variable not bound: p". *)
*)
(*Printf.printf"here" 
*)

(* Uncomment to test part (b) 
*)
let evn = [("z1",Int 0);("x",Int 1);("y",Int 2);("z",Int 3);("z1",Int 4)]
 
let e1  = If(Bin(Var "z1",Lt,Var "x"),Bin(Var "y",Ne,Var "z"),False)
  
let _   = eval (evn,e1)         (* EXPECTED: Nano.value = Bool true *)

let e2  = If(Bin(Var "z1",Eq,Var "x"), 
                Bin(Var "y",Le,Var "z"),
                Bin(Var "z",Le,Var "y")
            )

let _   = eval (evn,e2)         (* EXPECTED: Nano.value = Bool false *)



(* Uncomment to test part (c) 
*)
let e1 = Bin(Var "x",Plus,Var "y")

let e2 = Let("x",Const 1, Let("y", Const 2, e1)) 

let _  = eval ([], e2)          (* EXPECTED: Nano.value = Int 3 *)

let e3 = Let("x", Const 1, 
           Let("y", Const 2, 
             Let("z", e1, 
               Let("x", Bin(Var "x",Plus,Var "z"), 
                 e1)
             )
           )
         )

let _  = eval ([],e3)           (* EXPCETED: Nano.value = Int 6 *)




(* Uncomment to test part (d) 
*)
let _ = eval ([], Fun ("x",Bin(Var "x",Plus,Var "x"))) 

(* EXPECTED: Nano.value = Closure ([], None, "x", Bin (Var "x", Plus, Var "x")) *)

let _ = eval ([],App(Fun ("x",Bin(Var "x",Plus,Var "x")),Const 3));;

(* EXPECTED: Nano.value = Int 6 *)

let e3 = Let ("h", Fun("y", Bin(Var "x", Plus, Var "y")), 
               App(Var "f",Var "h"))
 
let e2 = Let("x", Const 100, e3)
 
let e1 = Let("f",Fun("g",Let("x",Const 0,App(Var "g",Const 2))),e2) 

let _  = eval ([], e1)        
    (* EXPECTED: Nano.value = Int 102 *)

let _ = eval ([],Letrec("f",Fun("x",Const 0),Var "f"))
    (* EXPECTED: Nano.value = Closure ([], Some "f", "x", Const 0) *)



(* Uncomment to test part (e)
*) 
let _ = eval ([], 
              Letrec("fac", 
                        Fun("n", If (Bin (Var "n", Eq, Const 0), 
                                    Const 1, 
                                    Bin(Var "n", Mul, App(Var "fac",Bin(Var "n",Minus,Const 1))))),
              App(Var "fac", Const 10)))

(* EXPECTED: Nano.value = Int 3628800 *)

 

(* Uncomment to test part (f)
*) 
(*
let _ = eval ([],Bin(Const 1,Cons,Bin(Const 2,Cons,NilExpr)))

    (* EXPECTED: Nano.value = Pair (Int 1, Pair (Int 2, Nil)) *)

let _ = eval ([],App(Var "hd",Bin(Const 1,Cons,Bin(Const 2,Cons,NilExpr))))

    (* EXPECTED: Nano.value = Int 1 *)

let _ = eval ([],App(Var "tl",Bin(Const 1,Cons,Bin(Const 2,Cons,NilExpr))))
   *) 
    (* EXPECTED: Nano.value = Pair (Int 2, Nil) *)

 
