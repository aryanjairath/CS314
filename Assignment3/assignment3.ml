open List
open Ast
open ExpressionLibrary

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec insert tree x =
  match tree with
  | Leaf -> Node(Leaf, x, Leaf)
  | Node(l, y, r) ->
     if x = y then tree
     else if x < y then Node(insert l x, y, r)
     else Node(l, y, insert r x)

let construct l =
  List.fold_left (fun acc x -> insert acc x) Leaf l

(**********************************)
(* Problem 1: Tree In-order Fold  *)
(**********************************)

let rec fold_inorder f acc t =
  match t with
  | Leaf -> acc
  | Node (left, value, right) ->
  let afterleft = fold_inorder f acc left in
  let valleft = f afterleft value in
  fold_inorder f valleft right


(**********************************)
(* Problem 2: BST Remove *)
(**********************************)

let rec remove x t = 
  let rec findminOfRight = function
    | Leaf -> failwith "Leaf does not have a min"
    | Node (Leaf, value, _) -> value
    | Node (t1, _, _) -> findminOfRight t1
  in
  match t with
  | Leaf -> Leaf  
  | Node (left, value, right) -> 
    if x < value then 
      Node(remove x left, value, right)
    else if x > value then
      Node(left, value, remove x right)
    else
      match t with
      | Node (t1, _, Leaf) -> t1
      | Node (Leaf, _, t2) -> t2
      | Node(t1, _, t2) ->
      let nodeinorder = findminOfRight t2 in Node (t1, nodeinorder, remove nodeinorder t2)
      | _ -> failwith "Unsupported"


(* ------ Type definitions for the abstract syntax tree defined in ast.ml ------- *)

(**********************************
    type binop = Add | Sub | Mul

    type expression =
      | Num of float
      | Var
      | Binop of binop * expression * expression
***********************************)



(**********************************
    There are some functions from expressionLibrary that you can use to debug your code.

    `parse: string -> expression` :
        translates a string in infix form (such as `x*x - 3.0*x + 2.5`) into an expression
        (treating `x` as the variable). The parse function parses according to the standard
        order of operations - so `5+x*8` will be read as `5+(x*8)`.
    `to_string: expression -> string` :
        prints expressions in a readable form, using infix notation. This function adds
        parentheses around every binary operation so that the output is completely unambiguous.
    `to_string_wo_paren: expression -> string` :
        prints expressions in a readable form, using infix notation. This function does not
        add any parentheses so it can only be used for expressions in standard forms.
    `make_exp: int -> expression` :
        takes in a length `l` and returns a randomly generated expression of length at most `2l`.
    `rand_exp_str: int -> string` :
        takes in a length `l` and returns a string representation of length at most `2l`.

    For example,

    let _ =
      (* The following code make an expression from a string
         "5*x*x*x + 4*x*x + 3*x + 2 + 1", and then convert the
         expression back to a string, finally it prints out the
         converted string
         *)
      let e = parse ("5*x*x*x + 4*x*x + 3*x + 2 + 1") in
      let s = to_string e in
      print_string (s^"\n")

    let _ =
      (* The following code make a random expression from a string
         and then convert the expression back to a string
         finally it prints out the converted string
         *)
      let e = make_exp 10 in
      let s = to_string e in
      print_string (s^"\n")
***********************************)


(**********************************)
(* Problem 3: Evaluation  *)
(**********************************)
let rec evaluate (e:expression) (x:float) : float =
    match e with
    | Num n -> n
    | Var -> x
    | Binop (b, e1, e2) ->
        match b with
        | Add -> evaluate e1 x +. evaluate e2 x
        | Sub -> evaluate e1 x -. evaluate e2 x
        | Mul -> evaluate e1 x *. evaluate e2 x




(**********************************)
(* Problem 4: Derivatives  *)
(**********************************)

let rec derivative (e:expression) : expression =
  match e with
  | Num _ -> Num 0.0
  | Var -> Num 1.0
  | Binop (Add, e1, e2) -> Binop (Add, derivative e1, derivative e2)
  | Binop (Sub, e1, e2) -> Binop (Sub, derivative e1, derivative e2)
  | Binop (Mul, e1, e2) -> Binop (Add, Binop (Mul, derivative e1, e2), Binop (Mul, e1, derivative e2))


(**********************************)
(* Problem 5: Find Zero  *)
(**********************************)
let find_zero (e:expression) (xn:float) (epsilon:float) (lim:int) : float option =
  let rec aux e deriv xn lim =
    if lim <= 0 then None
    else
      let res = evaluate e xn in
      if abs_float res < epsilon then Some xn
      else
        let resderiv = evaluate deriv xn in
        if resderiv = 0. then None 
        else
          let new_xn = xn -. (res /. resderiv) in
          aux e deriv new_xn (lim - 1)
  in aux e (derivative e) xn lim



(**********************************)
(* Problem 6: Simplification  *)
(**********************************)
let rec extract_terms expression =
  match expression with
  | Num value -> [(value, 0)]
  | Var -> [(1.0, 1)]
  | Binop (Add, left_expr, right_expr) ->
      let left_terms = extract_terms left_expr in
      let right_terms = extract_terms right_expr in
      List.concat [left_terms; right_terms]
  | Binop (Sub, left_expr, right_expr) ->
      let left_terms = extract_terms left_expr in
      let right_terms = List.map (fun (coeff, deg) -> (-. coeff, deg)) (extract_terms right_expr) in
      List.concat [left_terms; right_terms]
  | Binop (Mul, left_expr, right_expr) ->
      let left_terms = extract_terms left_expr in
      let right_terms = extract_terms right_expr in
      List.concat_map (fun (coeff1, deg1) -> List.map (fun (coeff2, deg2) -> (coeff1 *. coeff2, deg1 + deg2)) right_terms) left_terms

let combineterms terms =
  let rec combine_aux combined = function
    | [] -> combined
    | (c, d) :: t ->
        let (inside, restTerms) = List.partition (fun (_, d2) -> d = d2) combined in
        let new_coeff = c +. List.fold_left (fun acc (c, _) -> acc +. c) 0. inside in
        if new_coeff = 0. then combine_aux restTerms t else combine_aux ((new_coeff, d) :: restTerms) t
  in 
  let combined = combine_aux [] terms in
  List.sort (fun (_, d1) (_, d2) -> if d1 > d2 then -1 else if d1 < d2 then 1 else 0) combined

let rec pow x n =
  if n = 0 then 
    Num 1.0 
  else if n = 1 
    then x 
  else 
    Binop (Mul, x, pow x (n - 1))

let makeexpr terms =
  List.fold_right (fun (coeff, degree) acc ->
    match (coeff, degree, acc) with
    | (0., _, _) -> acc
    | (_, 0, Num 0.0) -> Num coeff
    | (_, 0, _) -> Binop (Add, Num coeff, acc)
    | (_, _, Num 0.0) -> Binop (Mul, Num coeff, pow Var degree)
    | _ -> Binop (Add, Binop (Mul, Num coeff, pow Var degree), acc)
  ) terms (Num 0.0)

let simplify (e:expression) : expression =
  let extracted = extract_terms e in
  let combined = combineterms extracted in
  let expr = makeexpr combined in expr
 
(*****************************************)
(* Problem 7: Automatic Differentiation *)
(*****************************************)

(*

"Forward mode automatic differentiation", has become an
important algorithm (since 2017 or so) in deep learning.
You can read about it in section 3.1 of this paper:
http://jmlr.org/papers/volume18/17-468/17-468.pdf
"Automatic Differentiation in Machine Learning: A Survey"
(and pay particular attention to Table 2 for a worked example).

So, the challenge (which is actually not very difficult) is,
write this function


that computes both e(x) and the first derivative e'(x),
without ever calculating (derivative e).  Like evaluate,
do it by case analysis on the syntax-tree of e.

*)

let rec evaluate2 (e: expression) (x: float) : float * float =
    match e with
    | Num n -> (n, 0.0)
    | Var -> (x, 1.0)
    | Binop(op, e1, e2) -> 
        let (v1, d1) = evaluate2 e1 x in
        let (v2, d2) = evaluate2 e2 x in
        match op with
        | Add -> (v1 +. v2, d1 +. d2)
        | Sub -> (v1 -. v2, d1 -. d2)
        | Mul -> (v1 *. v2, v1 *. d2 +. v2 *. d1)


(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in
  let bonus_count = ref 1 in

 (* Testcases for fold_inorder *)
  let _ =
    try
      assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = [1;2;3]);
      assert (fold_inorder (fun acc x -> acc + x) 0 (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = 6)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

 (* Testcases for remove *)
  let _ =
    try
      assert (remove 20 (Node (Node (Node (Leaf, 20, Leaf), 30, Node (Leaf, 40, Leaf)), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf))))
              = (Node (Node (Leaf,                  30, Node (Leaf, 40, Leaf)), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf)))));
      assert (remove 30 (Node (Node (Leaf,                  30, Node (Leaf, 40, Leaf)), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf))))
              = (Node (Node (Leaf,                  40, Leaf                 ), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf)))));
      assert (remove 50 (Node (Node (Leaf,                  40, Leaf                 ), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf))))
              = (Node (Node (Leaf,                  40, Leaf                 ), 60, Node (Leaf,                  70, Node (Leaf, 80, Leaf)))))
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

 (* Testcases for evaluate *)
  let _ =
    try
      assert (evaluate (parse "x*x + 3.0") 2.0 = 7.0)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

 (* Testcases for derivative *)
  let _ =
    try
      assert (evaluate (derivative (parse "x*x + 3.0 + 3.0*x")) 2.0 = 7.0);
      assert (evaluate (derivative (parse "x*x*x+ 3.0*x")) 2.0 = 15.0)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

 (* Testcases for zero finding *)
  let _ =
    try
      let e = (parse "2*x*x - x*x*x - 2") in
      let g, epsilon, lim = 3.0, 1e-3, 50 in
      let x = find_zero e g epsilon lim in
      match x with
      | None -> assert false
      | Some x ->
          let eval_result = evaluate e x in
          assert (0. -. epsilon < eval_result && eval_result < epsilon)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

 (* Testcases for simplify *)
  let _ =
    try
      (*print_string (to_string_wo_paren (simplify (parse "3*x*x + 8*x + 2*x*x - 5 - 5*x")));
       print_string (to_string_wo_paren (simplify (parse "(x-1)*x*(x-5)")));
       print_string (to_string_wo_paren (simplify (parse "x - x")));
      print_string (to_string_wo_paren (simplify (parse "x + x + 0")));
      print_string (to_string_wo_paren (simplify (parse "(x-1)*x*(x-5)*(4*x*x*x-11+66*x)")));*)
      assert (to_string_wo_paren (simplify (parse "3*x*x + 2*x - 5 + 4*x*x - 7*x")) = "7.*x*x+-5.*x+-5.");
      assert (to_string_wo_paren (simplify (parse "3*x*x + 8*x + 2*x*x - 5 - 13*x")) = "5.*x*x+-5.*x+-5.");
      assert (to_string_wo_paren (simplify (parse "(x-1)*x*(x-5)")) = "1.*x*x*x+-6.*x*x+5.*x");
      assert (to_string_wo_paren (simplify (parse "(x-1)*x*(x-5)*(4*x*x*x-11+66*x)")) = "4.*x*x*x*x*x*x+-24.*x*x*x*x*x+86.*x*x*x*x+-407.*x*x*x+396.*x*x+-55.*x");
      assert (to_string_wo_paren (simplify (parse "x - x")) = "0.");
      assert (to_string_wo_paren (simplify (parse "x + x + 0")) = "2.*x");
      assert (to_string_wo_paren (simplify (parse "0")) = "0.")
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

 (* Testcases for evaluate2 *)
  let _ =
    try
      assert (evaluate2 (parse "x*x + 3") 2.0 = (7.0, 4.0))
    with e -> (bonus_count := !bonus_count - 1; print_string ((Printexc.to_string e)^"\n")) in

  if !error_count = 0 then Printf.printf ("Passed all testcases.\n")
  else Printf.printf ("%d out of 6 programming questions are incorrect.\n") (!error_count);

  if !bonus_count = 0 then Printf.printf ("The bonus problem is not solved.\n")
  else Printf.printf ("The bonus problem is solved.\n")

let _ = main()
