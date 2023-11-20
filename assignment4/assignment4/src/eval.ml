open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Print env to stdout *)
let print_env_std (env : environment): unit =
  List.fold_left (fun _ (var, value) ->
      match value with
        | Int_Val i -> Printf.printf "- %s => %s\n" var (string_of_int i)
        | Bool_Val b -> Printf.printf "- %s => %s\n" var (string_of_bool b)
        | Closure _ -> ()) () (prune_env env)

(* Print env to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      match value with
        | Int_Val i -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_int i))
        | Bool_Val b -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_bool b))
        | Closure _ -> acc
      ) "" (prune_env env)


(***********************)
(****** Your Code ******)
(***********************)
let rec findvar env n = 
  match env with
  | (id, eval):: tail -> if( n = id) then eval else findvar tail n
  | [] -> raise UndefinedVar

(* evaluate an arithmetic expression in an environment *)
let rec eval_expr (e : exp) (env : environment) : value =
    match e with
    | Number n -> (Int_Val n)
    | True -> Bool_Val true
    | False -> Bool_Val false
    | Var n -> findvar env n (*Find the mapping of the variable if it exist*)
    | Plus(e1, e2) -> (
        match (eval_expr e1 env, eval_expr e2 env) with
        | Int_Val x, Int_Val z -> Int_Val(x+z)
        | _,_ -> raise TypeError (*In case any value is not an integer*)
      )
    | Minus(e1, e2) -> (
        match(eval_expr e1 env, eval_expr e2 env) with
        | Int_Val v1, Int_Val v2 -> Int_Val(v1-v2)
        | _,_ -> raise TypeError (*In case any value is not an integer*)
      )
    | Times(e1, e2) -> (
        match(eval_expr e1 env, eval_expr e2 env) with
        | Int_Val v1, Int_Val v2 -> Int_Val(v1*v2)
        | _,_ -> raise TypeError (*In case any value is not an integer*)
      )
    | Div(e1, e2) -> (
      match(eval_expr e1 env, eval_expr e2 env) with
      | Int_Val v1, Int_Val v2 -> if v2 = 0 then raise DivByZeroError else Int_Val(v1/v2) (*Cannot have 0 as denominator*)
      | _,_ -> raise TypeError (*In case any value is not an integer*)
    )
    | Mod(e1, e2) -> (
      match(eval_expr e1 env, eval_expr e2 env) with
      | Int_Val x, Int_Val y -> if y = 0 then raise DivByZeroError else Int_Val(x mod y) (*Cannot mod with 0*)
      | _,_ -> raise TypeError 
    )
    | Or(e1, e2) -> (
      match(eval_expr e1 env, eval_expr e2 env) with
       | Bool_Val v1, Bool_Val v2 -> Bool_Val(v1 || v2) (*Use || operator to check if v1 or v2 is true*)
       | _,_ -> raise TypeError
    )
    | And(e1, e2) -> (
      match(eval_expr e1 env, eval_expr e2 env) with
       | Bool_Val v1, Bool_Val v2 -> Bool_Val(v1 && v2) (*Use && operator to check if v1 and v2 is true*)
       | _,_ -> raise TypeError
    )
    | Not(e1) -> (
      match(eval_expr e1 env) with
        | Bool_Val v -> Bool_Val(not v)
        | _ -> raise TypeError
    )
    | Lt(e1, e2) -> (
      match(eval_expr e1 env, eval_expr e2 env) with
        | Int_Val v1, Int_Val v2 -> Bool_Val(v1 < v2)
        | _,_ -> raise TypeError
    )
    | Leq(e1, e2) -> (
      match(eval_expr e1 env, eval_expr e2 env) with
        | Int_Val v1, Int_Val v2 -> Bool_Val(v1 <= v2)
        | _,_ -> raise TypeError
    )
    | Eq(e1, e2) -> (
      match(eval_expr e1 env, eval_expr e2 env) with
        | Int_Val v1, Int_Val v2 -> Bool_Val(v1 = v2)
        | Bool_Val v1, Bool_Val v2 -> Bool_Val(v1 = v2)
        | _,_ -> raise TypeError
    )
    | Fun(p, exr) -> Closure(env, p, exr)
    | App(e1, e2) -> 
      let v1 = eval_expr e1 env in
      let v2 = eval_expr e2 env in
      match v1 with
        | Closure(env, param, body) ->
          let new_env = (param, v2) :: env in
          eval_expr body new_env
        | _ -> failwith "Nonfunction"

let rec eval_command (c : com) (env : environment) : environment =
    match c with 
    | Skip -> env
    | Comp(s1, s2) -> let env' = eval_command s1 env in eval_command s2 env'
    | Declare(t, x) -> (
      match t with
      | Int_Type -> (x,(Int_Val(0)))::env
      | Bool_Type -> (x,(Bool_Val(false)))::env
      | Lambda_Type -> (x, (Closure (env, "x", Var "x")))::env

    )
    | Assg(id, e) -> (
      let e_val = eval_expr e env in
    match (eval_expr (Var id) env) with
    | Bool_Val _ -> (
        match e_val with
        | Bool_Val _ -> (id, e_val)::env
        | _ -> raise TypeError
    )
    | Int_Val _ -> (
        match e_val with
        | Int_Val _ -> (id, e_val)::env
        | _ -> raise TypeError
    )
    | Closure(_, _, _) -> (
        match e_val with
        | Closure(_, _, _) -> (id, e_val)::env
        | _ -> raise TypeError
      )
    )
    | Cond(guard, iff, elses) -> (
      match (eval_expr guard env) with 
      | Bool_Val v -> (
        match v with
        | true -> eval_command iff env
        | false -> eval_command elses env
      )
      | _ -> raise TypeError
    )
    | While(guard, body) -> (
      match eval_expr guard env with
      | Bool_Val true -> let new_env = eval_command body env in
            eval_command (While(guard, body)) new_env
      | Bool_Val false -> env
      | _ -> raise TypeError
    )
    | For(guard, body) -> (
      let n = eval_expr guard env in (
       match n with
       | Int_Val x -> let new_env = eval_command body env in
        if x > 0 then eval_command (For(Number(x-1), body)) new_env else env
       | _ -> raise TypeError
      )
    )
