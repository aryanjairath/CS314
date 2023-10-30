(******************************)
(*** For debugging purposes ***)
(******************************)

(* print out an integer list *)
let rec print_int_list lst =
  match lst with
  | [] -> ()
  | [x] -> print_int x; print_newline ()
  | x :: xs -> print_int x; print_string "; "; print_int_list xs

(* print out a string list *)
let rec print_string_list lst =
  match lst with
  | [] -> ()
  | [x] -> print_string x; print_newline ()
  | x :: xs -> print_string x; print_string "; "; print_string_list xs


(********************)
(* Problem 1: pow *)
(********************)

let rec pow x p =
  if p = 0 then 
    1
  else
    x*pow x (p-1)

(********************)
(* Problem 2: range *)
(********************)

let rec getrange num1 num2 arr = 
  if num2 < num1 then
    arr
  else
    getrange num1 (num2-1) (num2::arr)

let rec range num1 num2 =
  getrange num1 num2 []


(**********************)
(* Problem 3: flatten *)
(**********************)
 
let rec flattenIt l arr = 
  match l with
  | [] -> arr
  | first::tail -> flattenIt tail (arr @ first)
let rec flatten l =
  flattenIt l []

(*****************************)
(* Problem 4: remove_stutter *)
(*****************************)

let rec remove_stutter l =
  match l with
  | [] -> []
  | [x] -> [x]
  | x::y::z -> if x = y then remove_stutter (y::z)
  else
    x::(remove_stutter (y::z))

(*********************)
(* Problem 5: rotate *)
(*********************)

let rec last_element lst =
  match lst with
  | [] -> " "
  | [x] -> x
  | h1::t1 -> last_element t1
  
let rec all_except_last lst = 
  match lst with
  | [] -> []
  | [x] -> []
  | h1::t1 -> h1 :: all_except_last t1
let rec rotate l n =
  if n = 0 then
    l
  else 
    let x = last_element l in
    let z = all_except_last l in
    rotate(x::z)(n-1)
  

(*******************)
(* Problem 6: jump *)
(*******************)

let rec jumpRec lst1 lst2 arr ind =
  match (lst1, lst2) with
  | ([],[]) -> List.rev arr
  | ([], h1::t1) ->  List.rev arr
  | (h1::t1, []) ->  List.rev arr
  | (h1::t1,h2::t2) -> 
  if ind mod 2 = 0 then
    jumpRec t1 t2 (h2::arr) (ind+1)
  else 
    jumpRec t1 t2 (h1::arr) (ind+1)

let jump lst1 lst2 =
  jumpRec lst1 lst2 [] 0

(******************)
(* Problem 7: nth *)
(******************)

let rec findNth l n arr curr = 
    match l with
    | [] -> List.rev arr
    |h1::t1 -> 
    if curr mod n = 0 then 
      findNth t1 n (h1::arr) (curr+1)
    else 
      findNth t1 n arr (curr+1)
let nth l n =
  findNth l n [] 1

(*****************************************************)
(* Problem 8: Digital Roots and Additive Persistence *)
(*****************************************************)

(* digits : int -> int list
 * we assume n >= 0
 * (digits n) is the list of digits of n in the order in which they appear in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *)

let rec findDigits n arr = 
  if n < 0 then
    []
  else if n = 0 then
    arr
  else
    findDigits (n/10) ((n mod 10)::arr)
let rec digitsOfInt n =
  findDigits n []


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits,
 * then adding the digits of the number derived from it, etc.,
 * until the remaining number has only one digit.
 * The number of additions required to obtain a single digit from a number n
 * is called the additive persistence of n, and the digit obtained is called
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)

let rec sumDigits lst =
  match lst with 
  | [] -> 0
  | [x] ->  x
  | h1::t1 -> h1+sumDigits(t1)

let rec additivePersistence n =
  if n < 10 then 
    0
  else
    1+additivePersistence(sumDigits (findDigits n []))


let rec digitalRoot n =
  if n < 10 then 
    begin
      n
    end
  else
    begin
      digitalRoot (sumDigits (findDigits n []))
    end
;;

(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  (* Testcases for pow*)
  let _ =
    try
      assert (pow 3 1 = 3);
      assert (pow 3 2 = 9);
      assert (pow (-3) 3 = -27)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for range *)
  let _ =
    try
      assert (range 2 5 = [2;3;4;5]);
      assert (range 0 0 = [0])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for flatten *)
  let _ =
    try
      assert (flatten [[1;2];[3;4]] = [1;2;3;4]);
      assert (flatten [[1;2];[];[3;4];[]] = [1;2;3;4])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for remove_stutter *)
  let _ =
    try
      assert (remove_stutter [1;2;2;3;1;1;1;4;4;2;2] = [1; 2; 3; 1; 4; 2]);
      assert (remove_stutter [] = []);
      assert (remove_stutter [1;1;1;1;1] = [1]);
      assert (remove_stutter [1;1;1;1;1;2] = [1;2])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for rotate *)
  let _ =
    try
      assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 2 = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]);
      assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 0 = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"]);
      assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 7 = ["b"; "c"; "d"; "e"; "f"; "g"; "h"; "a"])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for jump *)
  let _ =
    try
      assert (jump ["first"; "second"; "third"; "fourth"] ["fifth"; "sixth"; "seventh"; "eighth"] = ["fifth"; "second"; "seventh"; "fourth"]);
      assert (jump [1; 3; 5; 7] [0; 2; 4; 6; 8] = [0; 3; 4; 7]);
      assert (jump ["a"; "b"] ["c"] = ["c"])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for nth *)
  let _ =
    try
      (*print_int_list (nth [1; 2; 3; 4; 5; 6; 7] 1);*)
      assert (nth [1; 2; 3; 4; 5; 6; 7] 1 = [1; 2; 3; 4; 5; 6; 7]);
      assert (nth [1; 2; 3; 4; 5; 6; 7] 2 = [2; 4; 6]);
      assert (nth [1; 2; 3; 4; 5; 6; 7] 3 = [3; 6])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for digitsOfInt *)
  let _ =
    try
      assert (digitsOfInt 3124 = [3;1;2;4]);
      assert (digitsOfInt 352663 = [3;5;2;6;6;3]);
      assert (digitsOfInt 31243 = [3;1;2;4;3]);
      assert (digitsOfInt 23422 = [2;3;4;2;2])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for additivePersistence *)
  let _ =
    try
      assert (additivePersistence 9876 = 2)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for digitalRoot *)
  let _ =
    try
      assert (digitalRoot 9876 = 3)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  Printf.printf ("%d out of 10 programming questions are correct.\n") (10 - !error_count)

let _ = main()
