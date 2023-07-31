(* various test sets for Project 3, 2022 *)

(* Basic Features *)

let test1 = run "@ lambda x x 5"
 (*
val test1 : int list = [5]
 *)

let test2 = run "@ lambda x + x 7 2"
 (*
val test2 : int list = [9]
 *)

let test3 = run 
 "let plus4 lambda x + x 4
    @ plus4 28"
 (*
val test3 : int list = [32]
 *)
 
let test4 = run
  "let plus lambda x lambda y + x y 
   let plusseven @ plus 7
  @ plusseven 5"
 (*
val test4 : int list = [12]
 *)

let test5 = run
  "let twice lambda G lambda x @ G @ G x
   let multfour lambda x * 4 x 
  @ @ twice multfour 3"
 (*
val test5 : int list = [48]
 *)

let testFac0 = run
  "let fac lambda n
    if n 
     * n 
       @ fac - n 1 
     1 
  @ fac 6"
 (*
identifier fac not bound to value
val testFac0 : int list = [0]
 *)

(* Major Examples *)

let testFac = run
  "let Z lambda f @ lambda x @ f lambda v @ @ x x v lambda x @ f lambda v @ @ x x v
   let Fac lambda f lambda n 
   if n 
      * n @ f - n 1 
      1
  @ @ Z Fac 6"
 (*
val testFac : int list = [720]
 *)

let testMap = run
  "let Z lambda f @ lambda x @ f lambda v @ @ x x v lambda x @ f lambda v @ @ x x v
   let Map lambda map lambda f lambda L
   if first L
      + @ f first L
        @ @ map f 
        - L 1
     L
  @ @ @ Z Map 
    lambda x + 2 x
    + 4 + 5 * 4 list 8"
 (*
val testMap : int list = [6; 7; 10; 10; 10; 10]
 *)

(* List Operators *)

let testSum = run "
  let list56 + 5 list 6
  let list78 + list 7 8
   + list56 list78"
(*
val testSum : int list = [5; 6; 7; 8]
*)

let testProdDiff = run "
  let list1 = * 3 + list 5 7
   - list1 3"
(*
val testProdDiff : int list = [7; 5; 7]
*) 

let testEmpty = run "
  - list 5 1"
(*
val testEmpty : int list = []
*)

(* Typical Semantic Errors *)

let errorSemantic1 = run "@ lambda x w 7"
  (*
identifier w not bound to value
val errorSemantic1 : int list = [0]
  *)

let errorSemantic2 = run "lambda z + z 4"
  (*
program returns closure
val errorSemantic2 : int list = [0]
  *)

let errorSemantic3 = run "@ 8 7"
  (*
function part of application does not evaluate to closure
val errorSemantic3 : int list = [0]
  *)

let errorSemantic4 = run "if lambda x x 6 7"
  (*
test expression not integer
val errorSemantic4 : int list = [0]
  *)

let errorSemantic5 = run "list lambda x x"
  (*
attempt to make list of non-integers
val errorSemantic5 : int list = [0]
  *)

let errorSemantic6 = run "first 7"
  (*
argument to 'first' not list
val errorSemantic6 : int list = [0]
  *)

let errorSemantic7 = run "+ 4 lambda x x"
  (*
'+' has closure as operand
val errorSemantic7 : int list = [0]
  *)

let errorSemantic8 = run "- 7 lambda x x"
  (*
'-' has closure as operand, or list as 2nd operand
val errorSemantic8 : int list = [0]
  *)

let errorSemantic9 = run "- 1 * 3 list 4"
  (*
'-' has closure as operand, or list as 2nd operand
val errorSemantic9 : int list = [0]
  *)

let errorSemantic10 = run "* lambda x x 7"
  (*
'*' has closure as operand, or list as 1st operand
val errorSemantic10 : int list = [0]
  *)

let errorSemantic11 = run "* list 4 5"
  (*
'*' has closure as operand, or list as 1st operand
val errorSemantic11 : int list = [0]
  *)


(* Typical Syntax Errors *)

let errorSyntax1 = run "+ 3"
 (*
input prematurely exhaused
val errorSyntax1 : int list = [0]
 *)

let errorSyntax2 = run "+ 3 4 5"
 (*
input continues after expression is parsed
val errorSyntax2 : int list = [0]
 *)

let errorSyntax3 = run "let + 4 5"
 (*
identifier expected but + seen
val errorSyntax3 : int list = [0]
 *)


