(* various test sets for Project 4, 2022 *)

(*  Numbers and Operators *)

let test1a = run "47"
(* 
val test1a : int = 47
*)

let test1b = run "* + 11 5 7"
(* 
val test1b : int = 112
*)

let test1c = run "* + 3 8 - 9 4"
(* 
val test1c : int = 55
*)

(*  Conditionals *)

let test2a = run " 
    if - 3 2  
       * 7 9 
       - 7 9"
(* 
val test2a : int = 63
*)

let test2b = run "
   if - 3 6 
      0
      if - 8 8 
         11
         if - 8 7
            33
            44"
(* 
val test2b : int = 33
*)

(*  Lambda, @, Let *)

let test3a = run "
  @ lambda w + w w 
  14"
(* 
val test3a : int = 28
*)
 
let test3b = run "
   @
     @ lambda f lambda y @ f @ f y
       lambda h + h 7
     4"
(* 
val test3b : int = 18
*)

let test3c = run "
   let x 6 + x 25"
(* 
val test3c : int = 31
*)

let test3d = run "
   let mult lambda w lambda y * y w
   let mult6 @ mult 6
  @ mult6 16"
(* 
val test3d : int = 96
*)

let test3e = run "
   let plus4 lambda x + x 4
   let twice lambda h lambda yy @ h @ h yy
  @
    @ twice 
      plus4
    15"
(* 
val test3e : int = 23
*)

(*  Sequential, New, Get, Set! *)

let test4a = run "
   let y new a
   let_ set! a y 28 
  + get a y 
    6"
(* 
val test4a : int = 34
*)

let test4b = run "
   let w + new a new b
   let_ set! a w 49
   let_ set! b w 95
  - get b w 
    get a w"
(* 
val test4b : int = 46
*)

let test4c = run "
   let x new h
   let y new h
   let_ set! h x 16
   let_ set! h y 13
  + get h y 
    get h x"
(* 
val test4c : int = 29
*)

let test4d = run "
   let z new b
   let_ set! b z 4
  + let_ set! b z 5
      7
    get b z"
(* 
val test4d : int = 12   (7 + 5)
*)

let test4e = run "
   let z new b
   let_ set! b z 4
  + get b z
    let_ set! b z 5
      7"
(* 
val test4e : int = 11  (4 + 7)
*)

let test4f = run "
   let z new a
   let_ set! a z 1
  + let_ set! a z 7
       get a z
    let_ set! a z + get a z 2
       @ lambda y * y 3 
         get a z"
(* 
val test4f : int = 34  (7 + 9 x 3)
*)

let test4g = run "
   let z new a
   let_ set! a z 1
  + let_ set! a z + get a z 2
       @ lambda y * y 3 
         get a z
    let_ set! a z 7
      get a z"
(* 
val test4g : int = 16  (3 * 3 + 7)
*)

let test4h = run "
   let w + new n new add
   let_ set! n w 7
   let_ set! add w 
                lambda x set! n w + x get n w
   let_ @ get add w 
          10 
   let_ @ get add w 
          13
  get n w"
(* 
val test4h : int = 30
*)

(*  Input and Output *)

let test5a = run2 "
  - get port IO 
    get port IO" 
  [85 ; 40 ; 3]
(* 
val test5a : int = 45
*)

let test5b = run "
   let_ set! port IO 17
   let_ set! port IO 23
  14"
(*
17
23
val test5b : int = 14
*)

let test5c = run2 "
   let x get port IO
   let_ set! port IO + x 3
   let y get port IO
   let_ set! port IO - y x
   let v get port IO
  v"
    [34 ; 58; 52]
(*
37
24
val test5c : int = 52
*)
 
let test5d = run "
   let loc new counter
   let_ set! counter loc 0
   let fresh 
      lambda void 
         let_ set! counter loc 
               + 1 get counter loc
        get counter loc
   let_ set! port IO @ fresh 0
   let_ set! port IO @ fresh 0
   let_ set! port IO @ fresh 0
  0" 
(* 
1
2
3
val test5d : int = 0
*)

let test5e = run "
   let fresh 
         lambda void
           let loc new counter
           let_ set! counter loc 0 
           let_ set! counter loc 
                  + 1 get counter loc 
          get counter loc
   let_ set! port IO @ fresh 0
   let_ set! port IO @ fresh 0
   let_ set! port IO @ fresh 0
  234"
(*     
1
1
1
val test5e : int = 234
*)

(*  Syntax Errors *) 
 
let error1a = run "+ 2"
(*
input program prematurely exhaused
val error1a : int = 0
*)

let error1b = run "+ 2 3 4"
(* 
input continues after expression is parsed
val error1b : int = 0
*)

let error1c = run "@ lambda 6 8"
(* 
identifier expected but number seen
val error1c : int = 0
*)

let error1d = run "let 7 5 3"
(* 
identifier expected but number seen
val error1d : int = 0
*)

let error1e = run "let x new 4 5"
(* 
identifier expected but number seen
val error1e : int = 0
*)

let error1f = run "let x new c get @ x"
(* 
identifier expected but @ seen
val error1f : int = 0
*)

let error1g = run "let x new c set! if x 8" 
(* 
identifier expected but if seen
val error1g : int = 0
*)

(*  Semantic Errors *)

let error2a = run "let y 5 + y w"
(*
identifier w not bound to value
val error2a : int = 0
*)

let error2b = run "let x new c get a x"
(* 
identifier a not bound to value
val error2b : int = 0
*)

let error2c = run "let x new c set! b x 8"
(* 
identifier b not bound to value
val error2c : int = 0
*)

let error2d = run "@ 5 8"
(* 
function part of application does not evaluate to closure
val error2d : int = 0
*)

let error2e = run "let x new c @ x 4"
(* 
function part of application does not evaluate to closure
val error2e : int = 0
*)

let error2f = run "if lambda x x 7 8"
(*
test expression not a number
val error2f : int = 0
*)

let error2g = run "+ 3 lambda x x"
(* 
'+' is not applied to two numbers or to two objects
val error2g : int = 0
*)

let error2h = run "let x new a - 8 x"
(* 
'-' has a non-number as argument
val error2h : int = 0
*)

let error2i = run "* lambda w w 5"
(*
'*' has a non-number as argument
val error2i : int = 0
*)

let error2j = run "let x lambda w w get a x"
(*
field read from non-object
val error2j : int = 0
*)

let error2k = run "let z 7 get a z"
(* 
field read from non-object
val error2k : int = 0
*)

let error2l = run "let x lambda w w set! b x 7"
(* 
field written to non-object
val error2l : int = 0
*)

let error2m = run "let z 7 set! bb z 8"
(* 
field written to non-object
val error2m : int = 0
*)

let error2n = run2 "
  let_ get port IO 
    get port IO" 
  [7]
(*
input stream prematurely exhausted
val error2n : int = 0
*)

let error2o = run "
  set! port IO lambda x x"
(*
value printed is not a number
val error2o : int = 0
*)

let error2p = run "lambda x x"
(* 
final value of program is not a number
val error2p : int = 0
*)

let error2q = run "new c"
(* 
final value of program is not a number
val error2q : int = 0
*)


