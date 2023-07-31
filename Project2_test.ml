  (* BASIC FEATURES *)
let test0 = "var n; var q; input q; input n; output (q-n); output (q+n)"
let result0 = interpret test0 [7; 3; 9]
  (* 
     val result0 : outputStream = [4; 10]
  *)

  (* WHILE LOOPS (factorial) *)
let test1 = "var n; var fac; input n; output n; fac = 1; while n {fac = (fac * n); n = (n-1)}; output fac"
let result1 = interpret test1 [5]
   (* 
      val result1 : outputStream = [5; 120]
   *)

  (* PROCEDURES (factorial) *)
let test2 = "var n; proc F {if n {fac = (fac * n); n = (n-1); call F} {skip}}; var fac; input n; output n; fac = 1; call F; output fac"
let result2 = interpret test2 [7]
  (*
      val result2 : outputStream = [7; 5040]
  *)

  (* renamed *)
let test2a = "var n; proc fac {if n {fac = (fac * n); n = (n-1); call fac} {skip}}; var fac; input n; output n; fac = 1; call fac; output fac"
let result2a = interpret test2a [8]
  (*
     val result2a : outputStream = [8; 40320]
  *)

  (* ONE-DIMENSIONAL ARRAYS *) 
let test3 = "array B[5]; var q; input [B 1]; input [B 2]; q = [B 2]; [B 2] = [B 1]; [B 1] = q; [B 3] = ([B 1] + [B 2]); output [B 0]; output [B 1]; output [B 2]; output [B 3]; output [B 4]"
let result3 = interpret test3 [2; 7]
 (*
    val result3 : outputStream = [0; 7; 2; 9; 0]
 *)

  (* ALIASING INTEGER VARIABLES *)
let test4 = "var x; var y; alias ax x; var z; alias aax ax; input x; input y; input z; output x; output y; output ax; output z; output aax"
let result4 = interpret test4 [5; 3; 7; 8]
  (*
val result4 : outputStream = [5; 3; 5; 7; 5]
  *)

  (* INSERTION SORT *) 
let test5 = "var n; var i; var j; var t;"^ 
   "array Q[20]; input n;"^
   "i = 0; while (n - i) {input [Q i]; i = (i + 1)};"^
   "i = 0; while (n - i)"^
     "{j = i;"^
     "while j {"^
        "if ([Q (j-1)] - [Q j])"^
          "{t = [Q (j-1)]; [Q (j-1)] = [Q j]; [Q j] = t; j = (j-1)}"^
          "{j = 0}};"^
     "i = (i+1)};"^
   "i = 0; while (n - i) {output [Q i]; i = (i+1)}"
let result5 = interpret test5 [7; 14; 11; 4; 8; 10; 17; 3]
 (* 
   val result5 : outputStream =
      [3; 4; 8; 10; 11; 14; 17]
 *)

  (* INSERTION SORT with ALIASING of third element *)
let test5a = "var n; var i; var j; var t;"^ 
   "array Q[20]; alias third [Q 2]; input n;"^
   "i = 0; while (n - i) {input [Q i]; i = (i + 1)};"^
   "i = 0; while (n - i)"^
     "{j = i;"^
     "while j {"^
        "if ([Q (j-1)] - [Q j])"^
          "{t = [Q (j-1)]; [Q (j-1)] = [Q j]; [Q j] = t; j = (j-1)}"^
          "{j = 0}};"^
     "output third;"^
     "i = (i+1)};"^
   "i = 0; while (n - i) {output [Q i]; i = (i+1)}"
let result5a = interpret test5a [7; 14; 11; 4; 8; 10; 17; 3]
 (* 
   val result5a : outputStream =
      [4; 4; 14; 11; 10; 10; 8; 3; 4; 8; 10; 11; 14; 17]
 *)

  (* MATRIX MULTIPLICATION: 2x3 times 3x2 *) 
     (* cij = sum_k aik bkj *)
let test6 = "array A[2,3]; array B[3,2]; array C[2,2]; var i; var j; var k; i = 0; while (2 - i) {k = 0; while (3 - k) {input [[A i] k]; k = (k + 1)}; i = (i + 1)}; k = 0; while (3 - k) {j = 0; while (2 - j) {input [[B k] j]; j = (j + 1)}; k = (k + 1)}; i = 0; while (2 - i) {j = 0; while (2 - j) {[[C i] j] = 0; k = 0; while (3 - k) {[[C i] j] = ([[C i] j] + ([[A i] k] * [[B k] j])); k = (k + 1)}; output [[C i] j]; j = (j + 1)}; i = (i + 1)}"

let result6 = interpret test6 [3; 5; 4; 2; 7; 6; 1; 9; 8; 0; 4; 2]
 (*

      3  5  4        1  9      59  35  
      2  7  6   X    8  0      82  30
                     4  2

  val result6 : outputStream = [59; 35; 82; 30]
 *)

   (* MATRIX MULTIPLICATION with ALIASING of 2nd row *)
let test6a = "array A[2,3]; array B[3,2]; array C[2,2]; alias C1 [C 1]; var i; var j; var k; i = 0; while (2 - i) {k = 0; while (3 - k) {input [[A i] k]; k = (k + 1)}; i = (i + 1)}; k = 0; while (3 - k) {j = 0; while (2 - j) {input [[B k] j]; j = (j + 1)}; k = (k + 1)}; i = 0; while (2 - i) {j = 0; while (2 - j) {[[C i] j] = 0; k = 0; while (3 - k) {[[C i] j] = ([[C i] j] + ([[A i] k] * [[B k] j])); k = (k + 1)}; j = (j + 1)}; i = (i + 1)}; j = 0; while (2 - j) {output [C1 j]; j = (j+1) }"

let result6a = interpret test6a [3; 5; 4; 2; 7; 6; 1; 9; 8; 0; 4; 2]

(*
  val result6a : outputStream = [82; 30]
*)

(* ERRORS *)   

let error1a = interpret "output X" []
   (* expect: "*** ERROR: X used but not declared" *)
let error1b = interpret "call F" []
   (* expect: "*** ERROR: F used but not declared" *)
let error2 = interpret "var w; var z; input w; input z" [6]
   (* expect: "*** ERROR: input stream prematurely exhausted" *)
let error3 = interpret "array M[7,0,5]; output 27" []
   (* expect: "*** ERROR: M declared with a zero bound " *)
let error4a = interpret "var A; [A 3] = 9" []
   (* expect: "*** ERROR: a scalar consider an array" *)
let error4b = interpret "array A[7]; [[A 3] 2] = 6" []
   (* expect: "*** ERROR: a scalar consider an array" *)
let error5a = interpret "array z[5]; output z" []
   (* expect: "*** ERROR: an array considered a scalar" *)
let error5b = interpret "array B[5,7]; input [B 3]" [9]
   (* expect: "*** ERROR: an array considered a scalar" *)
let error6a = interpret "array B[6]; [B 6] = 4" []
   (* expect: "*** ERROR: index 6 but allowed range is 0..5" *)
let error6b = interpret "array C[7, 4]; [[C 3] (1-2)] = 8" []
   (* expect: "*** ERROR: index -1 but allowed range is 0..3" *)

