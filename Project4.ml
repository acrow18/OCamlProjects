(* === Parser and Interpreter for the language given in Project 4, CIS505/705, 
       Kansas State University, Fall 2022

This is a skeleton, with 5 lines where code needs to be changed.
Each such line is marked with "CHANGE #k" where k is a number,
indicating the suggested order in which to make the changes.

Even with the skeleton as given, you can still
  interpret certain simple expressions, such as    
     run "+ * 3 5 4"
 giving value 19.

The 5 tasks are

#1 == this is about how to evaluate a function application
     which is done slightly different from what we did in Project 3;
    you may take inspiration from the lecture notes
        15_MutationsInterpret/04_Boxes_FunctionCalls

Now you should be able to interpret
    run "@ lambda x + 2 x * 3 5"
 giving value 17

#2 == Observe that in the code for SetE, 
   the skeleton interpreter prints
      any number written to the location zero.
   You must therefore ensure that in the initial store,
       'IO' is bound (through a location) to an object
       with a field 'port' that resides in location 0

Now you should be able to interpret
   run "let_ set! port IO 4 
        let_ set! port IO 7
        8"
which will print
4
7
- : int = 8

#3 == this is about reading from the input stream,
 which must be done if the field given to GetE
  resides in the location designated for input/output.

Now you should be able to interpret
    run2 "let x get port IO
          let y get port IO
          - x y"
     [21 ; 4] 
 giving value 17

#4 ==  this is about how to change the value of a field,
     which is done by a suitable update of the store.
   (Usually, we don't care about the *value* being returned;
      the skeleton returns the value of the second expression
      and you may keep it that way.)

Now you should be able to interpret
     run "let x new a
          let_ set! a x 7
          let q get a x
          let_ set! a x 22
          let r get a x
         + q r"
  giving value 29

#5 == this is about how to create a new object;
    you must make sure that its field is bound to a *fresh* location!

Now you should be able to interpret
    run "let x new a
         let y new b
         let_ set! a x 12
         let_ set! b y 14
       + get a x get b y"
   giving value 26     

as well as all examples in the question text 
(and even in the upload file with test sets).

You should develop your interpreter incrementally, 
and make sure that each new change type checks! 
(If you do not type check until you have made all the required changes,
it is rather likely that you'll get a bunch of error messages 
that may be very hard to understand and fix.)

*)

(* CONCRETE SYNTAX

exp ::= id
     |  num
     |  "lambda" id exp0
     |  "@" exp1 exp2
     |  "let" id exp1 exp0
     |  "let_" exp1 exp0
     |  "if" exp0 exp1 exp2
     |  op exp1 exp2
     |  "new" id
     |  "get" id exp1
     |  "set!" id exp1 exp2

op ::= "+" (overloaded)
     |  "-"
     |  "*"

*)

(* EXCEPTIONS *)

exception InputEndsTooEarly
exception InputEndsTooLate
exception IdentifierExpectedBut of string
exception NotDeclared of string
exception TestNotNumber
exception PlusNotNumbersOrObjects
exception MinusNotNumbers
exception TimesNotNumbers
exception AppNotClosure
exception GetNotObject
exception SetNotObject
exception InputExhausted
exception OutputNotNumber
exception FinalNotNumber

(* ABSTRACT SYNTAX *)

type identifier = string

type expE =
 | IdE of identifier
 | NumE of int
 | LambdaE of identifier * expE
 | AppE of expE * expE
 | LetE of identifier * expE * expE
 | SeqE of expE * expE
 | IfE of expE * expE * expE
 | PlusE of expE * expE
 | MinusE of expE * expE
 | TimesE of expE * expE
 | NewE of identifier
 | GetE of identifier * expE
 | SetE of identifier * expE * expE

(* SCANNER *)

type tokenT = 
 | LambdaT
 | AppT
 | LetT
 | SeqT
 | IfT
 | PlusT
 | MinusT
 | TimesT
 | NewT
 | GetT
 | SetT
 | IdT of identifier
 | NumT of int

let print_token : tokenT -> string = fun token -> 
 match token with
 | LambdaT -> "lambda"
 | AppT -> "@"
 | LetT -> "let"
 | SeqT -> "let_"
 | IfT -> "if"
 | PlusT -> "+"
 | MinusT -> "-"
 | TimesT -> "*"
 | NewT -> "new"
 | GetT -> "get"
 | SetT -> "set!"
 | (IdT id) -> ("identifier "^id)
 | (NumT n) -> "number"

let is_digit : char -> bool = fun ch ->
   Char.code ch >= Char.code '0' && Char.code ch <= Char.code '9'

let char2digit : char -> int = fun ch -> 
   Char.code ch - Char.code '0'

let is_letter : char -> bool = fun ch ->
     (Char.code ch >= Char.code 'a' && Char.code ch <= Char.code 'z')
  || (Char.code ch >= Char.code 'A' && Char.code ch <= Char.code 'Z')

let scanNum : string -> (int * string) = fun str ->
  let rec get_num acc str = 
    if str = "" 
    then (acc, str)
    else 
      let c = String.get str 0 and 
          str' = String.sub str 1 (String.length str - 1) in
      if is_digit c
      then get_num (10 * acc + (char2digit c)) str' 
      else (acc, str)
 in get_num 0 str

let scanId : string -> (string * string) = fun str ->
  let rec get_id acc str = 
    if str = "" 
    then (acc, str)
    else 
      let c = String.get str 0 and 
          str' = String.sub str 1 (String.length str - 1) in
      if is_letter c || is_digit c || c = '_' || c = '?' || c = '!'
      then get_id (acc ^ (String.make 1 c)) str'
      else (acc, str)
 in get_id "" str

let rec scan : string -> tokenT list = 
  fun str -> 
   if str = ""
   then []
   else let c = String.get str 0 
        and str1 = String.sub str 1 (String.length str - 1) in
   if is_digit c
   then let (n,str') = scanNum str
         in (NumT n :: (scan str'))
   else if is_letter (c)
   then let (s,str') = scanId str
     in let token =
       if s = "lambda" then LambdaT
       else if s = "let" then LetT
       else if s = "let_" then SeqT
       else if s = "if" then IfT
       else if s = "new" then NewT
       else if s = "get" then GetT
       else if s = "set!" then SetT
       else IdT s
     in (token :: scan str')
   else match c with
     | '@' -> AppT :: (scan str1)
     | '+' -> PlusT :: (scan str1)
     | '-' -> MinusT :: (scan str1)
     | '*' -> TimesT :: (scan str1)
     | _ -> scan str1

(* PARSER *)

let getIdT : tokenT list -> string * tokenT list =
  fun tokens -> 
   match tokens with
   | [] -> raise InputEndsTooEarly
   | (IdT id) :: tokens' -> (id, tokens')
   | (token :: _) -> 
       raise (IdentifierExpectedBut (print_token token))

let rec parseExp : tokenT list -> expE * tokenT list =
   fun tokens ->
    match tokens with
    | [] -> raise InputEndsTooEarly
    | (IdT s) :: tokens1 ->
         (IdE s, tokens1)
    | (NumT z) :: tokens1 ->
         (NumE z, tokens1)
    | AppT :: tokens1 ->
        let (e1, tokens2) = parseExp tokens1 in
        let (e2, tokens3) = parseExp tokens2 in
       (AppE(e1,e2), tokens3)
    | IfT :: tokens1 ->
        let (e1, tokens2) = parseExp tokens1 in
        let (e2, tokens3) = parseExp tokens2 in
        let (e3, tokens4) = parseExp tokens3 in
       (IfE(e1,e2,e3), tokens4)
    | PlusT :: tokens1 ->
        let (e1, tokens2) = parseExp tokens1 in
        let (e2, tokens3) = parseExp tokens2 in
       (PlusE(e1,e2), tokens3)
    | MinusT :: tokens1 ->
        let (e1, tokens2) = parseExp tokens1 in
        let (e2, tokens3) = parseExp tokens2 in
       (MinusE(e1,e2), tokens3)
    | TimesT :: tokens1 ->
         let (e1, tokens2) = parseExp tokens1 in
         let (e2, tokens3) = parseExp tokens2 in
       (TimesE(e1,e2), tokens3)
    | SeqT :: tokens1 ->
         let (e1, tokens2) = parseExp tokens1 in
         let (e2, tokens3) = parseExp tokens2 in
       (SeqE(e1,e2), tokens3)
    | LambdaT :: tokens1 ->
         let (fp, tokens2) = getIdT tokens1   in
         let (e0, tokens3) = parseExp tokens2 in
       (LambdaE(fp,e0), tokens3)
    | LetT :: tokens1 ->
         let (id1, tokens2) = getIdT tokens1  in
         let (e1, tokens3) = parseExp tokens2 in
         let (e2, tokens4) = parseExp tokens3 in
       (LetE(id1,e1,e2), tokens4)
    | NewT :: tokens1 ->
         let (field, tokens2) = getIdT tokens1 in
       (NewE field, tokens2)
    | GetT :: tokens1 ->
         let (field, tokens2) = getIdT tokens1 in
         let (e0, tokens3) = parseExp tokens2 in
       (GetE (field,e0), tokens3)
    | SetT :: tokens1 ->
         let (field, tokens2) = getIdT tokens1 in
         let (e0, tokens3) = parseExp tokens2 in
         let (e2, tokens4) = parseExp tokens3 in
       (SetE(field,e0,e2), tokens4)

let parse : string -> expE =
  fun input_string ->
    let tokens = scan input_string in
    let (exp,tokens1) = parseExp tokens
    in if tokens1 = []
       then exp
       else raise InputEndsTooLate

(* DICTIONARIES *)

type ('a,'b) dictionary =  'a -> 'b

let insertDict : 'a -> 'b -> ('a,'b) dictionary -> ('a,'b) dictionary =
  fun new_key new_val env ->
    fun key -> if key = new_key then new_val else env key

let retrieveDict : ('a,'b) dictionary -> 'a -> 'b =
  fun env key -> env key

let emptyDict : (string,'b) dictionary = 
  fun key -> raise (NotDeclared key)

let defaultDict: 'b -> ('a,'b) dictionary =
  fun default -> fun _ -> default 

let try1next2 : ('a,'b) dictionary -> ('a,'b) dictionary -> ('a,'b) dictionary =
  fun env1 env2 -> fun key ->
   try (env1 key) with
    | NotDeclared _ -> env2 key 

(* LOCATIONS *)

type location = int

let next_free = ref 1

let new_location : unit -> location = (* will return 2; 3; 4 ... *)
  fun () ->
    (next_free := !next_free + 1 ;
     !next_free)

(* VALUES and ENVIRONMENTS *)

type environment = (identifier,location) dictionary

type value =
   NumV of int
 | ClosureV of 
       identifier 
     * expE 
     * environment
 | ObjV of environment

(* STORES and STATES *)

type store = (location,value) dictionary

type inputStream = int list
type runTimeState = inputStream * store

(* EVALUATING EXPRESSIONS  *)

let rec eval : expE -> environment -> runTimeState -> value * runTimeState =
  fun exp env state ->
   match state with
     (inp,sto) ->
   match exp with
   | IdE id -> (retrieveDict sto (retrieveDict env id), state)
   | NumE n -> (NumV n, state)
   | LambdaE(id,exp1) -> 
       (ClosureV (id, exp1, env), state)
   | AppE(exp1,exp2) ->
      (let (v1,state1) = eval exp1 env state  in
       let (v2,state2) = eval exp2 env state1 in
      match v1 with
        | ClosureV(x,exp0,env0) ->
            (let loc = new_location () in
            match state2 with
              (inp2,sto2) -> eval exp0 (insertDict x loc env0) (inp2, insertDict loc v2 sto2))
              (*DONE*)
              (*interpret e1 to a value w/ possibly an updated store*)
              (*interpret e2 to a value w/ possibly an updated store*)
              (*interpret the closure body*)
              (*in environment body*)
              (*create a fresh location*)
              (*let newloc = new_location() (*unit -> location*)*)
              (*interpret b in an enviroment where x is bound to l*)
              (*and in a store except that l is bound to v*)
              (*(inp2,sto2) -> (NumV loc, state)) (* CHANGE #1 *)*)
        | _ -> raise AppNotClosure)
   | SeqE(exp1,exp2) ->
      (let (_,state1) = eval exp1 env state  in
       eval exp2 env state1)
   | LetE(id,exp1,exp2) ->
       eval (AppE (LambdaE(id,exp2), exp1)) env state
   | IfE(exp0,exp1,exp2) ->
       (match (eval exp0 env state) with
         | (NumV n, state0) ->
             if n > 0
             then eval exp1 env state0
             else eval exp2 env state0
         | _ -> raise TestNotNumber)
   | PlusE(exp1,exp2) ->
      (let (v1,state1) = eval exp1 env state  in
       let (v2,state2) = eval exp2 env state1 in
       match (v1,v2) with
        | (NumV n1, NumV n2) -> (NumV (n1 + n2), state2)
        | (ObjV env1, ObjV env2) 
            -> (ObjV (try1next2 env1 env2), state2)
        | _ -> raise PlusNotNumbersOrObjects
       )
   | MinusE(exp1,exp2) ->
      (let (v1,state1) = eval exp1 env state  in
       let (v2,state2) = eval exp2 env state1 in
      match (v1,v2) with
        | (NumV n1, NumV n2) -> (NumV (n1 - n2), state2)
        | _ -> raise MinusNotNumbers
      )
   | TimesE(exp1,exp2) ->
      (let (v1,state1) = eval exp1 env state  in
       let (v2,state2) = eval exp2 env state1 in
      match (v1,v2) with
        | (NumV n1, NumV n2) -> (NumV (n1 * n2), state2)
        | _ -> raise TimesNotNumbers
      )
   | NewE field ->
       (ObjV (insertDict field (new_location ()) emptyDict), state) (* CHANGE #5 *)
       (*bound field to a fresh location*)
       (*(ObjV (insertDict field 345 emptyDict), state) (* CHANGE #5 *)*)
   | GetE(field,exp0) ->
      (match eval exp0 env state with
        | (ObjV env0, (inp0, sto0)) -> 
            let loc = retrieveDict env0 field in
            if loc = 0 then (*CHANGE #5*)
            match inp0 with
            | (h :: t) -> (NumV h, (t, sto0)) 
            | _ -> raise InputExhausted
            else (retrieveDict sto0 loc, (inp0, sto0))
        | _ -> raise GetNotObject
      )
   | SetE(field,exp0,exp2) ->
      (let (v0,state0) = eval exp0 env state  in
       let (v2,state2) = eval exp2 env state0 in
       match (v0, state2) with
        | (ObjV env0, (inp2, sto2)) -> 
           (let loc = retrieveDict env0 field in
            match (loc, v2) with
            | (0, NumV n2) ->
                (print_string ((string_of_int n2)^"\n") ;
                 (v2, state2))
            | (0, _) -> raise OutputNotNumber
            | _ -> (v2, (inp2, insertDict loc v2 sto2))) (* CHANGE #4 *)
            (*changing value of a field*)
            (*done by a suitable update of the store*)
            (*| _ -> (v2, (inp2, sto))) (* CHANGE #4 *)*)
        | _ -> raise SetNotObject
      )
let run2 prog input = 
   let objIO = ObjV (insertDict "port" 0 emptyDict) in (* CHANGE #2 *)
   (*IO is bound to an object with field 'port' that is in location 0*)
   (*let objIO = NumV 986 in (* CHANGE #2 *)*)
   let initEnv = insertDict "IO" 1 emptyDict in
   let initSto = insertDict 1 objIO (defaultDict (NumV 0)) in
   let initState = (input, initSto) in
  try
    (match (eval (parse prog) initEnv initState) with
       | (NumV n, _) -> n
       | _ -> raise FinalNotNumber
    ) with
   | InputEndsTooEarly -> 
       (print_string "input program prematurely exhaused\n"; 0)
   | InputEndsTooLate ->
       (print_string "input continues after expression is parsed\n"; 0)
   | IdentifierExpectedBut s ->
       (print_string ("identifier expected but "^s^" seen\n"); 0)
   | NotDeclared s ->
       (print_string ("identifier "^s^" not bound to value\n"); 0)
   | TestNotNumber ->
       (print_string "test expression not a number\n"; 0)
   | AppNotClosure ->
       (print_string "function part of application does not evaluate to closure\n"; 0)
   | PlusNotNumbersOrObjects ->
       (print_string "'+' is not applied to two numbers or to two objects\n"; 0)
   | MinusNotNumbers ->
       (print_string "'-' has a non-number as argument\n"; 0)
   | TimesNotNumbers ->
       (print_string "'*' has a non-number as argument\n"; 0)
   | GetNotObject ->
       (print_string "field read from non-object\n"; 0)
   | SetNotObject ->
       (print_string "field written to non-object\n"; 0)
   | InputExhausted ->
       (print_string "input stream prematurely exhausted\n"; 0)
   | OutputNotNumber ->
       (print_string "value printed is not a number\n"; 0)
   | FinalNotNumber ->
       (print_string "final value of program is not a number\n"; 0)

let run prog =
   run2 prog []
