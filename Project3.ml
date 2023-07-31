(* === Parser and Interpreter for the language given in Project 3, CIS505/705, 
       Kansas State University, Fall 2022

This is a skeleton, with 7 lines where code needs to be changed.
Each such line is marked with "CHANGE #k" where k is a number,
indicating the suggested order in which to make the changes.

Even the current skeleton enables the user to have dialogues such as

# run "+ 2 3" ;;
- : int list = [5]

The 7 tasks are

#1 == this is about how to evaluate an identifier;
     it should be looked up in the environment

#2 == this is about how to evaluate a function definition;
      the appropriate closure should be created

#3 == this is about how to evaluate a function application,
    after the function part has been evaluated to a closure
      and after the argument part has been evaluated;
    this involves calling the closure body in an environment
       that implements static scope 

The above 3 changes will allow us to run all programs that involve only function application and abstraction, and for example get the dialogue

# run "@ lambda x + x 7   
         2"  ;;
- : int list = [9]

#4 == how to parse 'let'
    take inspiration from how 'lambda' is parsed.

This will allow the dialogue

# run "let x + 1 2 + x 3" ;;
- : int list = [6]

and even the factorial program from the question text:

# run "let Z lambda f @ lambda x @ f lambda v @ @ x x v lambda x @ f
                                     lambda v @ @ x x v
   let Fac lambda f lambda n 
   if n 
      * n @ f - n 1 
      1
  @ @ Z Fac 6" ;;            
- : int list = [720]

#5 ==  extend '+' to handle the case where one or both operands are lists, 
    as described in the question text,
   but raise the appropriate exception otherwise
  
This will allow the dialogues

# run "
   let list56 + 5 list 6
   let list78 + list 7 8
    + list56 list78"      ;;
- : int list = [5; 6; 7; 8]

# run "+ 4 lambda x x" ;;
'+' has closure as operand
- : int list = [0]

#6 == extend '*' to handle the case where the second operand is a list,
   as described in the question text, 
   but raise the appropriate exception otherwise

This will allow the dialogues

# run "* 3 + list 5 7" ;;
- : int list = [5; 7; 5; 7; 5; 7]

# run "* lambda x x 7" ;;
'*' has closure as operand, or list as 1st operand
- : int list = [0]

# run "* list 4 5" ;;
'*' has closure as operand, or list as 1st operand
- : int list = [0]

#7 == extend '-' to handle the case where the first operand is a list,
   as described in the question text, 
   but raise the appropriate exception otherwise

This will allow the dialogues

# run "let list1 = * 3 + list 5 7
     - list1 3"  ;;
- : int list = [7; 5; 7]

# run "- 7 lambda x x" ;;
'-' has closure as operand, or list as 2nd operand
- : int list = [0]

# run "- 1 * 3 list 4" ;;
'-' has closure as operand, or list as 2nd operand
- : int list = [0]

and also allows us to run the list map program:

# run
  "let Z lambda f @ lambda x @ f lambda v @ @ x x v lambda x @ f lambda v @ @ x x v
   let Map lambda map lambda f lambda L
   if first L
      + @ f first L
        @ @ map f 
        - L 1
     L
  @ @ @ Z Map 
    lambda x + 2 x
    + 4 + 5 * 4 list 8" ;;
- : int list = [6; 7; 10; 10; 10; 10]

as well as all other programs from the question text!

You should develop your interpreter incrementally, 
and make sure that each new change type checks! 
(If you do not type check until you have made all the required changes,
it is rather likely that you'll get a bunch of error messages 
that may be very hard to understand and fix.)

*)

(* CONCRETE SYNTAX

exp ::= id
     |  num
     |  "lambda" id exp
     |  "@" exp1 exp2
     |  "let" id exp1 exp2
     |  "if" exp1 exp2 exp3
     |  "list" exp
     |  "first" exp1
     |  op exp1 exp2 

 op ::= "+"  (overloaded)
     |  "-"  (overloaded)
     |  "*"  (overloaded)

*)

(* EXCEPTIONS *)

exception InputEndsTooEarly
exception InputEndsTooLate
exception IdentifierExpectedBut of string
exception NotDeclared of string
exception ListNotInteger
exception FirstNotList
exception TestNotInteger
exception AppNotClosure
exception PlusWrongArgs
exception MinusWrongArgs
exception TimesWrongArgs
exception OutputClosure

(* ABSTRACT SYNTAX *)

type identifier = string

type expE =
 | IdE of identifier
 | NumE of int
 | LambdaE of identifier * expE
 | AppE of expE * expE
 | LetE of identifier * expE * expE
 | IfE of expE * expE * expE
 | ListE of expE
 | FirstE of expE
 | PlusE of expE * expE
 | MinusE of expE * expE
 | TimesE of expE * expE

(* SCANNER
    converts the input string into a list of "tokens" *)

type tokenT = 
 | LambdaT
 | AppT
 | LetT
 | IfT
 | ListT
 | FirstT
 | PlusT
 | MinusT
 | TimesT
 | IdT of identifier
 | NumT of int

let print_token token = match token with
 | LambdaT -> "lambda"
 | AppT -> "@"
 | LetT -> "let"
 | IfT -> "if"
 | ListT -> "list"
 | FirstT -> "first"
 | PlusT -> "+"
 | MinusT -> "-"
 | TimesT -> "*"
 | (IdT id) -> ("identifier "^id)
 | (NumT n) -> "number"

let is_digit(ch) = 
   Char.code ch >= Char.code '0' && Char.code ch <= Char.code '9'

let char2digit(ch) = Char.code ch - Char.code '0'

let is_letter(ch) = 
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
      if is_letter c || is_digit c || c = '_'
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
       else if s = "if" then IfT
       else if s = "list" then ListT
       else if s = "first" then FirstT
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
    | ListT :: tokens1 ->
        let (e1, tokens2) = parseExp tokens1 in
       (ListE e1, tokens2)
    | FirstT :: tokens1 ->
        let (e1, tokens2) = parseExp tokens1 in
       (FirstE e1, tokens2)
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
    | LambdaT :: tokens1 ->
         let (fp, tokens2) = getIdT tokens1   in
         let (e0, tokens3) = parseExp tokens2 in
       (LambdaE(fp,e0), tokens3)
    (*needs, id, expE, expE*)
    | LetT :: tokens1 -> 
        let (fp, tokens2) = getIdT tokens1 in (*id*)
        let (e0, tokens3) = parseExp tokens2 in (*expE*)
        let (e1, tokens4) = parseExp tokens3 in (*expE*)
       (LetE(fp, e0, e1), tokens4) (*identifier * expE * expE*)
    (*LetT :: tokens1 -> (NumE 457, tokens1) (* CHANGE #4 *)*)

let parse : string -> expE =
  fun input_string ->
    let tokens = scan input_string in
    let (exp,tokens1) = parseExp tokens
    in if tokens1 = []
       then exp
       else raise InputEndsTooLate

(* ENVIRONMENTS *)

type 'a environment =  identifier -> 'a

let initEnv : 'a environment = 
  fun id -> raise (NotDeclared id)

let insertEnv : identifier -> 'a -> 'a environment -> 'a environment =
  fun new_id a env ->
    fun id -> if id = new_id then a else env id

let retrieveEnv : 'a environment -> identifier -> 'a =
  fun env id -> env id

(* VALUES *)

type value =
   NumV of int
 | ListV of int list
 | ClosureV of identifier * expE * value environment

(* EVALUATING EXPRESSIONS *)

let rec eval exp env =
   match exp with
   | IdE id -> retrieveEnv env id
   (*IdE id -> NumV 159 (* CHANGE #1 *)*)
   | NumE n -> NumV n
   | LambdaE(id,exp1) -> 
      (*store id, and store exp*) (*create closure*)
      ClosureV(id, exp1, env)
    (*LambdaE(id,exp1) -> 
        NumV 987   (* CHANGE #2 *)*)
   | AppE(exp1,exp2) ->
       (match (eval exp1 env, eval exp2 env) with
         | (ClosureV(x,exp0,env0), v2) ->
            (eval exp0 (insertEnv x v2 env0)) (*inserts closure into enviroment*)
              (* NumV 789  (* CHANGE #3 *)*)
         | _ -> raise AppNotClosure)
   | LetE(id,exp1,exp2) ->
       eval (AppE (LambdaE(id,exp2), exp1)) env
   | IfE(exp0,exp1,exp2) ->
       (match (eval exp0 env) with
         | NumV n ->
             if n > 0
             then eval exp1 env
             else eval exp2 env
         | _ -> raise TestNotInteger)
   | ListE exp1 ->
       (match (eval exp1 env) with
         | NumV n -> ListV [n]
         | _ -> raise ListNotInteger)
   | FirstE exp1 ->
       (match (eval exp1 env) with
         | ListV (n :: _) -> NumV n
         | ListV [] -> NumV 0
         | _ -> raise FirstNotList)
   | PlusE(exp1,exp2) ->
       (match (eval exp1 env, eval exp2 env) with
        | (NumV n1, NumV n2) -> NumV (n1 + n2)
        (*one or both of the operands are a list*)
        | (NumV n1, ListV n2) -> ListV([n1]@n2)
        | (ListV n1, NumV n2) -> ListV(n1@[n2])
        | (ListV n1, ListV n2) -> ListV(n1 @ n2)
        | _ -> raise PlusWrongArgs (* CHANGE #5 *)
       )
   | MinusE(exp1,exp2) ->
       (match (eval exp1 env, eval exp2 env) with
        | (NumV n1, NumV n2) -> NumV (n1 - n2)
        | (ListV l1, NumV n1) ->
        (*1st operand is a list*)
          let rec remove =
            fun l n ->
              match l with
              | [] -> []
              | (_ :: l') -> 
                if n <= 0
                  then l
                else remove l' (n - 1)
              in ListV(remove l1 n1)
        | _ -> raise MinusWrongArgs  (* CHANGE #7 *)
       )
   | TimesE(exp1,exp2) ->
       (match (eval exp1 env, eval exp2 env) with
        | (NumV n1, NumV n2) -> NumV (n1 * n2)
        (*2nd operand is a list*)
        | (NumV n1, ListV l1) ->
            let rec copy = 
              fun n l -> 
              if n <= 0 
              then [] 
              else
                l @ (copy (n - 1) l) (*append when using copy*)
              in ListV(copy n1 l1)
        | _ -> raise TimesWrongArgs  (* CHANGE #6 *)
       )

let run x = 
  try
    (match (eval (parse x) initEnv) with
       | NumV n -> [n]
       | ListV l -> l
       | ClosureV _ -> raise OutputClosure
    ) with
   | InputEndsTooEarly -> 
       (print_string "input prematurely exhaused\n"; [0])
   | InputEndsTooLate ->
       (print_string "input continues after expression is parsed\n"; [0])
   | IdentifierExpectedBut s ->
       (print_string ("identifier expected but "^s^" seen\n"); [0])
   | NotDeclared s ->
       (print_string ("identifier "^s^" not bound to value\n"); [0])
   | ListNotInteger ->
       (print_string "attempt to make list of non-integers\n"; [0])
   | FirstNotList ->
       (print_string "argument to 'first' not list\n"; [0])
   | TestNotInteger ->
       (print_string "test expression not integer\n"; [0])
   | AppNotClosure ->
       (print_string "function part of application does not evaluate to closure\n"; [0])
   | PlusWrongArgs ->
       (print_string "'+' has closure as operand\n"; [0])
   | MinusWrongArgs ->
       (print_string "'-' has closure as operand, or list as 2nd operand\n"; [0])
   | TimesWrongArgs ->
       (print_string "'*' has closure as operand, or list as 1st operand\n"; [0])
   | OutputClosure ->
       (print_string "program returns closure\n"; [0])
