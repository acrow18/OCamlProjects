(* CONCRETE SYNTAX

P : Program
C : Command
E : Expression
L : LeftHandSide
D : Declaration
Ds: Declarations
N : Numeral
I : Identifier

P ::= Ds C

C ::=  skip 
   |  C1 ; C2  
   |  output E  
   |  input L
   |  if E { C1 } { C2 }  
   |  while E { C }  
   |  call I
   |  L = E  

E ::=  N  |  L  |  (E1 + E2)  |  (E1 * E2)  |  (E1 - E2)

L ::= I  | [L E]

Ds := <nothing>
   |  D ; Ds

D ::= var I
   |  array I [N1,..,Nn]
   |  alias I L
   |  proc  I { C }

N ::=  strings of digits

I ::=  strings of letters or digits or underscore, 
           starting with a letter, 
           not including keywords

*)

(* ABSTRACT SYNTAX *)

type expE =
 | NumE of int
 | GetE of leftL
 | PlusE of expE * expE
 | MinusE of expE * expE
 | TimesE of expE * expE
and leftL =
 | LocationL of string
 | ReferenceL of leftL * expE 

type commC =
 | SkipC
 | SeqC of commC * commC
 | OutputC of expE
 | InputC of leftL
 | IfC of expE * commC * commC
 | WhileC of expE * commC
 | CallC of string
 | MutateC of leftL * expE

type declD = 
 | DataD of string * int list
 | AliasD of string * leftL
 | ProcD of string * commC 

type progP = 
 | ProgP of declD list * commC
 | ErrorP of string   (* to report errors *)

(* SCANNER
    converts the input string into a list of "tokens" *)

type tokenT = 
 | SkipT
 | OutputT
 | InputT
 | IfT
 | WhileT
 | CallT
 | VarT
 | ArrayT
 | AliasT
 | ProcT
 | SemicT
 | CommaT
 | EqualT
 | PlusT
 | MinusT
 | TimesT
 | LparenT
 | RparenT
 | LsquareT
 | RsquareT
 | LcurlyT
 | RcurlyT
 | IdT of string
 | NumT of int

let print_token token = match token with
 | SkipT -> "skip"
 | OutputT -> "output"
 | InputT -> "input"
 | IfT -> "if"
 | WhileT -> "while"
 | CallT -> "call"
 | VarT -> "var"
 | ArrayT -> "array"
 | AliasT -> "alias"
 | ProcT -> "proc"
 | SemicT -> ";"
 | CommaT -> ","
 | EqualT -> "="
 | PlusT -> "+"
 | MinusT -> "-"
 | TimesT -> "*"
 | LparenT -> "("
 | RparenT -> ")"
 | LsquareT -> "["
 | RsquareT -> "]"
 | LcurlyT -> "{"
 | RcurlyT -> "}"
 | (IdT s) -> ("identifier "^s)
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
       if s = "skip" then SkipT
       else if s = "output" then OutputT
       else if s = "input" then InputT
       else if s = "if" then IfT
       else if s = "while" then WhileT
       else if s = "call" then CallT
       else if s = "var" then VarT
       else if s = "array" then ArrayT
       else if s = "alias" then AliasT
       else if s = "proc" then ProcT
       else IdT s
     in (token :: scan str')
   else match c with
     | ';' -> SemicT :: (scan str1)
     | ',' -> CommaT :: (scan str1)
     | '=' -> EqualT :: (scan str1)
     | '+' -> PlusT :: (scan str1)
     | '-' -> MinusT :: (scan str1)
     | '*' -> TimesT :: (scan str1)
     | '(' -> LparenT :: (scan str1)
     | ')' -> RparenT :: (scan str1)
     | '[' -> LsquareT :: (scan str1)
     | ']' -> RsquareT :: (scan str1)
     | '{' -> LcurlyT :: (scan str1)
     | '}' -> RcurlyT :: (scan str1)
     | _ -> scan str1

(* PARSER *)

exception SyntaxError of string

let expectToken : tokenT -> tokenT list -> tokenT list = 
  fun expected tokens -> 
   match tokens with
   |  [] -> raise (SyntaxError ((print_token expected)^" expected"))
   | token1 :: tokens' -> 
       if token1 = expected
       then tokens'
       else raise (SyntaxError  
              ((print_token expected)^" expected but "^
               (print_token token1)^" seen"))

let getIdT : tokenT list -> string * tokenT list =
  fun tokens -> 
   match tokens with
   | [] -> raise (SyntaxError "identifier expected")
   | (IdT s) :: tokens' -> (s, tokens')
   | (token :: _) -> 
       raise (SyntaxError 
          ("identifier expected but "^(print_token token)^" seen"))

let getNumT : tokenT list -> int * tokenT list =
  fun tokens -> 
   match tokens with
   | [] -> raise (SyntaxError "number expected")
   | ((NumT n) :: tokens') -> (n, tokens')
   | (token :: _) -> 
        raise (SyntaxError 
          ("number expected but "^(print_token token)^" seen"))

let rec parseNumList : tokenT list -> int list * tokenT list =
  fun tokens ->
   match getNumT tokens with
   | (n1, CommaT :: tokens2) ->
        (let (ns, tokens') = parseNumList tokens2 
         in (n1 :: ns, tokens'))
   | (n1, tokens1) -> ([n1], tokens1)

let rec parseExp : tokenT list -> expE * tokenT list =
   fun tokens ->
    match tokens with
    | [] -> raise (SyntaxError "expression expected")
    | NumT n :: tokens1 -> (NumE n, tokens1)
    | LparenT :: tokens1 -> 
       (match parseExp tokens1 with
        | (exp1, PlusT :: tokens2) ->
            let (exp2, tokens') = parseExp tokens2
            in (PlusE(exp1,exp2), expectToken RparenT tokens')
        | (exp1, MinusT :: tokens2) ->
            let (exp2, tokens') = parseExp tokens2
            in (MinusE(exp1,exp2), expectToken RparenT tokens')
        | (exp1, TimesT :: tokens2) ->
            let (exp2, tokens') = parseExp tokens2
            in (TimesE(exp1,exp2), expectToken RparenT tokens')
        | _ -> raise (SyntaxError "operator expected"))
    | _ -> let (left,tokens') = parseLeft tokens
            in (GetE left, tokens')
and parseLeft : tokenT list -> leftL * tokenT list =
  fun tokens ->
   match tokens with
   | (IdT s :: tokens1) -> (LocationL s, tokens1)
   | (LsquareT :: tokens1) ->
       let (left, tokens2) = parseLeft tokens1 in
       let (exp, tokens') = parseExp tokens2 in
      (ReferenceL (left, exp), expectToken RsquareT tokens')
   | _ -> raise (SyntaxError "left hand side expression expected")

let rec parse1Comm : tokenT list -> commC * tokenT list =
 (*  parse1Comm reads a command that is 'atomic',
       i.e., not formed by sequential composition    *)
  fun tokens -> 
   match tokens with
   | [] -> raise (SyntaxError "command expected")
   | SkipT :: tokens1 -> (SkipC, tokens1)
   | OutputT :: tokens1 ->
      let (exp, tokens') = parseExp tokens1
       in (OutputC exp, tokens')
   | InputT :: tokens1 ->
      let (left, tokens') = parseLeft tokens1
       in (InputC left, tokens')
   | IfT :: tokens1 -> 
       let (exp,tokens2) = parseExp(tokens1) in
       let (comm1,tokens3) = parseComm(expectToken LcurlyT tokens2) in
       let (comm2,tokens4) = parseComm(expectToken LcurlyT
                                    (expectToken RcurlyT tokens3))
      in (IfC(exp,comm1,comm2),
           expectToken RcurlyT tokens4)
   | WhileT :: tokens1 ->
       let (exp,tokens2) = parseExp tokens1 in
       let (comm,tokens3) = parseComm(expectToken LcurlyT tokens2)
     in (WhileC(exp,comm),
         expectToken RcurlyT tokens3)
   | CallT :: tokens1 ->
       let (id, tokens2) = getIdT tokens1
        in (CallC id, tokens2)
   | _ -> 
       let (left, tokens1) = parseLeft tokens in
       let (exp, tokens2) =
              parseExp (expectToken EqualT tokens1)
       in (MutateC(left,exp), tokens2)
and parseComm : tokenT list -> commC * tokenT list =
(*  parseComm reads a sequence of one or more atomic commands *)
  fun tokens ->
   let (comm1,tokens1) = parse1Comm tokens
    in match tokens1 with
       | SemicT :: tokens2 -> 
          let (comm2,tokens3) = parseComm tokens2
           in (SeqC(comm1,comm2), tokens3)
       | _ -> (comm1,tokens1)

let hasDecl : tokenT list -> bool = 
  fun tokens ->
   match tokens with
   | (VarT :: _) -> true
   | (ArrayT :: _) -> true
   | (AliasT :: _) -> true
   | (ProcT :: _) -> true
   | _ -> false       

let parseDecl : tokenT list -> declD * tokenT list =
  fun tokens ->
   match tokens with
   | token1 :: tokens1 -> 
      (let (id, tokens2) = getIdT tokens1 in
       match token1 with
       | VarT -> (DataD(id, []), tokens2)
       | ArrayT ->
           let (bounds,tokens3) = 
             parseNumList (expectToken LsquareT tokens2)
           in (DataD(id, bounds), expectToken RsquareT tokens3)
       | AliasT ->
          let (left,tokens') = parseLeft tokens2
           in (AliasD (id, left), tokens')
       | ProcT ->
          let (body,tokens3) = 
            parseComm (expectToken LcurlyT tokens2)
          in (ProcD (id,body), expectToken RcurlyT tokens3)
       | _ -> raise (SyntaxError "cannot happen!"))
   | [] -> raise (SyntaxError "cannot happen!")

let rec parseDecls : tokenT list -> declD list * tokenT list =
  fun tokens ->
   if hasDecl tokens
   then let (decl,tokens1) = parseDecl tokens in
        let (decls,tokens2) = 
                 parseDecls (expectToken SemicT tokens1)
         in (decl :: decls, tokens2)
   else ([], tokens)

let parseProg : tokenT list -> progP * tokenT list =
  fun tokens -> 
   let (decls, tokens1) = parseDecls tokens in
   let (comm, tokens2) = parseComm tokens1
    in (ProgP(decls,comm), tokens2)

let parse : string -> progP =
  fun input_string ->
   try ( 
    let tokens = scan input_string in
    let (prog,tokens1) = parseProg tokens
    in if tokens1 = []
       then prog
       else raise (SyntaxError "input contains symbols after the program")
     ) with SyntaxError s -> ErrorP s
