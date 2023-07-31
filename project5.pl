/*

 === Parser and Interpreter for the language given in
           Project 5, CIS505/705, 
        Kansas State University, Fall 2022

This is a skeleton, with several places where code needs to be changed/inserted.
Each such place is marked with "CHANGE #k" where k is a number
    (perhaps with a suffix) that indicates the suggested priority.

Even with the skeleton as given, you can interpret certain simple queries:

?- query([5, =, 5, &&, 7, =, 7]).
true ;
false

?- query([3, =, 3, &&, 8, =, 9]).
false

#1a,#1b: how to solve queries involving inequality of two numbers

Now you should be able to get

?- query([27, <, 29, &&, 4, <=, 4, &&, 8, <=, 11]).
true ;
false

#2a,#2b,#2c: how to evaluate sums, differences, and products

Now you should be able to get

?- query([33, -, 4, *, [1, +, 5], =, 9]).
true ;
false.

#3: how to handle the "some" construct
  #3a: how to generate all values from a given interval
 
  #3b: how to solve an existential query;
          this should involve generating all possible instances
            of the quantified variable.
  #3c: how to evaluate variables (observe: there is no environment)

Now you should be able to get

?- generate_instances(5,X,7).
X = 5 ;
X = 6 ;
X = 7 ;
false.

? query([find(2,X,10), [X, *, X, *, X, -, 8, *, X, *, X], +, 15, *, X, =, 0]).
X = 3 ;
X = 5 ;
false.

#4: how to parse disjunction
      (hint: take inspiration from how to parse conjunction)

Now you should be able to get

?- query([find(1,X,20), [18, <, X, '||', X, <, 3]]).
X = 1 ;
X = 2 ;
X = 19 ;
X = 20 ;
false.

#5: how to handle universal quantifiers
      hint: by DeMorgan, forall x.P is the same as not(exists x.not P)

Now you should be able to get

?- query([each(1,X,20), find(0,Y,19), Y, +, 1, =, X]).
true ;
false.
?- query([find(0,X,10), each(0,Y,20), Y, *, X, =, Y]).
X = 1 ;
false.

as well as all the examples in the question text!

*/

/* query(Tokens)
   INPUT: Tokens is a list representing a condition with quantified variables
   OUTPUT: Any instantiation of the variables mentioned existentially
        (without a surrounding universal quantifier) 
      which satisfies the condition represented by Tokens
*/

query(Tokens) :- parse(Tokens, Tree), solve(Tree).

/* parse(Tks, C)
   INPUT: Tks is a list of tokens
   OUTPUT: if Tks forms a boolean expression
           then C is instantiated to a 
                 condition tree representing that boolean expression
*/

parse(Tks, C) :- parseBE(Tks, C, []).

/* parseBE(Tks, C, TksR)
   INPUT: Tks is a list of tokens
   OUTPUT: if a prefix of Tks forms a boolean expression
           then C is instantiated to a 
                 condition tree representing that boolean expression
           and TksR is the remaining tokens
*/

parseBE(Tks, C, TksR) :- parseBT(Tks, C, TksR).

/*CHANGE #4 HERE*/
/*Does not like when it hits the final false*/

parseBE(Tks, logical_or(C1,C2), TksR) :- 
   parseBE(Tks, C1, [Or | Tks1]),  already(Or, '||'),
   parseBT(Tks1, C2, TksR).

/* parseBT(Tks, C, TksR)
   INPUT: Tks is a list of tokens
   OUTPUT: if a prefix of Tks forms a boolean term
           then C is instantiated to a 
                 condition tree representing that boolean term
           and TksR is the remaining tokens
*/

parseBT(Tks, C, TksR) :- parseBF(Tks, C, TksR).

parseBT(Tks, logical_and(C1,C2), TksR) :- 
   parseBF(Tks, C1, [And | Tks1]),  already(And, &&),
   parseBT(Tks1, C2, TksR).


/* parseBF(Tks, C, TksR)
   INPUT: Tks is a list of tokens
   OUTPUT: if a prefix of Tks forms a boolean factor
           then C is instantiated to a 
                 condition tree representing that boolean factor
           and TksR is the remaining tokens
*/

parseBF(Tks, strictly_less(E1,E2), TksR) :- 
   parseE(Tks, E1, [Less | Tks1]), already(Less, <), 
   parseE(Tks1, E2, TksR).

parseBF(Tks, less_or_equal(E1,E2), TksR) :-
   parseE(Tks, E1, [LessEq | Tks1]), already(LessEq, <=), 
   parseE(Tks1, E2, TksR).

parseBF(Tks, equal(E1,E2), TksR) :-
   parseE(Tks, E1, [Equal | Tks1]),  already(Equal, =),
   parseE(Tks1, E2, TksR).

parseBF([Q | Tks], exists(L,V,U,C), TksR) :-
   already(Q, find(L,V,U)), parseBF(Tks, C, TksR).

parseBF([Q | Tks], forall(L,V,U,C), TksR) :-
   already(Q, each(L,V,U)), parseBF(Tks, C, TksR).

parseBF([Tks0 | TksR], C, TksR) :- 
   compound(Tks0), parseBE(Tks0, C, []).

/* parseE(Tks, T, TksR)
   INPUT: Tks is a list of tokens
   OUTPUT: if a prefix of Tks forms an expression
           then T is instantiated to an 
                 expression tree representing that expression
           and TksR is the remaining tokens
*/

parseE(Tks, E, TksR) :- parseT(Tks, E, TksR).

parseE(Tks, plus(E1,E2),  TksR) :- 
   parseT(Tks, E1, [Plus | Tks1]),  already(Plus, +), 
   parseE(Tks1, E2, TksR).

parseE(Tks, minus(E1,E2), TksR) :- 
   parseT(Tks, E1, [Minus | Tks1]), already(Minus, -), 
   parseE(Tks1, E2, TksR).


/* parseT(Tks, T, TksR)
   INPUT: Tks is a list of tokens
   OUTPUT: if a prefix of Tks forms a term
           then T is instantiated to an 
                 expression tree representing that term
           and TksR is the remaining tokens
*/

parseT(Tks, E, TksR) :- parseF(Tks, E, TksR).

parseT(Tks, times(E1,E2), TksR) :- 
   parseF(Tks, E1, [Times | Tks1]), already(Times, *), 
   parseT(Tks1, E2, TksR).


/* parseF(Tks, T, TksR)
   INPUT: Tks is a list of tokens
   OUTPUT: if a prefix of Tks forms a factor
           then T is instantiated to an 
                 expression tree representing that factor
           and TksR is the remaining tokens
*/

parseF([C | Tks], constant(C), Tks) :- number(C).

parseF([Tks0 | TksR], E, TksR) :- 
   compound(Tks0), parseE(Tks0, E, []).

parseF([X | Tks], variable(X), Tks) :- var(X).


/* already(X, C)
   INPUT: C is instantiated to a non-variable
   OUTPUT: is true if X is already instantiated to something 
      that matches that non-variable
*/

already(X, C) :- not(var(X)), X = C. 

/* evaluate(E, V)
   INPUT: E is a fully instantiated expression tree
   OUTPUT: V is instantiated to the value of evaluating E
*/

evaluate(constant(N), N).


/*CHANGE #3c HERE*/
evaluate(variable(N), M) :- N = M. 


evaluate(plus(E1,E2), V) :- evaluate(E1,V1), evaluate(E2,V2), V is V1 + V2.

/*CHANGE #2b HERE*/
evaluate(minus(E1,E2), V) :- evaluate(E1,V1), evaluate(E2,V2), V is V1 - V2.

/*CHANGE #2c HERE*/
evaluate(times(E1,E2), V) :- evaluate(E1,V1), evaluate(E2,V2), V is V1 * V2.



/* solve(C)
   INPUT: C is a condition tree without free variables
   OUTPUT: succeeds (perhaps several times) iff C evaluates to true,
         together with the corresponding instantiations of the 
         existentially quantified variables 
               (that are not surrounded by a universal quantifier)
*/


/*CHANGE #1a HERE*/
solve(strictly_less(E1, E2)) :- evaluate(E1, V1), evaluate(E2, V2), V1 < V2. 

/*CHANGE #1b HERE*/
solve(less_or_equal(E1, E2)) :- evaluate(E1, V1), evaluate(E2, V2), V1 =< V2.


solve(equal(E1, E2)) :- 
   evaluate(E1, V1), evaluate(E2, V2), V1 = V2.

solve(logical_and(C1,C2)) :- 
   solve(C1), solve(C2).

solve(logical_or(C1, _)) :- solve(C1).
solve(logical_or(_, C2)) :- solve(C2). 

/*CHANGE #3b HERE*/
solve(exists(L, V, U, C)) :- generate_instances(L,V,U), solve(C).


solve(forall(L, V, U, C)) :- not(counterexample(L, V, U, C)).

/*CHANGE #5 HERE*/
counterexample(L, V, U, C) :- generate_instances(L, V, U), not(solve(C)).  

generate_instances(L, L, U) :- L =< U.

/*CHANGE #3a HERE*/
generate_instances(L, V, U) :- L1 is L + 1, L1 =< U, generate_instances(L1,V,U). 