/*******************************************************************
        CS471 - Programming Languages
        Lab #5 due: 2/27/15
        Author: Goodwill, Kyle (kgoodwi2)
        Date: 2/26/15
  *****************************************************************/

/**** THIS IS A VERY SHORT LAB AND IS DUE FRIDAY @ END OF DAY ****/

/* 1:(0pts) (Do not turn in.)
   Given the 4 logically equivalent predicates try to predict the outcome of 
   ?- subList1(X,[a]),fail.
   ?- subList2(X,[a]),fail.
   ?- subList3(X,[a]),fail.
   ?- subList4(X,[a]),fail.
   Try to understand why some produce "ERROR: Out of global stack"

*/ 
subList1(S,L):-append(_,S,P),append(P,_,L).
subList2(S,L):-append(P,_,L),append(_,S,P).
subList3(S,L):-append(S,_,T),append(_,T,L).
subList4(S,L):-append(_,T,L),append(S,_,T).

/* Using writes to see the backtracking */

subList1w(S,L):-write('1-1'),append(_,S,P),write(' 1-2'),write(P),append(P,_,L).
subList2w(S,L):-write('2-1'),append(P,_,L),write(' 2-2'),write(P),append(_,S,P).
subList3w(S,L):-write('3-1'),append(S,_,T),write(' 3-2'),write(T),append(_,T,L).
subList4w(S,L):-write('4-1'),append(_,T,L),write(' 4-2'),write(T),append(S,_,T).



/*2:(10pts) In class on Monday we defined a predicate "simplify/3" that succeeds if the last arguement is 
      a list with items with the form Var:Value,  
      the first argument is a "var" atom in the list and 
      the second argument is the var's Value.  Requires only one clause.
      (hint::member, atom).
      ?- simplify(b,Value,[a:100,b:(-5)]).
      Value = -5
      ?- simplify(b,Value,[a : 1,b : 5, c : 10]).
      Value = 5 .
   
   Now extend predicate "simplify/3" to  evaluates an algebraic expression.
   The algebraic expression consists of variable with operators 'plus', 
   'minus' and 'times'. Here are two test queries:
          ?- simplify(plus(times(x,y),times(3 ,minus(x,y))),V,[x:4,y:2]).
          V = 14
          ?- simplify(times(2,plus(a,b)),Val,[a:1,b:5]).
          Val = 12
          ?- simplify(times(2,plus(a,b)),Val,[a:1,b:(-5)]).
          Val = -8 .
     Requires only a total 5 clauses including the clause from %2.  
    You may use "number" in one of your clauses. 
*/

simplify(Var,Value,_Vars):-number(Var), Value = Var. 
simplify(Var,Value,Vars):-atom(Var),member(Var:Value,Vars).  %% atom is an extra check
simplify(Var,Value,Vars):-Var = plus(A,B), simplify(A,Value1,Vars), simplify(B,Value2,Vars), Value is Value1 + Value2.
simplify(Var,Value,Vars):-Var = minus(A,B), simplify(A,Value1,Vars), simplify(B,Value2,Vars), Value is Value1 - Value2.
simplify(Var,Value,Vars):-Var = times(A,B), simplify(A,Value1,Vars), simplify(B,Value2,Vars), Value is Value1 * Value2.

/*3:(10 pts) Define a predicate append3DL  that concatenates three difference lists:
   ?- append3DL( [z,y|A] - A, [x,w | B] -B, [u,v | C] - C, What).
   A = [x, w, u, v|C],
   B = [u, v|C],
   What = [z, y, x, w, u, v|C]-C.

   ?- append3DL([1,2,3|X]-X,[a,b|Y]-Y,[x,y,z|Z]-Z,A - B).
   X = [a, b, x, y, z|B],
   Y = [x, y, z|B],
   Z = B,
   A = [1, 2, 3, a, b, x, y, z|B].

*/

append3DL(I-M,M-O,O-P,I-P). 


/* 4:(10pts) Below is a database of US coins. 
      Write a predicate value(Coin, Number, Amount) which
      succeeds if Amount is the total amount of value of the Number of Coins.

      ?- coin(dime,V)=Coin, value(Coin, 5, Amount).
      V = 10,
      Coin = coin(dime, 10),
      Amount = 50.

*/

coin(dollar, 100).
coin(half, 50).
coin(quarter, 25).
coin(dime,10).
coin(nickel,5).
coin(penny,1).

value(Coin, Number, Amount):-Coin = coin(Name, Value), coin(Name,Value), Amount is Number * Value.

