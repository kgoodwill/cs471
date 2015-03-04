/*
  CS471 - Programming Languages
        Assignment #4 due: 2/24/15
        Author: Goodwill, Kyle (kgoodwi2)
        Date: 2/23/15
*/

/* Purpose:
   * to reenforce search order.
   * to reenforce recursive programming
   * to reenforce the use of the list data structure in Prolog

	1) For some of the problems below you need only generate a correct
      result from the query without requesting alternative results.
      You may use cuts but it is not required.
*/


/* 1:  : (From Learn Prolog NOW! exercise 3.3)
 * Binary trees are trees where all internal nodes have exactly two
 * children. The smallest binary trees consist of only one leaf node. We
 * will represent leaf nodes as leaf(Label). For instance, leaf(3) and
 * leaf(7) are leaf nodes, and therefore small binary trees. Given two
 * binary trees B1 and B2 we can combine them into one binary tree using
 * the predicate tree: tree(B1,B2). So, from the leaves leaf(1) and
 * leaf(2) we can build the binary tree tree(leaf(1), leaf(2)). And from
 * the binary trees tree(leaf(1), leaf(2)) and leaf(4) we can build the
 * binary tree tree(tree(leaf(1), leaf(2)), leaf(4)).

 * Now, define a predicate swap/2, which produces a mirror image of the
 * binary tree that is its first argument. For example:

    ?- swap(tree(tree(leaf(1), leaf(2)), leaf(4)),T).
    T = tree(leaf(4), tree(leaf(2), leaf(1))).
    yes

  2 clauses
**/

swap(leaf(A),leaf(A)).
swap(tree(A,B), T) :- swap(A,X), swap(B,Y), T=tree(Y,X).


/* 2: Define a predicate, addTree/2 that computes the sum of all the values
      in a binary tree. Assume the tree structure define in problem 1 and all
      the values in the leaves are integers. (2 clauses)
    e.g.
      ?- addTree(tree(leaf(4), tree(leaf(2), leaf(1))),S).
      S = 7.
      ?- addTree(tree(tree(leaf(-2),leaf(-4)), tree(leaf(2), leaf(1))),S).
      S = -3.
      ?- addTree(tree(tree(leaf(-2),leaf(4)), tree(leaf(2), leaf(-1))),S).
      S = 3.
      ?- addTree(tree(tree(leaf(0), leaf(2)), leaf(4)),S).
      S = 6.

  */

addTree(leaf(V),V).
addTree(tree(A,B),S) :- addTree(A,X), addTree(B,Y), S is X+Y.


/** 3 sumR(+N,?P).
   Given a number, N, S is a list of the sum of the numbers from N
   down to 1 such that first number in S is the sum of all the number from N to 1,
   the second number in P the sum of all the numbers from N-1 down to 1
    etc.
   For example:

     ?- sumR(6,S).
     S = [21, 15, 10, 6, 3, 1] .

   2 clauses
*/

sumR(1, [1]).
sumR(N, [A,B|T]):- X is A-1, sumR(X,[C|T]), B is A+C.


/* 4:  sumL(+N,?S).
   Is simular to sumR(+N,?S), except that sum totals
   accumulate left to right. e.g. The first value in S will be N,
   the second value will be N + N-1, etc.

     ?- sumL(6,S).
     S = [6, 11, 15, 18, 20, 21]


    It would be helpful to overload sumL/2 and include the following
    clause:

       sumL(N,Lst):-sumL(N,N,Lst).

    2 additional clauses.

*/

sumL(N,Lst):-sumL(N,N,Lst).
sumL(1,A,[A]).
sumL(N,A,[A|T]):- X is N-1, Y is A+X, sumL(X,Y,T).

/** :  "Send more money" is a well-known puzzle. Each of the letters
    D,E,M,N,O,R,S and Y represents a different digit. Moreover, when each
    letter is mapped to its corresponding digit the equation SEND + MORE =
    MONEY holds. Below is a very naive solution. Since there are 8 letters to be
    solved, it simply explore the 10*9*...*3 mappings of letters to
    digits.
    A little insight can simplify things. Clearly, SEND < 9999 and
    MORE < 9999. Thus MONEY < 19998 and hence M = 1.
    Now we have SEND + 1ORE = 1ONEY. Again SEND < 9999
     and now 1ORE < 1999 so 1ONEY < 11998. Since M is already bound to 1,
     O must be bound to 0. A little more thought shows that S must be
     bound to 8 or 9, and that N = E + 1. Using these insights to reduce
     the number of solutions that must be explored, write a Prolog
     predicate soln([D,E,M,N,O,R,S,Y]) that solves this puzzle by binding
     the correct digits to each of the variables in the list. (Modified
     from http://www.cs.wisc.edu/~fischer/)
    (1 clause with multiple subgoals.)

    
     =:= will evaluate boths sides of the operator and try to unify them.
     You may use assign_digits/2 in as a helper predicate in your
     solution OR you may use the ideas as part of your subgoals.


*/

solvSlow( [D,E,M,N,O,R,S,Y]) :-
	Lst = [S,E,N,D,M,O,R,Y],
	Digits = [0,1,2,3,4,5,6,7,8,9],
	assign_digits(Lst, Digits),
	M > 0,
	S > 0,
	1000*S + 100*E + 10*N + D +
	1000*M + 100*O + 10*R + E =:=
	10000*M + 1000*O + 100*N + 10*E + Y,
	write(Lst).


assign_digits([], _List).
assign_digits([D|Ds], List):-
        select(D, List, NewList),
        assign_digits(Ds, NewList).

solv([D,E,M,N,O,R,S,Y]) :- 
		Lst = [S,E,N,D,M,O,R,Y],
		Digits = [0,1,2,3,4,5,6,7,8,9],
		assign_digits(Lst,Digits),
		M = 1, 
		N is E + 1,
		O = 0,
		S > 7,
		S < 10,
		1000*S + 100*E + 10*N + D +
		1000*M + 100*O + 10*R + E =:=
		10000*M + 1000*O + 100*N + 10*E + Y,
		write(Lst).
		

 % --------------------------
   /*     ?- solv([D,E,M,N,O,R,S,Y]).
          [9, 5, 6, 7, 1, 0, 8, 2]
          D = 7,
          E = 5,
          M = 1,
          N = 6,
          O = 0,
          R = 8,
          S = 9,
          Y = 2
   */


/* 6:  Syntax-Directed Differentiation:  A motivating example illustrating the
         power of pattern matching in Prolog.
         Consider the following rules for symbolic differentiation
         (U, V are mathematical expressions, x is a variable):

        dx/dx = 1
        d(C)/dx = 0.
        d(Cx)/dx = C               (C is a constant)
        d(-U)/dx = -(dU/dx)
        d(U+V)/dx = dU/dx + dV/dx
        d(U-V)/dx = dU/dx - dV/dx
        d(U*V)/dx = U*(dV/dx) + V*(dU/dx)
        d(U^n)/dx = nU^(n-1)*(dU/dx)

        These rules can easily be translated into Prolog, for instance,
        the second rule can be defined as
                   d(C,x,0):-number(C).
          and the fifth rule can be defined as
                   d(U+ V ,x, DU+ DV)):-d(U,x,DU),d(V,x,DV).

         Write the remaining rules. Here is a test query:

           ?- d(3*(x +2*x*x),x,Result).
           Result = 3* (1+ (2*x*1+x*2))+ (x+2*x*x)*0 ;
           Result = 3* (1+ (2*x*1+x* (2*1+x*0)))+ (x+2*x*x)*0 ;
           false.


        Keep in mind, though, that terms such as U+V are still trees with the
        functor at the root, and that evaluation of such terms requires
        additional processing .  See next week's assignment.
        1 clause for each definition.
*/
d(x,x,1).
d(C,x,0):-number(C).
d(C*x,x,0):-number(C).
d(-U,x,-DU):-d(U,x,DU).
d(U+V,x,DU+DV) :- d(U,x,DU),d(V,x,DV).
d(U-V,x,DU-DV) :- d(U,x,DU),d(V,x,DV).
d(U*V,x,U*DV + V*DU) :- d(U,x,DU),d(V,x,DV).
d(U^N,x,N*U^A*DU):-A is N-1,d(U,x,DU). 

/* 7: fsa(Lst).
 Using the ideas presented on slides 21-25 found in
(http://dingo.sbs.arizona.edu/~sandiway/ling388n/lecture5.pdf)

Write a recognizer/generator for the regular expression:
      (a(bb)+a)|(b(aa)+b)

i.e.
   1 ?- fsa([a,b,b,b,b,a]).
   true
   2 ?- fsa([a,b,b,b,b]).
   false
   3 ?- fsa([b,a,a,a,a,a,a,b]).
   true
   4 ?- fsa([X,Y,Z]).
   false
  5 ?- fsa(X).
    X = [a, b, b, a] ;
    X = [a, b, b, b, b, a] ;
    X = [a, b, b, b, b, b, b, a] ;   
    etc
 The order of your causes is important.

*/

fsa([a,b,b|T]) :- fsa(T,L), L = a.
fsa([b,a,a|T]) :- fsa(T,L), L = b.
fsa([V],V).
fsa([H|T],L) :- fsa(T,L).

/* 8: In lab 4 you wrote a predicate isBST/3.  Using the same
      structure, define treeInsert(BST,Value,RBST) such that
      RBST is the result of inserting Value in BST.  RBST must
      maintain the BST properties.

      You may assume BST, is a binary search tree.   You may assume
      the Values are Integers.

      ?- tree1(T), treeInsert(T, 3, R).
      T = node(33, empty, empty),
      R = node(33, node(3, empty, empty), empty) .

      ?- tree3(T), treeInsert(T, 6, R).
      T = node(3, node(2, empty, empty), node(10, node(5, empty, empty), empty)),
      R = node(3, node(2, empty, empty), node(10, node(5, empty, node(6, empty, empty)), empty)) 

      ?- tree3(T), treeInsert(T, 1, R).
      T = node(3, node(2, empty, empty), node(10, node(5, empty, empty), empty)),
      R = node(3, node(2, node(1, empty, empty), empty), node(10, node(5, empty, empty), empty)) 


**/

% Some test structures.

tree1(node(33,empty,empty)).
tree2(node(33,node(22,empty,empty),empty)).
tree3(node(3,node(2,empty,empty),node(10,node(5,empty,empty),empty))).
tree4(node(10, node(5, node(1,empty,empty), node(8,empty,empty)), node(13,empty,empty))).
tree5(node(15,L,R)):-tree4(L),tree2(R).
notBtree(node(3,empty,empty,empty)).

treeInsert(empty, X, node(X,empty,empty)).
treeInsert(T,X,T):-T=node(X,_,_).
treeInsert(node(V,L,R),X,node(V,LN,R)) :- X < V, treeInsert(L,X,LN).
treeInsert(node(V,L,R),X,node(V,L,RN)) :- X > V, treeInsert(R,X,RN).






