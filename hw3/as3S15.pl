/*
CS471 - Programming Languages
Assignment #3 due: 2/17/15
Author: Goodwill, Kyle (kgoodwi2)
Date: 2/16/15
*/

numeral(0).
numeral(succ(X)) :- numeral(X).

/*Problem 1*/
convertToDecimal(succ(0), 1).
convertToDecimal(X,Y) :- X = succ(N), convertToDecimal(N,Z), Y is Z+1.

/*Problem 2*/
minus(0,0,Z) :- Z = 0.
minus(0,succ(Y),succ(Z)) :- minus(0,Y,Z).
minus(succ(X),0,succ(Z)) :- minus(X,0,Z).
minus(succ(X),succ(Y),Z) :- minus(X,Y,Z).

/*Problem 3*/
add(0,0,Z) :- Z = 0.
add(0,succ(Y),succ(Z)) :- add(0,Y,Z).
add(succ(X),0,succ(Z)) :- add(X,0,Z).
add(succ(X),succ(Y),succ(succ(Z))) :- add(X,Y,Z).

/*Problem 4*/
equLen([],[]).
equLen([H|T1],[H|T2]) :- equLen(T1,T2).

/*Problem 5*/
origin(point(0,0)).

/*Problem 6*/
collinear(point(X1,Y1),point(X2,Y2),point(X3,Y3)) :- A is X1-X2, B is Y2-Y3, C is Y1-Y2, D is X2-X3, E is A*B, F is C*D, E=F.

/*Problem 7 - NOT WORKING*/
prefix([],_).
prefix([H|T1],[H|T2]) :- prefix(T1,T2).

/*Problem 8 - NOT WORKING*/
hasSubseq(_,[]).
hasSubseq([H|T1], [H|T2]) :- hasSubseq(T1,T2).
hasSubseq([H|T], Y) :- hasSubseq(T,Y).

tree1(Tree1):- Tree1 = node(4, node(a, empty, node(b, empty, empty)), empty).
tree2(Tree2):- Tree2 = node(5, empty, empty).
tree3(Tree3):- Tree3 = node(a, node(b, node(c,empty,empty), node(d,empty,empty)), empty).
tree4(NotTree):- NotTree= node(a, empty, node(x ,node(b, node(c,empty,empty), node(d,empty,empty)))).
tree5(NotTree):-NotTree= node(a, empty, node(x, node(b, node(c, empty, empty), node(d, empty, empty))), empty).
tree6(Tree6):- Tree6 = node(a, node(b, empty,empty), node(c, node(d,empty,empty),node(e,empty,empty))).

/*Problem 9*/
isTree(empty).
isTree(node(_,L,R)) :- isTree(L), isTree(R).

/*Problem 10*/
gcd(A,0,GCD) :- GCD = A.
gcd(A,B,GCD) :- C is mod(A,B), gcd(B,C,GCD).

/*Problem 11*/
ack(M,N,A) :- M=0, A is 2*N.
ack(M,N,A) :- M>0, N=0, A=0.
ack(M,N,A) :- M>0, N=1, A=2.
ack(M,N,A) :- X is M-1, Y is N-1, ack(M,Y,Z), ack(X,Z,A).
