/*
  CS471 - Programming Languages
  Lab #3
  Author: Goodwill, Kyle (kgoodwi2)
  Date: 2-12-15

  1)
    a. Relation - A relationship between sets of information
       Function - A sub-classification of relations where there is a one-to-one mapping of input to output
    b. Every function is a relation.
    c. Every relation is not a function. Any parabolic relation is not a function because it can have multiple inputs mapped to an output. Ex: y^2 = -3x + 6

  4)
    a. Atoms - aBoy, mia
       Variables - Butch, X
       Complex Structures(Arity) - loves(2), boxer(1), and(2), big(1), kahuna(1) hide(2)

    b. Facts - indian(curry), indian(tandoori), mild(tandoori), italian(pizza)
       Rules - likes(sam, Food) :- indian(Food), mild(Food) ; likes(sam, Food) :- italian(Food)
       Clauses - indian(Food). , italian(Food). , indian(curry). , indian(tandoori). , mild(tandoori). , italian(pizza).
       Predicates - likes, indian, mild, italian

       Heads - likes(sam, Food)
       Goals - indian(Food), mild(Food), italian(Food)
*/


second(X,Y) :- X=triple(A,B,C), B=Y.

secondF((A,B,C),B).

/* list of facts in prolog, stored in an ascii file, 'family.pl'*/
/* WE can think of "mother(mary,ann)" as meaning - */
/*                  Mary is the mother of Ann */

mother(mary, ann).
mother(mary, joe).
mother(sue, marY).
mother(sue,mary).

father(mike, ann).
father(mike, joe).
father(tom,mary).

grandparent(sue, ann). 

male(joe).
male(mike).
male(tom).

female(mary).
female(ann).
female(sue).

parent(P,C) :- father(P,C).
parent(P,C) :- mother(P,C).

daughter(P,C) :- female(C), parent(P,C).

last([X],X).
last([_|Xs],Y):-last(Xs,Y).

isOrdered([]).
isOrdered([X]).
isOrdered([H,A|T]) :- H<A, isOrdered([A|T]).

pyth(X,Y,Z) :- Z is X*X + Y*Y.

triple(X, Sum) :- X=triple(A,B,C), Sum is A+B+C.

sumOfList([],0).
sumOfList([X],X).
sumOfList([One,Two|T],S) :- Value is One+Two, sumOfList([Value|T],S).
