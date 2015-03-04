/*
Kyle Goodwill
Lab4
2/19/15
*/

/*
Problem 1

Homoiconic - The property of some programming languages where the program structure is similar to its syntax.

Prolog is homoiconic because you can define different parts of the language using the language itself.

A fully reflective language is a language that allows for meta-programming and introspection.

Prolog is fully reflective because you can write a meta-programming loop that describes the process for solving a clause in the language.
*/


%Problem 4
listOfTerms([],_,[]).
listOfTerms([X|Y],Name,R) :-  V =.. [Name|X], listOfTerms(Y,Name,Results), R = [V|Results].

/*
Problem 5
A binary search tree is a node-based binary tree data structure where each node has a comparable key(an associated value) and satisfies the restriction that the key in any node is larger than the keys in that nodes left subtree and smaller than the nodes in the right subtree.
*/

isBST(node(V,empty,empty),V,V). %leaf node
isBST(node(V,L,empty),Min,Max) :- isBST(L,Lmin,Lmax), V > Lmax, Min = Lmin, Max = V.
isBST(node(V,empty,R),Min,Max) :- isBST(R,Rmin,Rmax), V < Rmax, Min = Lmax, Min = V.
isBST(node(V,L,R),Min,Max) :- isBST(L,Lmin,Lmax), V > Lmax, Min = Lmin, isBST(R,Rmin,Rmax), V < Rmin,  Max = Rmax.

insert(E,L,R) :- select(E,R,L).

applyList([]).
applyList([H|T]) :- H, applyList(T).

%Problem 10 - A unifies to B then B unifies to what, therefore A also unifies to what. Anything inside of a not won't unify even if the there are multiple not's

%Problem 11 - This will always return 0 because you are not recursing down to the base case. It just subtracts and doesn't come back
