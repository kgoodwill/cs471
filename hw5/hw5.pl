/*CS471 - Programming Languages
  Assignment #5 due: 3/3/2015
  Author: Goodwill, Kyle (kgoodwi2)
  Date: 3/3/2015
*/

eTree1(exp('+',
			expTree(lit, 5),
			expTree('*',
					expTree(lit, 3), 
					expTree(lit, 2)
			)
	)
).

eTree2(expTree('*',
			expTree('-',
					expTree(lit, -3),
					expTree(lit, 2)
			),
			expTree('+',
					expTree(lit, 3),
					expTree('-',
							expTree(lit, 2)
					)
			)
	)
).

eTree3(expTree('*',
				expTree('min',
						expTree(lit, -3),
						expTree(lit, 2)
				),
				expTree('+',
						expTree(lit, 3),
						expTree('-',
								expTree(lit, 2)
						)
				)
		)
).

eTree4(expTree(float,
				expTree('sin',
						expTree('/',
								expTree(lit, pi),
								expTree(lit, 2)
						)
				)
		)
).

%expTree(Op, Lt, Rt).
%expTree(lit, Value).
%expTree(Op,T).

%Question 1 - Looks like pattern matching, with evaluation at the end
eval(expTree(lit, V), V).
eval(expTree(Op,T), V) :- eval(T,A), Func =.. [Op,A], V is Func.
eval(expTree(Op,L,R), V) :- eval(L,V1), eval(R,V2), Func =.. [Op,V1,V2], V is Func.


%Question 2
d(x,x,1).
d(C,x,0):-number(C).
d(C*x,x,C):-number(C).
d(-U,X,-DU):-d(U,X,DU).
d(U+V,x,RU+RV):-d(U,x,RU), d(V,x,RV).
d(U-V,x,RU-RV):-d(U,x,RU), d(V,x,RV).
d(U*V,x,U*DV+V*DU):-d(U,x,DU),d(V,x,DV).
d(U^N,x,N*U^N1*DU):-integer(N),N1 is N-1, d(U,x,DU).

%Define predicate 'evaluate/3'
evaluate(Result, Value, VarValue):-VarValue = A:Value.

%Question 3
concatALL([],[]).
concatALL(X,C):-not(is_list(X)), C=[X].
concatALL([H|T],C):-concatALL(H,A),concatALL(T,B),append(A,B,C).

