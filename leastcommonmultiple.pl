%LeastCommonMulitple(LCM)
gcd(0,Y,Y):-
	X1 is 8 mod 12,
	write(X1),
	Z = Y.
gcd(X,X,X).
gcd(_,1,1).
gcd(X,Y,Z):-
	(Y>X
	-> X1 is Y mod X,
	gcd(X1,X,Z)
	;X1 is X mod Y,
	gcd(X1,Y,Z)
	).
lcm(X,Y,Z):-
	X1 is X*Y,
	gcd(X,Y,K),
	Z is X1//K.
	



