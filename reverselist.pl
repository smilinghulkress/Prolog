%Reverse a list

reverse([],[]).
reverse([H|T],[H1|T1]):-
	findlast([H|T],X),
	H1 = X,
	remove([H|T],X,NewList),
	reverse(NewList,T1).

	
findlast([],0).
findlast([X],X).
findlast([_,E|T],X):-
	findlast([E|T],X).
	
remove([],_, []).
remove([H|T], H, T).
remove([H|T], Value, [H|T1]):- %Kept the H same to ensure the leading characters are unaffected.
	remove(T,Value,T1).
	
%TODO : Using Inbuilt append function.