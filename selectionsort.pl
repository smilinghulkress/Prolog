selectionsort([],[]).

selectionsort([First|Tail],[Smallest|Rest]):-
	findMinimum(Tail, First, Min), %findminimumfromtheinputlist
	Smallest =Min,
	write(Min),
	remove([First|Tail],Min,NewList),
	selectionsort(NewList, Rest).
	


findMinimum([], First, First).


findMinimum([H|T], First, Min):-
	H<First,
	findMinimum(T,H,Min).
	
findMinimum([_|T], First, Min):-
	findMinimum(T,First,Min).
	
remove([],_, []).
remove([H|T], H, T).
remove([H|T], Value, [H|T1]):- %Kept the H same to ensure the leading characters are unaffected.
	remove(T,Value,T1).

	
	



	
	