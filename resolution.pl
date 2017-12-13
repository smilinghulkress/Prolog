/*
Resolution: This resolution technique uses proof by contradiction
and is based on the fact that any sentence in propositional logic
can be transformed into an equivalent sentence in conjunctive 
normal form.
 
resolution(F) where F is the file name, it loads the input file F and performs
on it.
Example : ?- resolution('input.txt').
*/

%Main Function :takes input and performs resolution
resolution(F):-
	retractall(myClause(_,_)),
	retractall(myQuery(_,_)),
    open(F,read,Str), 
    read_predicates(Str,Clauses), 
    close(Str), 
    assertClauses(Clauses),
	extract.
	
read_predicates(Stream,L):- 
    read(Stream,X), 
	(X = end_of_file -> L = [];
		L = [X|T],
		read_predicates(Stream,T)).

%Takes a list and assert its elements
assertClauses([]).
assertClauses([H|T]):-
	assert(H),
	%writeln(H),
	assertClauses(T).
	 
%Find complement
comp(X, neg(X)):-atom(X).
comp(neg(X), X):-atom(X).


chainNegate(Z,M):-
	countNeg(Z,0,K,A),
	K1 is K+1,
	(isOdd(K1)
	->
	M = neg(A)
	;
		M = A
	).

	
countNeg(neg(X), I, K,A):-
	I1 is I+1,
	(\+atom(X)
	->
		countNeg(X,I1,K,A)
	;K = I1, A = X
	).

even(0).
even(X) :- X > 0, X1 is X - 1, odd(X1).
even(X) :- X < 0, X1 is X + 1, odd(X1).

odd(1).
odd(X) :- X > 0, X1 is X - 1, even(X1).
odd(X) :- X < 0, X1 is X + 1, even(X1).

isOdd(X):-
	odd(X),
	true.

%Perform DeMorgan's on Or input
negate([], _,[]).
negate([H|T], X,[[X,H1]|T1]):-
	comp(H, H1),
	X1 is X+1,
	negate(T, X1,T1).
	
%Perform DeMorgan's on and input
negate_and([],[]).
negate_and([H|T], [H1|T1]):-
	comp(H, H1),
	negate_and(T,T1).
	

%Add neg(Query) in clause
querytoClause(X, Y):-
	myQuery(X,Z),
	(compound(Z)
	-> 
		(isNeg(Z)
		->
			/*comp(Z,C),
			Y = [[X|C]]*/
			chainNegate(Z,C),
			Y = [[X|C]]
		;
			(isOr(Z)
			->
				extract_or(Z,L),
				flatten(L, L1),
				negate(L1,X,Y)
			;
				extract_and(Z,L1),
				flatten(L1,L2),
				negate_and(L2,Y1),
				append([X],[Y1],Y2),
				Y = [Y2]
			)
		)
	;Y = [[X|neg(Z)]]
	).

%Checks if input is a or term
isOr(or(X,Y)):-
	true.
isOr(X):-
	false.
	
%takes all the clauses and returns a list of them
listofclauses(_,L):-
	findall([X|Y], myClause(X,Y), L1),
	querytoClause(X,Z),
	append(L1,Z,L).

%Parse and to a list
extract_and_(and(X,Y),[H|T]):-
	(compound(X)
	->
		(isNeg(X)
		->
			H = X
		;
			extract_and(X,H)
		)
	;H = X
	),
	
	(compound(Y)
	->
		(isNeg(Y)
		->
			T = Y
		;extract_and(Y,T)
		)
	;T = Y).

extract_and(K,L):-
	extract_and_(K,L1),
	flatten(L1,L2),
	make_list(L2,L).
	
make_list([],[]).
make_list([H|T],[[H]|T1]):-
	make_list(T,T1).
	
%Parse Or to a list
extract_or(or(X,Y),[H|T]):-
	(compound(X)
	->
		(isNeg(X)
		->
			H = X
		;
			extract_or(X,H)
		)
	;H = X
	),
	
	(compound(Y)
	->
		(isNeg(Y)
		->
			T = Y
		;extract_or(Y,T)
		)
	;T = Y). 
	
extract_or(X, X).
	
%Checks if input is a Neg term
isNeg(neg(X)):-
	true.
isNeg(X):-
	false.	
	
%Performs extra_or on a list
extract_all([],[]).
extract_all([H|T], [H1|T1]):-
	%writeln(H),
	H =.. [I|N],
	findsecond(N,E),
	%writeln(E),
	extract_or(E,L1),
	flatten(L1, H1),
	extract_all(T,T1).

%Calls all the functions in sequence
extract:-
	listofclauses(_,L1),
	%writeln(L1),
	extract_all(L1,NI),
	%writeln(NI),
	listindexing(NI,L),
	%writeln(L),
	length(L,K),
	%write('L == '), writeln(L),
	%write('K == '), writeln(K),
	K1 is K+1,
	(resolute(L,K1,[])
	->writeln('')
	; writeln('resolution(fail).')
	).

%Gives index to each item of the list
listindexing([],_,[]).
listindexing(L,I):-
	listindexing(L,1,I).
listindexing([H|T],N,[H1|T1]):-
	append([N],[H],H1),
	N1 is N+1,
	listindexing(T,N1,T1).
	
%Performs Resolution
resolute(L,K,O):-
	member(X,L),
	member(Y,L),
	X = [H,T],
	Y = [H1,T1],
	member(A,T),
	member(neg(A),T1),
	select(A,T,X1),
	select(neg(A),T1,Y1),
	append(X1,Y1,Z),
	%writeln([H, H1, Z]),
	\+checktautology(Z),
	remove_duplicates(Z,L1),
	listtorule(L1, P),
	O1 = [resolution(H, H1, P,K)],
	append(O,O1,O2),
	(length(L1,0)
	-> 
		printlist(O2),
		writeln('resolution(success).')
	;
	
	%writeln(O1),
	select(X,L,L2),
	select(Y,L2,L3),
	append([[K,L1]],L3,R1),
	K1 is K+1,
	
	%write('O2:'), writeln(O2),
	resolute(R1,K1,O2)
	).
	

%Helper Functions
checktautology(L):-
	member(X,L),
	member(neg(X),L).

remove_duplicates([],[]).

remove_duplicates([H | T], List) :-    
     member(H, T),
     remove_duplicates( T, List).

remove_duplicates([H | T], [H|T1]) :- 
      \+member(H, T),
      remove_duplicates( T, T1).
	

length([],0).
length([H|T], L):-
	length(T,N),
	L is N+1.
	
select(X,[X|R],R).
select(X,[F|R],[F|S]):-
	select(X,R,S).
	
member(X,[X|R]).
member(X,[Y|R]) :- member(X,R).
	
append([],L2,L2).
append([],[],[]).
append([H|T1],T2,[H|T3]):-
	append(T1,T2,T3).
	
	
flatten(List, Flat) :-
        flatten(List, Flat, []).

flatten([], Res, Res) :- !.
flatten([Head|Tail], Res, Cont) :-
        !,
        flatten(Head, Res, Cont1),
        flatten(Tail, Cont1, Cont).
flatten(Term, [Term|Cont], Cont).
	 
findsecond([],false).
findsecond([K],false).
findsecond([A,B|C],B).

printlist([]).
printlist([X|List]) :-
    write(X),nl,
    printlist(List).

listtorule([],empty).
listtorule([X], X).

listtorule(List,Y) :-
            Term =.. [or|List],
            Y = Term.
	




