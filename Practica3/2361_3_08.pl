% ejercicio 1

duplica([], []).
duplica([X|R], [X, X|S]) :- duplica(R, S).

% ejercicio 2

concatena([], L, L). 
concatena([X|L1], L2, [X|L3]) :-
    concatena(L1, L2, L3).

invierte([], []).
invierte([X|L], R) :- 
	invierte(L, S),
	append(S, [X], R).

% ejercicio 3

palindromo[L] :- invierte(L, L).

% ejercicio 4

divide(L, 0, [], L).
divide([X|L], N, [X|L1], L2) :- 
    N > 0,
    M is N-1,
    divide(L, M, L1, L2).

% Forma 2, Generar y comprobar 

divide(L, N, R, S) :-
    append(R, S, L),
    length(R, N).

 % ejercicio 5

aplasta([], []).
aplasta([X|L], LA) :-
    aplasta(X, X1),
    aplasta(L, L1) ,
    concatena(X1, L1, LA).        
aplasta(X, [X]). 

 % ejercicio 6

primos(1, [_]).
primos(N, [N]):-
   	N < 3.
		
primos(N, L):-
    N > 1,
    primos_rec(N, 2, L).

% necesitoo un caso baseeeee
primos_rec(1, _, []).
primos_rec(N, F, [F|L]):-
    0 is N mod F,
    A is N / F,
	primos_rec(A, F, L).

primos_rec(N, F, L):-
    next_factor(N, F, NF), 
    primos_rec(N, NF, L).
    
next_factor(_, 2, 3).
next_factor(N, F, NF):-
	F<sqrt(N),
	NF is F+2.
next_factor(N, _, N):-
    N > 2.