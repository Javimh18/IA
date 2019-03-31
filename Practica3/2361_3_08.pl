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

palindromo[L] :- invierte(L, L)

% ejercicio 4

divide(L, 0, [], L)
divide([X|L], N, [X|L1], L2) :- 
    N > 0,
    M is N-1,
    divide(L, M, L1, L2).

% Forma 2, Generar y comprobar 

divide(L, N, R, S) :-
    append(R, S, L),
    length(R, N).