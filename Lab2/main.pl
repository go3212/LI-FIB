% 1. prod(L,P)
prod([], 1).
prod([L1|L], P) :- prod(L, P1), P is L1*P1.

% 2. pescalar(L1,L2,P)
pescalar([],[],0).
pescalar([X|L1],[Y|L2],P) :- pescalar(L1, L2, PN), P is PN + X*Y.

% 3. Interseccion y union. 
% union(L1,L2,R)
union([], L, L).
union([X|L1], L2, R):- member(X, L2), union(L1, L2, R). 
union([X|L1], L2, [X|R]):- union(L1, L2, R).

% intersection(L1,L2,R), remove
remove(X, L, R) :- append(L1,[X|L2],L), append(L1,L2,R).

intersection([], _, []).
intersection([X|L1], L2, [X|R]) :- 
    member(X, L2), remove(X, L2, NL), intersection(L1, NL, R).
intersection([_|L1], L2, R) :- intersection(L1, L2, R).

% 4. Último elemento y inversa de lista (usar append)
% last(L,R)
last(L,R) :- append(_,[R],L).

% reverse(L,R)
reverse([], []).
reverse([X|L],R) :- append(NR,[X],R), reverse(L, NR).

% 5. fib(N,F) - F es el n-ésimo número de fibonacci.
fib(1,1).
fib(2,1).
fib(N,R) :- M1 is N-1, M2 is N-2, fib(M1, P), fib(M2, Q), R is P+Q.

% 6. dados(P,N,L)
dados(0,0,[]).
dados(P,N,[X|L]) :- 
    N > 0, P > 0, member(X, [1,2,3,4,5,6]), 
    PN is P - X, NN is N - 1,
    dados(PN,NN,L).

% 7. suma_demas(L)
sum([], 0).
sum([X|L], R) :- sum(L, NR), R is NR + X.

suma_demas(L) :- 
    append(Left, [X|Right], L), 
    append(Left, Right, NL),
    sum(NL, X).

% 8. suma_ants(L)
suma_ants(L) :- 
    append(Left, [X|_], L),
    sum(Left, X).

% 9. card(L)
count(_,[],0).
count(X, [X|L], R) :- count(X, L, NR), R is NR + 1.
count(X, [_|L], R) :- count(X, L, R).

removeAll(_,[],[]). 
removeAll(X,[X|L],R) :- removeAll(X, L, R).
removeAll(X,[Y|L],[Y|R]) :- removeAll(X, L, R).
    
card(L) :- card_aux(L, Result), write(Result).

card_aux([], []).
card_aux([X|L], Result) :-
    count(X, [X|L], CountX),
    removeAll(X, L, LNew),
    card_aux(LNew, R2),
    Result = [[X, CountX]|R2].

% 10. esta_ordenada(L) - ordenada de menor a mayor
esta_ordenada(L) :- (esta_ordenada_aux(L) -> write("yes") ; write("no")).

esta_ordenada_aux([_]).
esta_ordenada_aux([X,Y|Z]) :- X =< Y, esta_ordenada_aux([Y|Z]). 

main :-
    prod([1,2,3], P0), write(P0), nl,
    pescalar([1,2], [1,2], P1), write(P1), nl,
    union([1,2], [3,4], P2), write(P2), nl,
    intersection([1,2,3],[1,2,2,2],P3), write(P3), nl,
    last([1,2,3,4],P4), write(P4), nl,
    reverse([1,3,2,3,4],P5), write(P5), nl,
    fib(4,P6), write(P6), nl,
    dados(5,2,P7), write(P7), nl,
    (suma_demas([1,2,3]) -> write("True") ; write("False")), nl,
    (suma_ants([1,2,3]) -> write("True") ; write("False")), nl,
    card([1,2,1,5,1,3,3,7]), nl,
    esta_ordenada([3,45,67,83]), nl,
    esta_ordenada([3,67,45]), nl.



