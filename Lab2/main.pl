% swipl -s main.pl -g main -t halt

% select(X, [X|Tail], Tail).
% select(X, [Head|Tail], [Head|Rest]) :-
%     select(X, Tail, Rest).

% select(X, [Y|Tail], Result) :-
%     ( X = Y ->
%         Result = Tail
%     ;
%         Result = [Y|Rest],
%         select(X, Tail, Rest)
%     ).

% select_all(_, [], []).
% select_all(X, [X|Tail], Result) :-
%     select_all(X, Tail, Result).
% select_all(X, [Y|Tail], [Y|Result]) :-
%     X \= Y,
%     select_all(X, Tail, Result).



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
intersection([X|Xs], Ys, [X|Zs]) :-
    member(X, Ys),
    intersection(Xs, Ys, Zs).
intersection([X|Xs], Ys, Zs) :-
    \+ member(X, Ys),
    intersection(Xs, Ys, Zs).

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
esta_ordenada([_]).
esta_ordenada([X,Y|Z]) :- X =< Y, esta_ordenada([Y|Z]). 

% 11. ord(L1,L2) - usa solo permutación y esta_ordenada
ord(L1,L2) :- permutation(L1,L2), esta_ordenada(L2).

% 12. diccionario(A,N)
lstConcat([X],X).
lstConcat([X,Y|Z],R) :- atom_concat(X, Y, RU), lstConcat([RU|Z],R).

lstReverse(L, R) :- reverse_aux(L, [], R).
reverse_aux([], Acc, Acc).
reverse_aux([H|T], Acc, R) :-
    reverse_aux(T, [H|Acc], R).

diccionario(A, N) :- findall(R, diccionario_aux(A, N, [], R), _).
diccionario_aux(_, 0, W, W) :-  lstReverse(W, K), lstConcat(K, Out), write(Out), write(' ').
diccionario_aux(A, N, Acc, R) :- 
    N > 0, member(M, A), N1 is N - 1, 
    diccionario_aux(A, N1, [M | Acc], R).

% 13. palindromos(L)
intEven(I) :- intEvenAux(I, 0).
intEvenAux(0, Odd) :- Odd == 0.
intEvenAux(I, 1) :- I > 0, N is I - 1, intEvenAux(N, 0).
intEvenAux(I, 0) :- I > 0, N is I - 1, intEvenAux(N, 1). 

lstLength([], 0).
lstLength([_|L], R) :- lstLength(L, K), R is K + 1.

lstEven(L) :- lstLength(L, R), intEven(R).
lstOdd(L) :- \+ lstEven(L).

lstRemoveEquals([],[]).
lstRemoveEquals([X|L],[X|Res]) :- \+ member(X, L), lstRemoveEquals(L, Res).
lstRemoveEquals([X|L],Res) :- member(X, L), lstRemoveEquals(L, Res).

is_palindromic(L) :- 
    lstEven(L), append(A, B, L), lstReverse(B, BR), A == BR.
is_palindromic(L) :- 
    lstOdd(L), append(A, [_|B], L), lstReverse(B, BR), A == BR.

palindromos(L) :- 
    findall(K, permutation(L, K), L1), lstRemoveEquals(L1, L2), 
    write_palindromes(L2).

write_palindromes([]).
write_palindromes([X|L]) :- is_palindromic(X), write(X), write_palindromes(L).
write_palindromes([_|L]) :- write_palindromes(L).

% 14. ['S', 'E', 'N', 'D'] +. ['M', 'O', 'R', 'E'] = ['M', 'O', 'N', 'E', Y].
% Encontrar S E N D M O R Y (digitos).
sendMoreMoney(_, _, _).

% 15.

% 16.
reverse_dom(f(A,B), f(B,A)).

p16([],[]).
p16(L, [X|P]) :- select(X,L,R), p16(R,P).
p16(L, [X|P]) :- reverse_dom(X,X1), select(X1,L,R), p16(R,P).

dom(L) :- p16(L,P), dom_ok(P), write(P), nl.
dom(_) :- write('no hay cedena'), nl.

dom_connect(f(_,Y), f(Y,_)).

dom_ok([_]).
dom_ok([X,Y|P]) :- dom_connect(X,Y), dom_ok([Y|P]). 

% 17. Npi de que es readclauses, no puedo depurar.
% p17 :- readclauses(F), sat([],F).
% p17 :- write('UNSAT'), nl.
% sat(I,[]) :- write('IT IS SATISFIABL. Model: '), write(I), nl, !.
% sat(I,F) :-
%     decision_lit(F,Lit), % Select unit clause if any; otherwise, an arbitrary one. 
%     simplif(Lit,F,F1), % Simplifies F. Warning: may fail and cause backtracking 
%     sat(...,...).

% 18.
group(X, Y, Z, I) :- 
    between(1,10,X), 
    between(1,10,Y), 
    between(1,10,Z), 
    between(1,10,I), 
    % Fijamos la proporción aquí.
    % X - NoCancerDoesntSmoke
    % Y - NoCancerSmokes
    % Z - CancerDoesntSmoke
    % I - CancerSmokes
    Z/(X + Z) < I/(Y + I),
    10 is X + Y + Z + I.

p18 :- 
    group(
        NoCancerDoesntSmoke1, 
        NoCancerSmokes1,
        CancerDoesntSmoke1,
        CancerSmokes1
    ),
    group(
        NoCancerDoesntSmoke2, 
        NoCancerSmokes2,
        CancerDoesntSmoke2,
        CancerSmokes2
    ),
    CancerNoSmokers = (CancerDoesntSmoke1+CancerDoesntSmoke2)/(CancerDoesntSmoke2+NoCancerDoesntSmoke2+CancerDoesntSmoke1+NoCancerDoesntSmoke1),
    CancerSmokers = (CancerSmokes1+CancerSmokes2)/(CancerSmokes2+NoCancerSmokes2+CancerSmokes1+NoCancerSmokes1),
    CancerNoSmokers > CancerSmokers, 
    write([NoCancerDoesntSmoke1, NoCancerSmokes1, CancerDoesntSmoke1, CancerSmokes1]),
    write([NoCancerDoesntSmoke2, NoCancerSmokes2, CancerDoesntSmoke2, CancerSmokes2]), nl.

% 19. % L - Monedas, C - Cambio (cantidad), M - Monedas a devolver.
lstGet([X|M], 0, X).
lstGet([X|L], I, R) :- I > 0, I1 is I - 1, lstGet(L, I1, R).

lstSum([X],X).
lstSum([Y|R], S) :- lstSum(R, S1), S is S1 + Y.

sum_coins([],[],0).
sum_coins([X|L1],[Y|L2], C):- sum_coins(L1,L2,C1), C is C1 + X*Y.

generate_with_coins(1,C,[C]) :- !.
generate_with_coins(Llen,C,[X|Res]) :- 
    between(0,C,X), 
    NewLlen is Llen - 1,
    NewC is C - X, NewC >= 0,
    generate_with_coins(NewLlen, NewC, Res).

maq(L,C,M) :- maqRec(L,C,M,1).

maqRec(L,C,M,S) :- 
    length(L,Llen),
    generate_with_coins(Llen,S,M),
    sum_coins(L,M,C), !.
maqRec(L,C,M,S) :- NewS is S+1, maqRec(L,C,M,NewS).

% 20. 
flatten([], []) :- !.
flatten([H|T], F) :-
    is_list(H),
    flatten(H, FH),
    flatten(T, FT),
    append(FH, FT, F).

flatten([H|T], [H|FT]) :-
    \+ is_list(H),
    flatten(T, FT).

% 21. log_b(N) = L; b^L = N
log(B, N, L) :- 
    between(0, N, L),
    B**L =< N, 
    B**(L+1) > N.

% 22. 
lstHas([X|_],X).
lstHas([_|S],X) :- lstHas(S,X).

lstRange(I, J, []) :- I > J, !.
lstRange(I, J, [I|T]) :- 
    I1 is I + 1,
    lstRange(I1, J, T).

lstCombination(0, _, []) :- !.
lstCombination(K, [X|T], [X|Comb]) :- 
    K > 0, 
    K1 is K - 1, 
    lstCombination(K1, T, Comb).
lstCombination(K, [_|T], Comb) :- lstCombination(K, T, Comb).

li_not_solution(S,R) :- 
    lstHas(S,A), lstHas(S,B),
    (lstHas(R,[B,A]) ; lstHas(R,[A,B])).
li_is_solution(S,R) :- \+ li_not_solution(S,R).

li_gen_students(N, M, Res) :- 
    lstRange(1, N, Lst),
    lstCombination(M, Lst, Res).

li(N,M,L,S) :- 
    li_gen_students(N,M,S),
    li_is_solution(S,L).

% 23.
% subsetWithRest(L,Subset,Rest)
lstRemove([],_,[]).
lstRemove([X|T], X, T).
lstRemove([Y|T], X, [Y|R]) :- X \= Y, lstRemove(T, X, R).

subsetWithRest([], [], []).
subsetWithRest([X|L], [X|Subset], Rest) :-
    subsetWithRest(L, Subset, Rest).
subsetWithRest([X|L], Subset, [X|Rest]) :-
    subsetWithRest(L, Subset, Rest).

% maxSubset(K,L,Sm)
maxSubset(K,L,Sm) :-
    subsetWithRest(L,Sm,Rest),
    lstSum(Sm,SubsetSum), SubsetSum =< K,
    (lstHas(Rest, X), SubsetSum + X> K).

p23 :- 
    (maxSubset(7,[1,2,3,2],R) -> write(R) ; write('No solution')), nl.

% p24
numVertices(10).
minCliqueSize(4).
vertices(Vs) :- numVertices(N), findall(I,between(1,N,I),Vs).
vertex(V) :- vertices(Vs), member(V,Vs).
edge(U,V) :- edge1(U,V).
edge(U,V) :- edge1(V,U).

edge1(9,8).
edge1(8,2).
edge1(7,4).
edge1(5,7).
edge1(4,2).
edge1(5,2).
edge1(2,7).
edge1(7,9).
edge1(2,9).
edge1(4,8).
edge1(4,9).
edge1(9,5).
edge1(4,5).

% Tenemos un clique si la lista se vacia
isCliqueDeep([], _).
isCliqueDeep(L, S) :- 
    select(X, L, RestL), !,
    all_connected(X, S),
    all_connected(X, RestL),
    isCliqueDeep(RestL, [X | S]).

all_connected(_, []).
all_connected(X, [Y | Ys]) :-
    edge(X, Y),
    all_connected(X, Ys).

isClique(Nodes) :- 
    isCliqueDeep(Nodes, []).

subset([], []).
subset([E|Tail], [E|NTail]) :- subset(Tail, NTail).
subset(Tail, [_|NTail]) :- subset(Tail, NTail).


p24 :- 
    vertices(Vs), 
    findall(S, 
    (
        subset(S, Vs), isClique(S), lstLength(S, SLen), minCliqueSize(MCSize), SLen >= MCSize, write(S), nl
    ), _).

nthRoot(N, K, R) :-
    between(1, K, R),
    R_NthPower is R ^ N,
    R_NthPower =< K,
    R1 is R + 1,
    R1_NthPower is R1 ^ N,
    R1_NthPower > K,
    !.

p25 :- nthRoot(2,16,R), write(R), nl.

lstEmpty([]).

lstEq([],[]).
lstEq([X|L1], [Y|L2]) :- X =:= Y, lstEq(L1,L2).

lstUnorderedEq(L1, L2) :-
    msort(L1, SortedL1),
    msort(L2, SortedL2),
    lstEq(SortedL1, SortedL2).

no_overlap(_, []).
no_overlap(X, [Y|Ys]) :-
    \+ lstUnorderedEq(X, Y),
    no_overlap(X, Ys).

ordered_subset_deep([], _, []).

ordered_subset_deep([X|SS], Acc, [X|Rest]) :-
    no_overlap(X, Acc),
    ordered_subset_deep(SS, [X|Acc], Rest).

ordered_subset_deep([X|SS], Acc, Rest) :-
    \+ no_overlap(X, Acc),
    ordered_subset_deep(SS, Acc, Rest).

ordered_subset(S, L) :-
    findall(X, subset(X, L), Subsets),
    ordered_subset_deep(Subsets, [], OSS),
    member(S, OSS).

allSSSS(L):- 
    ordered_subset(SS,L),
    sum(SS, Sum),
    nthRoot(2, Sum, Root),
    Sum is Root ^ 2 ,
    write(Sum-SS), nl, fail.

p26 :- allSSSS([6,3,4,5,6,9,8,5,2,3,4]).
    

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
    (esta_ordenada([3,45,67,83]) -> write("yes") ; write("no")), nl,
    (esta_ordenada([3,67,45]) -> write("yes") ; write("no")), nl,
    ord([4,5,3,3,2],P13), write(P13), nl,
    diccionario(['ga','chu','le'], 2), nl,
    palindromos(['a','a','c','c']), nl,
    % 14
    % 15
    write("17."), dom([f(3,2), f(3,4), f(4,2), f(2,2), f(2,1), f(1,6)]), nl,
    % 17
    p18, nl,
    maq([1,2,5,13,17,35,157], 361,P14), write(P14), nl,
    flatten([a,b,[c,[d],e,[]],f,[g,h]], P15), write(P15), nl,
    log(2,1020,LogRes), write(LogRes), nl,
    li(20,16,[[8,11],[8,15],[11,6],[4,9],[18,13],[7,9],[16,8],[18,10],[6,17],[8,20]],S), write(S), nl,
    p23, nl, !,
    write("24."), nl, p24, nl, !,
    write("25."), nl, p25, nl, !,
    write("26."), nl, p26, nl, !,
    halt.

    









