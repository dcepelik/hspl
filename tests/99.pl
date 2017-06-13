n(0).
n(s(N)) :- n(N).

le(N, N) :- n(N).
le(A, s(B)) :- le(A, B).

subtract(N, 0, N).
subtract(N, s(M), R) :- subtract(N, M, s(R)).

select(Item, [Item|Tail], Tail).
select(Item, [Head|Tail], [Head|Result]) :- select(Item, Tail, Result), ne(Head, Item).

last([T], T).
last([H|T], L) :- last(T, L).

lastButOne([U, V], U).
lastButOne([H|T], L) :- lastButOne(T, L).

kth([H|T], s(0), H).
kth([H|T], s(N), R) :- n(N), kth(T, N, R).

length([], 0).
length([H|T], s(N)) :- n(N), length(T, N).

reverse(L, R) :- reverse(L, [], R).

append(List1, List2, List12) :- reverse(List1, List1R), reverse(List1R, List2, List12).

reverse([], A, A).
reverse([H|T], A, R) :- reverse(T, [H|A], R).

same(X, X).

palindrome([]).
palindrome(L) :- reverse(L, R), same(L, R).

flatten([], []).
flatten([[HI|TI]|T], R) :- flatten([HI|TI], FH), flatten(T, FT), append(FH, FT, R).
flatten([H|T], [H|FT]) :- flatten(T, FT), ne(H, [A|B]).

flattenAcc(L, F) :- flattenAcc(L, [], RF), reverse(RF, F).

flattenAcc([], Acc, Acc).
flattenAcc([[HI|TI]|T], Acc, R) :- flattenAcc([HI|TI], Acc, R1), flattenAcc(T, R1, R).
flattenAcc([H|T], Acc, R) :- flattenAcc(T, [H|Acc], R), ne(H, [A|B]).

uniq([], []).
uniq([A], [A]).
uniq([A, A|T], U) :- uniq([A|T], U).
uniq([A, B|T], [A|U]) :- ne(A, B), uniq([B|T], U).

uniqAcc(L, U) :- uniqAcc(L, [], RU), reverse(RU, U).

uniqAcc([], Acc, Acc).
uniqAcc([H|T], [], R) :- uniqAcc(T, [H], R).
uniqAcc([H|T], [H|Acc], R) :- uniqAcc(T, [H|Acc], R).
uniqAcc([H|T], [S|Acc], R) :- ne(H, S), uniqAcc(T, [H, S|Acc], R).

pack([], []).
pack([H|T], [P|R]) :- pack2([H|T], H, [], P, O), pack(O, R).

pack2([H|T], H, A, R, O) :- pack2(T, H, [H|A], R, O).
pack2([], I, A, A, []).
pack2([H|T], I, A, A, [H|T]) :- ne(H, I).

rle([], []).
rle(L, Q) :- pack(L, P), rle2(P, Q).

rle2([], []).
rle2([[HI|TI]|T], [[L, HI]|R]) :- length([HI|TI], L), rle2(T, R).

modifiedRle(L, Q) :- rle(L, P), filterRle(P, Q).

filterRle([], []).
filterRle([[s(s(N)), V]|T], [[s(s(N)), V]|R]) :- n(N), filterRle(T, R).
filterRle([[s(0), V]|T], [V|R]) :- filterRle(T, R).

copy(V, 0, []).
copy(V, s(N), [V|R]) :- n(N), copy(V, N, R).

decodeRle([], []).
decodeRle([[N, V]|T], [C|R]) :- copy(V, N, C), decodeRle(T, R).

dup([], []).
dup([H|T], [H, H|R]) :- dup(T, R).

dupn([], N, []).
dupn([H|T], N, [C|R]) :- copy(H, N, C), dupn(T, N, R).

dropAllNth(L, N, R) :- dropAllNth(L, N, N, R).

dropAllNth([], M, N, []).
dropAllNth([H|T], s(0), N, R) :- dropAllNth(T, N, N, R).
dropAllNth([H|T], s(M), N, [H|R]) :- ne(M, 0), n(M), dropAllNth(T, M, N, R).

split([], 0, [], []).
split([H|T], 0, [], [H|T]).
split([H|T], s(N), [H|F], S) :- split(T, N, F, S), n(N).

slice(L, A, B, R) :- subtract(B, A, BI), split(L, A, F, S), split(S, BI, R, S2).

rotateLeft(L, N, R) :- split(L, N, F, S), append(S, F, R).

insert(L, 0, V, [V|L]).
insert([H|T], s(M), V, [H|R]) :- n(M), insert(T, M, V, R).

range(A, B, R) :- le(A, B), range(A, B, A, R).

range(A, B, B, [B]).
range(A, B, V, [V|R]) :- ne(B, V), range(A, B, s(V), R).

comb(0, L, []).
comb(s(N), [H|T], [H|R]) :- n(N), comb(N, T, R).
comb(N, [H|T], R) :- n(N), ne(N, 0), ne(T, []), comb(N, T, R).

sgroups(s(0), L, [L]) :- ne(L, []).
sgroups(s(N), L, [F|R]) :- n(N), ne(N, 0), ne(L, []), split(L, s(K), F, S), sgroups(N, S, R).
