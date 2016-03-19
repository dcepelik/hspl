n(0).
n(s(N)) :- n(N).

sum(0, B, B).
sum(s(A), B, s(S)) :- sum(A, B, S).

prod(0, B, 0).
prod(s(A), B, P) :- prod(A, B, MP), sum(B, MP, P).

fact(0, s(0)).
fact(s(N), F) :- fact(N, MF), prod(s(N), MF, F).

lt(A, A).
lt(A, s(B)) :- lt(A, B).

pow(N, 0, s(0)).
pow(N, s(E), R) :- pow(N, E, NM), prod(N, NM, R).

pairlt(A, B, N) :- lt(A, N), lt(B, N).
triplelt(A, B, C, N) :- lt(A, N), lt(B, N), lt(C, N), lt(A, B), lt(B, C).

pyth(A, B, C, N) :- triplelt(A, B, C, N), prod(A, A, A2), prod(B, B, B2), prod(C, C, C2), sum(A2, B2, C2).
