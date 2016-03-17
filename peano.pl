n(0).
n(s(N)) :- n(N).

sum(0, B, B).
sum(s(A), B, s(S)) :- sum(A, B, S).

prod(0, B, 0).
prod(s(A), B, P) :- prod(A, B, MP), sum(B, MP, P).

fact(0, s(0)).
fact(s(N), F) :- fact(N, MF), prod(s(N), MF, F).
