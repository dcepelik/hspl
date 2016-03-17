male(david).
male(andrew).

female(claire).

husband_wife(david,claire).

married(X,Y):-sinatra(frank).
married(X,Y):-husband_wife(X,Y).
married(X,Y):-husband_wife(Y,X).

n(0).
n(s(N)):-n(N).

sum(0,B,B).
sum(s(A),B,s(S)):-sum(A,B,S).
