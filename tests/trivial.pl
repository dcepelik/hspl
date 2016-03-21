male(david).
male(andrew).

female(claire).

human(X) :- male(X).
human(X) :- female(X).

husband_wife(david,claire).

married(X, Y) :- husband_wife(X, Y).
married(X, Y) :- husband_wife(Y, X).
