fst([Head|Tail], Head).
snd([First, Second|Tail], Second).

head(List, Head) :- fst(List, Head).
tail([Head|Tail], Tail).

reverse(List, Result) :- reverse(List, [], Result).

reverse([], Acc, Acc).
reverse([Head|Tail], Acc, Result) :- reverse(Tail, [Head|Acc], Result).

append(List1, List2, List12) :- reverse(List1, List1R), reverse(List1R, List2, List12).

member(Item, [Item|Tail]).
member(Item, [Head|Tail]) :- member(Item, Tail).

delete(Item, [], []).
delete(Item, [Item|Tail], Result) :- delete(Item, Tail, Result).
delete(Item, [Head|Tail], [Head|Result]) :- delete(Item, Tail, Result), ne(Head, Item).

select(Item, [Item|Tail], Tail).
select(Item, [Head|Tail], [Head|Result]) :- select(Item, Tail, Result), ne(Head, Item).

subtract([], List, List).
subtract([Head|Tail], List, SList) :- delete(Head, List, MList), subtract(Tail, MList, SList).

permutation([], []).
permutation(List, [X|Perm]) :- select(X, List, SList), permutation(SList, Perm).

prefix([], List).
prefix([First|Rest], [First|Tail]) :- prefix(Rest, Tail).

suffix(SuffixList, List) :- reverse(List, RList), reverse(SuffixList, RSuffixList), prefix(RSuffixList, RList).

sublist([], List).
sublist([Head|Tail], [Head|Rest]) :- sublist(Tail, Rest).
sublist([Head|Tail], [Item|Rest]) :- sublist([Head|Tail], Rest), ne(Head, Item).

list([]).
list([Head|Tail]).
