% Input:
%%%

edge(start,a).
edge(start,b).
edge(a,c).
edge(a,b).
edge(b,d).
edge(a,end).
edge(b,end).

small(b).
small(c).
small(d).
small(end).
small(start).

big(a).
%%%

adjacent(X, Y) :- edge(X, Y).
adjacent(X, Y) :- edge(Y, X).

path(Start, End, Path) :-
	traverse(Start, End, [Start], Path).

traverse(X, Y, Visited, [Y|Visited]) :-
	adjacent(X, Y).

traverse(X, Y, Visited, Path) :-
	adjacent(X, Z),
	Z \== Y,
	small(Z),
	not(member(Z, Visited)),
	traverse(Z, Y, [Z|Visited], Path).

traverse(X, Y, Visited, Path) :-
	adjacent(X, Z),
	Z \== Y,
	big(Z),
	traverse(Z, Y, [Z|Visited], Path).

pathcount(Start, End, Count) :-
	aggregate_all(count, path(Start, End, _), Count).
