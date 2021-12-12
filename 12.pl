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
	traverse(Start, End, [Start], [], Reversed),
	reverse(Reversed, Path).

traverse(X, Y, Visited, _, [Y|Visited]) :-
	adjacent(X, Y).

traverse(X, Y, Visited, Single, Path) :-
	adjacent(X, Z),
	Z \== Y,
	small(Z),
	not(member(Z, Visited)),
	traverse(Z, Y, [Z|Visited], Single, Path).

traverse(X, Y, Visited, Single, Path) :-
	adjacent(X, Z),
	Z \== Y,
	Z \== start,
	Z \== end,
	small(Z),
	member(Z, Visited),
	length(Single, L),
	L < 1,
	not(member(Z, Single)),
	traverse(Z, Y, [Z|Visited], [Z|Single], Path).

traverse(X, Y, Visited, Single, Path) :-
	adjacent(X, Z),
	Z \== Y,
	big(Z),
	traverse(Z, Y, [Z|Visited], Single, Path).

pathcount(Start, End, Count) :-
	aggregate_all(count, path(Start, End, _), Count).
