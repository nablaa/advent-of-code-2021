median(List, Median) :-
    msort(List, SortedList),
    middle_element(SortedList, SortedList, Median).

middle_element([], [M|_], M).
middle_element([_], [M|_], M).
middle_element([_,_|Xs], [_|Ys], M) :-
    middle_element(Xs, Ys, M).

cost(X, X, 0).
cost(X, Y, C) :-
    X > Y,
    Y2 is Y+1,
    cost(X, Y2, C2),
    C is C2+1.
cost(X, Y, C) :-
    X < Y,
    Y2 is Y-1,
    cost(X, Y2, C2),
    C is C2+1.

allCosts([], _, 0).
allCosts([X|Xs], Y, C) :-
    cost(X, Y, C1),
    allCosts(Xs, Y, C2),
    C is C1 + C2.

cost2(X, X, 0).
cost2(X, Y, C) :-
    X > Y, !,
    D is X - Y,
    C is (D + 1) * D / 2.
cost2(X, Y, C) :-
    X < Y, !,
    D is Y - X,
    C is (D + 1) * D / 2.

allCosts2([], _, 0).
allCosts2([X|Xs], Y, C) :-
    cost2(X, Y, C1),
    allCosts2(Xs, Y, C2),
    C is C1 + C2.

minimum([X|Xs], Min) :-
    minimum(Xs, X, Min).
minimum([], Min, Min).
minimum([X|Xs], T, Min) :-
    X < T, !,
    minimum(Xs, X, Min).
minimum([_|Xs], T, Min) :-
    minimum(Xs, T, Min).

maximum([X|Xs], Max) :-
    maximum(Xs, X, Max).
maximum([], Max, Max).
maximum([X|Xs], T, Max) :-
    X > T, !,
    maximum(Xs, X, Max).
maximum([_|Xs], T, Max) :-
    maximum(Xs, T, Max).

listBetween(Min, Max, List) :-
    listBetween(Min, Max, [], List).
listBetween(X, X, List, List).
listBetween(Min, Max, T, List) :-
    X is Min + 1,
    listBetween(X, Max, [Min|T], List).

lowestCost(Nums, Cost) :-
    minimum(Nums, Min),
    maximum(Nums, Max),
    listBetween(Min, Max, Positions),
    positionsCost(Nums, Positions, AllCosts),
    minimum(AllCosts, Cost).

positionsCost(Nums, Positions, Cost) :-
    positionsCost(Nums, Positions, [], Cost).
positionsCost(_, [], T, T).
positionsCost(Nums, [P|Ps], T, Cost) :-
    allCosts2(Nums, P, C),
    positionsCost(Nums, Ps, [C|T], Cost).

solvePart1(Nums, Cost) :-
    median(Nums, Median),
    allCosts(Nums, Median, Cost).

solvePart2(Nums, Cost) :-
    lowestCost(Nums, Cost).
