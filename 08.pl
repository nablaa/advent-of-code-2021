% 0 = 6
% 1 = 2
% 2 = 5
% 3 = 5
% 4 = 4
% 5 = 5
% 6 = 6
% 7 = 3
% 8 = 7
% 9 = 6

zeros([a,b,c,d,e,f,g]).
ones([a,b]).
twos([a,b,c,d,e,f,g]).
threes([a,b,c,d,e,f,g]).
fours([a,b,e,f]).
fives([a,b,c,d,e,f,g]).
sixes([a,b,c,d,e,f,g]).
sevens([a,b,d]).
eights([a,b,c,d,e,f,g]).
nines([a,b,c,d,e,f,g]).
%%%%

segments(A, B, C, D, E, F, G) :-
    zeros(Zeros),
    ones(Ones),
    twos(Twos),
    threes(Threes),
    fours(Fours),
    fives(Fives),
    sixes(Sixes),
    sevens(Sevens),
    eights(Eights),
    nines(Nines),


    % 0
    member(A, Zeros),
    member(B, Zeros),
    member(C, Zeros),
    member(E, Zeros),
    member(F, Zeros),
    member(G, Zeros),
    % 1
    member(C, Ones),
    member(F, Ones),
    % 2
    member(A, Twos),
    member(C, Twos),
    member(D, Twos),
    member(E, Twos),
    member(G, Twos),
    % 3
    member(A, Threes),
    member(C, Threes),
    member(D, Threes),
    member(F, Threes),
    member(G, Threes),
    % 4
    member(B, Fours),
    member(C, Fours),
    member(D, Fours),
    member(F, Fours),
    % 5
    member(A, Fives),
    member(B, Fives),
    member(D, Fives),
    member(F, Fives),
    member(G, Fives),
    % 6
    member(A, Sixes),
    member(B, Sixes),
    member(D, Sixes),
    member(E, Sixes),
    member(F, Sixes),
    member(G, Sixes),
    % 7
    member(A, Sevens),
    member(C, Sevens),
    member(F, Sevens),
    % 8
    member(A, Eights),
    member(B, Eights),
    member(C, Eights),
    member(D, Eights),
    member(E, Eights),
    member(F, Eights),
    member(G, Eights),
    % 9
    member(A, Nines),
    member(B, Nines),
    member(C, Nines),
    member(D, Nines),
    member(F, Nines),
    member(G, Nines),

    member(A, [a, b, c, d, e, f, g]),
    member(B, [a, b, c, d, e, f, g]),
    member(C, [a, b, c, d, e, f, g]),
    member(D, [a, b, c, d, e, f, g]),
    member(E, [a, b, c, d, e, f, g]),
    member(F, [a, b, c, d, e, f, g]),
    member(G, [a, b, c, d, e, f, g]),

    nonmember(A, Ones),
    nonmember(B, Ones),
    nonmember(D, Ones),
    nonmember(E, Ones),
    nonmember(G, Ones),
    nonmember(A, Fours),
    nonmember(E, Fours),
    nonmember(G, Fours),
    nonmember(B, Sevens),
    nonmember(D, Sevens),
    nonmember(E, Sevens),
    nonmember(G, Sevens),
    A \== B,
    A \== C,
    A \== D,
    A \== E,
    A \== F,
    A \== G,
    B \== C,
    B \== D,
    B \== E,
    B \== F,
    B \== G,
    C \== D,
    C \== E,
    C \== F,
    C \== G,
    D \== E,
    D \== F,
    D \== G,
    E \== F,
    E \== G,
    F \== G.

nonmember(Arg,[Arg|_]) :-
        !,
        fail.
nonmember(Arg,[_|Tail]) :-
        !,
        nonmember(Arg,Tail).
nonmember(_,[]).
