#!/usr/bin/env python3

WIN_LIMIT = 21

cache = {}
player1 = 0
player2 = 1

def get_winner_counts(state):
    if state in cache:
        return cache
    winner = get_winner(state)
    if winner == player1:
        cache[state] = (1, 0)
        return cache
    if winner == player2:
        cache[state] = (0, 1)
        return cache

    states = play_dirac_step(state)
    counts = (0, 0)
    for s in states:
        get_winner_counts(s)
        p1, p2 = cache[s]
        counts = (counts[0] + p1, counts[1] + p2)
    cache[state] = counts
    return cache


def get_winner(state):
    _, _, _, p1score, p2score = state
    if p1score >= WIN_LIMIT:
        return player1
    if p2score >= WIN_LIMIT:
        return player2
    return None


def play_dirac_step(state):
    turn, p1pos, p2pos, p1score, p2score = state
    if turn == player1:
        return [(player2, x, p2pos, p1score + x, p2score) for x in [add_position(p1pos, r) for r in rolls()]]
    else:
        return [(player1, p1pos, x, p1score, p2score + x) for x in [add_position(p2pos, r) for r in rolls()]]


def add_position(pos, roll):
    if pos + roll == 10:
        return 10
    return (pos + roll) % 10


def rolls():
    return [r1 + r2 + r3 for r1 in range(1, 4) for r2 in range(1, 4) for r3 in range(1, 4)]


if __name__ == "__main__":
    state = player1, 10, 4, 0, 0
    get_winner_counts(state)
    winners = cache[state]
    print(winners)
    print(max(winners))
