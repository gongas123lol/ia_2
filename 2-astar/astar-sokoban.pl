% Import the A* search algorithm
:- ensure_loaded('astar.pl').

% Simplified Sokoban Solver with A* Search
% Type definitions
type("wall").
type("empty").
type("box").
type("x").     % target position for boxes
type("player").

% Map representation (simple example)
square("wall", 0, 0).
square("wall", 0, 1).
square("wall", 0, 2).
square("wall", 0, 3).
square("wall", 0, 4).
square("wall", 0, 5).
square("wall", 0, 6).
square("wall", 0, 7).

square("wall", 1, 0).
square("player", 1, 1).
square("empty", 1, 2).
square("box", 1, 3).
square("empty", 1, 4).
square("empty", 1, 5).
square("x", 1, 6).
square("wall", 1, 7).

square("wall", 2, 0).
square("wall", 2, 1).
square("wall", 2, 2).
square("wall", 2, 3).
square("wall", 2, 4).
square("wall", 2, 5).
square("wall", 0, 6).
square("wall", 0, 7).

% Basic helper predicates
is_empty(X, Y) :- square("empty", X, Y).
is_empty(X, Y) :- square("x", X, Y).
is_wall(X, Y) :- square("wall", X, Y).
is_box(X, Y) :- square("box", X, Y).
is_target(X, Y) :- square("x", X, Y).
is_player(X, Y) :- square("player", X, Y).

% Define directions
direction(up, 0, -1).
direction(down, 0, 1).
direction(left, -1, 0).
direction(right, 1, 0).

% Check if all boxes are on targets (win condition)
all_boxes_on_targets :-
    findall((X, Y), is_box(X, Y), Boxes),
    findall((X, Y), is_target(X, Y), Targets),
    subset_check(Boxes, Targets).

subset_check([], _).
subset_check([(X, Y)|Rest], Targets) :-
    member((X, Y), Targets),
    subset_check(Rest, Targets).

% State representation: state(PlayerX, PlayerY, BoxPositions)
initial_state(State) :-
    findall((X, Y), is_player(X, Y), [(PX, PY)]),
    findall((X, Y), is_box(X, Y), Boxes),
    State = state(PX, PY, Boxes).

% Define valid moves for the player
valid_move(state(PX, PY, Boxes), Direction, NewState) :-
    direction(Direction, DX, DY),
    NX is PX + DX,
    NY is PY + DY,

    % Case 1: Moving to an empty space
    (   \+ member((NX, NY), Boxes),
        \+ is_wall(NX, NY),
        NewState = state(NX, NY, Boxes)
    ;
        % Case 2: Pushing a box
        member((NX, NY), Boxes),
        BX is NX + DX,
        BY is NY + DY,
        \+ member((BX, BY), Boxes),
        \+ is_wall(BX, BY),
        delete(Boxes, (NX, NY), TempBoxes),
        append(TempBoxes, [(BX, BY)], NewBoxes),
        NewState = state(NX, NY, NewBoxes)
    ).

% Manhattan distance for a single box to its nearest target
box_distance((BX, BY), Targets, Distance) :-
    findall(Dist, (member((TX, TY), Targets), Dist is abs(BX - TX) + abs(BY - TY)), Distances),
    min_list(Distances, Distance).

% Sum of Manhattan distances from all boxes to their nearest targets
% This is our admissible heuristic function
h(state(_, _, Boxes), H) :-
    findall((X, Y), is_target(X, Y), Targets),
    findall(Dist, (member(Box, Boxes), box_distance(Box, Targets, Dist)), Distances),
    sum_list(Distances, H).

% Define goal state - all boxes on targets
goal(State) :-
    State = state(_, _, Boxes),
    findall((X, Y), is_target(X, Y), Targets),
    subset_check(Boxes, Targets),
    length(Boxes, BoxCount),
    length(Targets, TargetCount),
    BoxCount =< TargetCount.

% Define successor states
s(State, NextState, Cost) :-
    direction(Dir, _, _),
    valid_move(State, Dir, NextState),
    Cost = 1.  % In Sokoban, each move costs 1

% Entry point for A* search
solve_sokoban(Solution) :-
    initial_state(InitialState),
    bestfirst(InitialState, RevSolution),
    reverse(RevSolution, Solution).

% A* search implementation
bestfirst(Start, Solution) :-
	expand([], l(Start, 0/0), 9999, _, yes, Solution).

% Case 1: goal leaf-node, construct a solution path
expand(P, l(N, _), _, _, yes, [N | P]) :-
	goal(N).

% Case 2: leaf-node, f-value less than Bound
expand(P, l(N, F/G), Bound, Tree1, Solved, Sol) :-
	F =< Bound,
	( bagof(M/C, (s(N, M, C), \+ member(M, P)), Succ),
	  !,
	  succlist(G, Succ, Ts),
	  bestf(Ts, F1),
	  expand(P, t(N, F1/G, Ts), Bound, Tree1, Solved, Sol)
	;
	  Solved = never
	).

% Case 3: non-leaf, f-value less than Bound
expand(P, t(N, F/G, [T | Ts]), Bound, Tree1, Solved, Sol) :-
	F =< Bound,
	bestf(Ts, BF),
	Bound1 is min(Bound, BF),
	expand([N | P], T, Bound1, T1, Solved1, Sol),
	continue(P, t(N, F/G, [T1 | Ts]), Bound, Tree1, Solved1, Solved, Sol).

% Case 4: non-leaf with empty subtrees
expand(_, t(_, _, []), _, _, never, _) :- !.

% Case 5: value greater than Bound
expand(_, Tree, Bound, Tree, no, _) :-
	f(Tree, F), F > Bound.

% Continue helper function
continue(_, _, _, _, yes, yes, Sol) :- !.

continue(P, t(N, F/G, [T1 | Ts]), Bound, Tree1, no, Solved, Sol) :-
	insert(T1, Ts, NTs),
	bestf(NTs, F1),
	expand(P, t(N, F1/G, NTs), Bound, Tree1, Solved, Sol).

continue(P, t(N, F/G, [_ | Ts]), Bound, Tree1, never, Solved, Sol) :-
	bestf(Ts, F1),
	expand(P, t(N, F1/G, Ts), Bound, Tree1, Solved, Sol).

% Make list of search leaves ordered by their f-values
succlist(_, [], []).

succlist(GO, [N/C | NCs], Ts) :-
	G is GO + C,
	h(N, H),
	F is G + H,
	succlist(GO, NCs, Ts1),
	insert(l(N, F/G), Ts1, Ts).

% Insert T into list of trees Ts preserving order with respect to f-values
insert(T, Ts, [T | Ts]) :-
	f(T, F), bestf(Ts, F1),
	F =< F1, !.

insert(T, [T1 | Ts], [T1 | Ts1]) :-
	insert(T, Ts, Ts1).

% Extract f-value
f(l(_, F/_), F).        % f-value of a leaf
f(t(_, F/_, _), F).     % f-value of a tree

bestf([T | _], F) :-    % Best f-value of a list of trees
	f(T, F).
bestf([], 9999).        % No trees: bad f-value

% Example usage
% ?- solve_sokoban(Solution), writeln(Solution).