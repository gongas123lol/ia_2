:- ensure_loaded('depth_first_iterative_deepening.pl').
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(system)).
:- dynamic square/3.

type("wall").
type("empty").
type("box").
type("x").
type("player").

load_level(File) :-
    retractall(square(_,_,_)),
    consult(File).

out_of_bounds(X,Y) :- \+ square(_,X,Y).

is_wall(X,Y)    :- square("wall",X,Y).
is_target(X,Y)  :- square("x",   X,Y).
is_player(X,Y)  :- square("player",X,Y).
is_box(X,Y)     :- square("box", X,Y).
is_floor(X,Y)   :- \+ is_wall(X,Y).

direction(up,    0,-1).
direction(down,  0, 1).
direction(left, -1, 0).
direction(right, 1, 0).

canonical(state(PX,PY,B0), state(PX,PY,B)) :- sort(B0,B).

is_hard_wall(X,Y) :- square("wall",X,Y).

dead_corner(X,Y) :-
    \+ is_target(X,Y),
    (
      (X1 is X-1, is_hard_wall(X1,Y), Y1 is Y-1, is_hard_wall(X, Y1));
      (X1 is X+1, is_hard_wall(X1,Y), Y1 is Y-1, is_hard_wall(X, Y1));
      (X1 is X-1, is_hard_wall(X1,Y), Y1 is Y+1, is_hard_wall(X, Y1));
      (X1 is X+1, is_hard_wall(X1,Y), Y1 is Y+1, is_hard_wall(X, Y1))
    ).

deadlock(Boxes) :- member((X,Y),Boxes), dead_corner(X,Y), !.

initial_state(State) :-
    is_player(PX,PY),
    findall((BX,BY), is_box(BX,BY), Boxes0),
    canonical(state(PX,PY,Boxes0), State).

valid_move(state(PX,PY,Boxes0), Dir, state(NPX,NPY,BoxesC)) :-
    direction(Dir,DX,DY),
    NX is PX+DX,  NY is PY+DY,
    is_floor(NX,NY),
    (
      \+ member((NX,NY),Boxes0)
    -> canonical(state(NX,NY,Boxes0), state(NPX,NPY,BoxesC))
    ; member((NX,NY),Boxes0),
      BX is NX+DX, BY is NY+DY,
      is_floor(BX,BY),
      \+ member((BX,BY),Boxes0),
      delete(Boxes0,(NX,NY),Tmp),
      append(Tmp,[(BX,BY)],Boxes1),
      canonical(state(NX,NY,Boxes1), state(NPX,NPY,BoxesC)),
      \+ deadlock(BoxesC)
    ).

s(State, Next, _) :-
    valid_move(State, _Dir, Next).

goal(state(_,_,Boxes)) :-
    findall((TX,TY), is_target(TX,TY), Targets),
    msort(Boxes, SortedBoxes),
    msort(Targets, SortedTargets),
    SortedBoxes == SortedTargets.

subset([], _).
subset([E|Es],Set) :- member(E,Set), subset(Es,Set).

path_dfs(Node, Node, [Node], _Visited, _Limit) :-
    goal(Node).

% path_dfs/5: depth-limited DFS, now cuts on success
 path_dfs(Current, GoalFound, [Current|RestPath], Visited, Limit) :-
     Limit > 0,
     s(Current, Next, _),
     \+ member(Next, Visited),
     NewLimit is Limit - 1,
     path_dfs(Next, GoalFound, RestPath, [Next|Visited], NewLimit),
     !.

iterative_deepening(Depth, Path, Moves) :-
    initial_state(Start),
    path_dfs(Start, _Goal, Path, [Start], Depth),
    states_to_moves(Path, Moves).

% start timing and invoke loop with T0
solve(Level) :-
    statistics(runtime, [T0|_]),
    atom_concat('../levels_prolog/', Level, LevelPath),
    load_level(LevelPath),
    solve_loop(Level, 1, T0).

% depth‐limited loop carries T0 to compute elapsed time
solve_loop(Level, D, T0) :-
    format('Trying depth ~w (limit=~d)...~n', [Level, D]),
    ( iterative_deepening(D, Path, Moves) ->
        !,
        length(Moves, N),
        statistics(runtime, [T1|_]),
        Time is T1 - T0,
        format('ID-DFS solved ~w in ~d moves (depth = ~d, time = ~d ms)~nPath: ~w~n~n',
               [Level, N, D, Time, Moves]),
        show_states(Path)
    ;
        NextD is D + 1,
        solve_loop(Level, NextD, T0)
    ).

states_to_moves([_],[]) :- !.
states_to_moves([state(X1,Y1,_),state(X2,Y2,_)|R],[Dir|Dirs]) :-
    DX is X2-X1, DY is Y2-Y1,
    direction(Dir,DX,DY),
    states_to_moves([state(X2,Y2,_)|R],Dirs).

board_size(MaxX,MaxY) :-
    findall(X, square(_,X,_), Xs),
    findall(Y, square(_,_,Y), Ys),
    max_list(Xs,MaxX),
    max_list(Ys,MaxY).

cell_symbol(state(PX,PY,Boxes),X,Y,Char) :-
    ( is_wall(X,Y)                 -> Char = '#'
    ; member((X,Y),Boxes), is_target(X,Y) -> Char = '*'
    ; member((X,Y),Boxes)          -> Char = '$'
    ; PX =:= X, PY =:= Y,
      is_target(X,Y)               -> Char = '+'
    ; PX =:= X, PY =:= Y           -> Char = '@'
    ; is_target(X,Y)               -> Char = '.'
    ;                               Char = ' '
    ).

print_board(State) :-
    board_size(MaxX,MaxY),
    forall(between(0,MaxY,Y),
           ( forall(between(0,MaxX,X),
                    ( cell_symbol(State,X,Y,Ch),
                      write(Ch)
                    )),
             nl
           )),
    nl.

show_states([]).
show_states([S|Ss]) :-
    print_board(S),
    show_states(Ss).