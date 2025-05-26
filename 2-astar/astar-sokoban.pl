:- ensure_loaded('astar.pl').
:- dynamic visited/1.
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(system)).

type("wall").
type("empty").
type("box").
type("x").
type("player").

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
square("empty",  1, 2).
square("box",    1, 3).
square("empty",  1, 4).
square("empty",  1, 5).
square("x",      1, 6).
square("wall",   1, 7).

square("wall", 2, 0).
square("wall", 2, 1).
square("wall", 2, 2).
square("wall", 2, 3).
square("wall", 2, 4).
square("wall", 2, 5).

out_of_bounds(X,Y) :- \+ square(_,X,Y).

is_wall(X,Y) :- square("wall",X,Y).
is_wall(X,Y) :- out_of_bounds(X,Y).

is_target(X,Y) :- square("x",X,Y).
is_player(X,Y) :- square("player",X,Y).
is_box(X,Y) :- square("box",X,Y).

is_floor(X,Y) :- \+ is_wall(X,Y).

direction(up,    0,-1).
direction(down,  0, 1).
direction(left, -1, 0).
direction(right, 1, 0).

canonical(state(PX,PY,B0), state(PX,PY,B)) :- sort(B0,B).

wall_or_frame(X,Y) :- is_wall(X,Y).

dead_corner(X,Y) :-
    \+ is_target(X,Y),
    (
      (X1 is X-1, wall_or_frame(X1,Y), Y1 is Y-1, wall_or_frame(X, Y1));
      (X1 is X-1, wall_or_frame(X1,Y), Y1 is Y+1, wall_or_frame(X, Y1));
      (X1 is X+1, wall_or_frame(X1,Y), Y1 is Y-1, wall_or_frame(X, Y1));
      (X1 is X+1, wall_or_frame(X1,Y), Y1 is Y+1, wall_or_frame(X, Y1))
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

s(State,Next,1) :-
    valid_move(State,_,Next),
    ( visited(Next) -> fail ; assertz(visited(Next)) ).

box_distance((BX,BY),D) :-
    findall(D0,(is_target(TX,TY), D0 is abs(BX-TX)+abs(BY-TY)), Ds),
    min_list(Ds,D).

h(state(_,_,Boxes),H) :-
    findall(D,(member(B,Boxes), box_distance(B,D)), Ds),
    sum_list(Ds,H).

goal(state(_,_,Boxes)) :-
    findall((TX,TY), is_target(TX,TY), Targets),
    subset(Boxes,Targets).

subset([], _).
subset([E|Es],Set) :- member(E,Set), subset(Es,Set).

solve_sokoban(States,Moves) :-
    retractall(visited(_)),
    initial_state(Start),
    assertz(visited(Start)),
    bestfirst(Start,Rev),
    reverse(Rev,States),
    states_to_moves(States,Moves).

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

solve :-
    retractall(visited(_)),
    solve_sokoban(States,Moves),
    length(Moves,N),
    format('Solved in ~d moves: ~w~n~n',[N,Moves]),
    show_states(States).

animate(Delay) :-
    retractall(visited(_)),
    solve_sokoban(States,Moves),
    length(Moves,N),
    format('Solved in ~d moves.~n~n',[N]),
    maplist(show_with_delay(Delay),States).

show_with_delay(Delay,State) :-
    print_board(State),
    sleep(Delay).