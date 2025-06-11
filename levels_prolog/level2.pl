:- dynamic square/3.

% ########
% #@ $ . #
% # $$ . #
% #   .  #
% ########

% Outer frame walls
square("wall", 0, 0). square("wall", 1, 0). square("wall", 2, 0). square("wall", 3, 0).
square("wall", 4, 0). square("wall", 5, 0). square("wall", 6, 0). square("wall", 7, 0).

square("wall", 0, 1). square("wall", 7, 1).
square("wall", 0, 2). square("wall", 7, 2).
square("wall", 0, 3). square("wall", 7, 3).

square("wall", 0, 4). square("wall", 1, 4). square("wall", 2, 4). square("wall", 3, 4).
square("wall", 4, 4). square("wall", 5, 4). square("wall", 6, 4). square("wall", 7, 4).

% Player, boxes, targets
square("player", 1, 1).
square("box",    3, 1).
square("x",      5, 1).
square("box",    2, 2).
square("box",    3, 2).
square("x",      5, 2).
square("x",      4, 3).

% Empty squares
square("empty", 2, 1).
square("empty", 4, 1).
square("empty", 6, 1).
square("empty", 1, 2).
square("empty", 4, 2).
square("empty", 6, 2).
square("empty", 1, 3).
square("empty", 2, 3).
square("empty", 3, 3).
square("empty", 5, 3).
square("empty", 6, 3).