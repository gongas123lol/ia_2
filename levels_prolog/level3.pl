:- dynamic square/3.

% ########
% #@     #
% # $$   #
% #  #####
% #    ..#
% ########

% Row 0
square("wall", 0, 0).
square("wall", 1, 0).
square("wall", 2, 0).
square("wall", 3, 0).
square("wall", 4, 0).
square("wall", 5, 0).
square("wall", 6, 0).
square("wall", 7, 0).

% Row 1
square("wall",   0, 1).
square("player", 1, 1).
square("empty",  2, 1).
square("empty",  3, 1).
square("empty",  4, 1).
square("empty",  5, 1).
square("empty",  6, 1).
square("wall",   7, 1).

% Row 2
square("wall",  0, 2).
square("empty", 1, 2).
square("box",   2, 2).
square("box",   3, 2).
square("empty", 4, 2).
square("empty", 5, 2).
square("empty", 6, 2).
square("wall",  7, 2).

% Row 3
square("wall",  0, 3).
square("empty", 1, 3).
square("empty", 2, 3).
square("wall",  3, 3).
square("wall",  4, 3).
square("wall",  5, 3).
square("wall",  6, 3).
square("wall",  7, 3).

% Row 4
square("wall",  0, 4).
square("empty", 1, 4).
square("empty", 2, 4).
square("empty", 3, 4).
square("empty", 4, 4).
square("x",     5, 4).
square("x",     6, 4).
square("wall",  7, 4).

% Row 5
square("wall", 0, 5).
square("wall", 1, 5).
square("wall", 2, 5).
square("wall", 3, 5).
square("wall", 4, 5).
square("wall", 5, 5).
square("wall", 6, 5).
square("wall", 7, 5).