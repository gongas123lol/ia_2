:- dynamic square/3.

% ###
% #@#
% # #
% #$#
% # #
% # #
% #.#
% ###

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
square("wall", 2, 6).
square("wall", 2, 7).