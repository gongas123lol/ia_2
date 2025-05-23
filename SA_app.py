# Example Board Legend:
# '#' : Wall
# ' ' : Floor
# '.' : Goal
# '$' : Box
# '*' : Box on Goal
# '@' : Player
# '+' : Player on Goal
from sokobanClass import SokobanState

board = [['##########'],
         ['# P      #'],
         ['#   $ .  #'],
         ['##########']]

initial_board_layout = []
player_start_pos = None

for r_idx, row_str in enumerate(board):
    row_chars = []
    for c_idx, char in enumerate(row_str):
        if char == 'P':
            player_start_pos = (r_idx, c_idx)
            row_chars.append(' ')
        else:
            row_chars.append(char)
    initial_board_layout.append(row_chars)

if player_start_pos is None:
    raise ValueError("Player not found in initial board layout.")

game = SokobanState(initial_board_layout, player_start_pos)