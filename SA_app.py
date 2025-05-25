# Example Board Legend:
# '#' : Wall
# ' ' : Floor
# '.' : Goal
# '$' : Box
# '*' : Box on Goal
# 'P' : Player
# '+' : Player on Goal
from sokobanClass import SokobanState

def eval_function_sokoban(state, data):
    distance = 0
    boxes = []
    goals = []
    for i in range(state.rows):
        for j in range(state.cols):
            if state.board[i][j] == '$':
                boxes.append((i, j))
            elif state.board[i][j] == '.':
                goals.append((i, j))

    for box in boxes:
        results = []
        for goal in goals:
            box_goal_distance = abs(box[0] - goal[0]) + abs(box[1] - goal[1])
            results += [box_goal_distance]

        a = results[0]
        idx = 0
        for i in range(1, len(results)):
            if results[i] < a:
                a = results[i]
                idx = i
        distance += a
        goals.remove(goals[idx]) #remove the one we used so it doesnt get caught up in the next

    return distance


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