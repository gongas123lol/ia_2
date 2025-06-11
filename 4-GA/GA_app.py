from random import random


POSSIBLE_MOVES = [1 , 2, 3, 4]
# 1 is UP, 2 is DOWN, 3 is LEFT, 4 is RIGHT
CHROMOSOME_LENGTH = 100

def get_initial_sokoban_chromosome(data):
    """Creates a single random chromosome (a sequence of moves)."""
    return [random.choice(POSSIBLE_MOVES) for _ in range(CHROMOSOME_LENGTH)]


def eval_function_sokoban(state, data):
    if state.deadlock():
        return 100000000000000000000  # no deadlocks alowed

    total_cost = 0
    boxes = []
    goals = []

    # Find boxes and goals
    for i in range(state.rows):
        for j in range(state.cols):
            cell = state.board[i][j]
            if cell == '$':
                boxes.append((i, j))
            elif cell == '.':
                goals.append((i, j))

    # Calculate box to goal distances
    for box in boxes:
        min_distance = float(0)
        for goal in goals:
            distance = abs(box[0] - goal[0]) + abs(box[1] - goal[1])
            min_distance = min(min_distance, distance)
        total_cost += min_distance * 2

        # Add player to box distance
        player_box_dist = abs(state.player_pos[0] - box[0]) + abs(state.player_pos[1] - box[1])
        total_cost += player_box_dist

    return total_cost


board = [
    '##########',
    '# P      #',
    '#   $ .  #',
    '#        #',
    '#        #',
    '#   $ .  #',
    '#        #',
    '##########'
]

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


def get_initial_sokoban_solution(data):
    """Return the initial Sokoban state"""
    return data  # data will be the initial game state


def is_sokoban_solved(cost, data):
    """Check if all boxes are on goals"""
    return cost == 0


def move(state, move):
    """Applies a single move to a state. Returns True if move was valid, False otherwise."""
    player_r, player_c = state.player_pos
    dr, dc = {1: (-1, 0), 2: (1, 0), 3: (0, -1), 4: (0, 1)}[move]

    new_r, new_c = player_r + dr, player_c + dc

    if not state.is_valid_pos(new_r, new_c) or state.board[new_r][new_c] == '#':
        return False  # Invalid move: wall or out of bounds

    # Empty space or goal
    if state.board[new_r][new_c] in [' ', '.']:
        state.player_pos = (new_r, new_c)
        return True

    # Box
    elif state.board[new_r][new_c] in ['$', '*']:
        push_r, push_c = new_r + dr, new_c + dc
        if state.is_valid_pos(push_r, push_c) and state.board[push_r][push_c] in [' ', '.']:
            # Move box
            state.board[push_r][push_c] = '*' if (push_r, push_c) in state.goals else '$'
            # Old box space becomes empty or goal
            state.board[new_r][new_c] = '.' if (new_r, new_c) in state.goals else ' '
            # Move player
            state.player_pos = (new_r, new_c)
            return True
    return False  # Invalid push


    return (True,True)


def sokoban_fitness_function(chromosome, data):

    game_state = data['game_state']
    for moves in chromosome:
        moved = move(game_state, moves)
        isValid = moved[0]
        if isValid:
            game_state = moved[1]
        else:
            return 9999999999999999999999

    # if we get here all moves are valid
    return eval_function_sokoban(game_state, data)
