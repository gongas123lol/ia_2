# Example Board Legend:
# '#' : Wall
# ' ' : Floor
# '.' : Goal
# '$' : Box
# '*' : Box on Goal
# 'P' : Player
# '+' : Player on Goal
import random

from SA import simulated_annealing
from sokobanClass import SokobanState

def eval_function_sokoban(state, data):
    total_cost = 0
    boxes = []
    goals = []
    
    # Find boxes and goals
    for i in range(state.rows):
        for j in range(state.cols):
            if state.board[i][j] == '$':
                boxes.append((i, j))
            elif state.board[i][j] == '*':  # Box on goal
                return 0  # If any box is on goal, we're making progress
            elif state.board[i][j] == '.':
                goals.append((i, j))
    
    # Calculate box-to-goal distances
    for box in boxes:
        min_distance = float('inf')
        for goal in goals:
            distance = abs(box[0] - goal[0]) + abs(box[1] - goal[1])
            min_distance = min(min_distance, distance)
        total_cost += min_distance * 2  # Weight box-goal distance more heavily
        
        # Add player-to-box distance
        player_box_dist = abs(state.player_pos[0] - box[0]) + abs(state.player_pos[1] - box[1])
        total_cost += player_box_dist
    
    return total_cost

board = [
    '##########',
    '# P      #',
    '#   $ .  #',
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


def get_random_neighbor_sokoban(current_state, data, moves_list):
    old_state = current_state.clone()
    new_state = old_state.clone()

    # Get possible directions
    directions = [(0, 1, "RIGHT"), (0, -1, "LEFT"), (1, 0, "DOWN"), (-1, 0, "UP")]

    # Try directions in random order
    random.shuffle(directions)

    player_r, player_c = new_state.player_pos

    for dr, dc, direction in directions:
        new_r, new_c = player_r + dr, player_c + dc

        # Check if move is valid
        if new_state.is_valid_pos(new_r, new_c):
            if new_state.board[new_r][new_c] in [' ', '.']:  # Empty space or goal
                new_state.player_pos = (new_r, new_c)
                moves_list.append(f"MOVE {direction}")
                return new_state

            elif new_state.board[new_r][new_c] in ['$', '*']:  # Box
                push_r, push_c = new_r + dr, new_c + dc
                if (new_state.is_valid_pos(push_r, push_c) and
                        new_state.board[push_r][push_c] in [' ', '.']):
                    # Update box position
                    if new_state.board[new_r][new_c] == '$':
                        new_state.board[new_r][new_c] = ' '
                    else:  # '*'
                        new_state.board[new_r][new_c] = '.'

                    if (push_r, push_c) in new_state.goals:
                        new_state.board[push_r][push_c] = '*'
                    else:
                        new_state.board[push_r][push_c] = '$'

                    new_state.player_pos = (new_r, new_c)
                    moves_list.append(f"PUSH {direction}")
                    return new_state

    return current_state


def solve_sokoban_sa(initial_state):
    moves_list = []  # List to store moves

    results = simulated_annealing(
        Tmax=5,
        Tmin=0.05,
        R=0.001,
        k=100,
        data=initial_state,
        get_initial_solution=get_initial_sokoban_solution,
        get_random_neighbor=lambda state, data: get_random_neighbor_sokoban(state, data, moves_list),
        eval_func=eval_function_sokoban,
        is_optimum=is_sokoban_solved,
        sense='minimize'
    )

    # Add moves to results
    results['moves'] = moves_list
    return results


# Run the solver and display results
game = SokobanState(initial_board_layout, player_start_pos)
results = solve_sokoban_sa(game)

# Print final state and moves
print("\nFinal state:")
results['final_solution'].display()
print(f"\nFinal cost: {results['Cost']}")
print("\nMoves made:")
for i, move in enumerate(results['moves'], 1):
    print(f"{i}. {move}")
print(f"\nTotal moves: {len(results['moves'])}")