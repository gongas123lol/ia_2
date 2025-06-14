# Example Board Legend:
# '#' : Wall
# ' ' : Floor
# '.' : Goal
# '$' : Box
# '*' : Box on Goal
# 'P' : Player
import random

from SA import simulated_annealing
from sokobanClass import SokobanState

def eval_function_sokoban(state, data):

    if state.deadlock():
        return float('inf') # A deadlock state is infinitely bad

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
        min_distance = float('inf')
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
    '#   $   .#',
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

# get initial sokoban solution
def get_initial_sokoban_solution(data): return data

# check is is solved -> cost = 0
def is_sokoban_solved(cost, data): return cost == 0

# returns a random neighbor needed for SA
def get_random_neighbor_sokoban(current_state, moves_list):

    new_state = current_state.clone()
    directions = [(0, 1, "RIGHT"), (0, -1, "LEFT"), (1, 0, "DOWN"), (-1, 0, "UP")]
    # get random play
    random.shuffle(directions)

    player_r = new_state.player_pos[0]
    player_c = new_state.player_pos[1]

    for dic in directions:
        #destiny row
        dr =dic[0]
        #destiny col
        dc = dic[1]
        direction_name = dic[2]

        new_r = player_r + dr
        new_c = player_c + dc



        # check if the player new position is valid and not a wall
        if new_state.is_valid_pos(new_r, new_c) and new_state.board[new_r][new_c] != '#':

            # check if moving to an empty space or a goal

            if new_state.board[new_r][new_c] in [' ', '.']:
                new_state.player_pos = (new_r, new_c)
                moves_list.append(f"MOVE {direction_name}")
                return new_state

            # check if its pushing a box
            # We only allow pushing a box, not a box on goal
            elif new_state.board[new_r][new_c] == '$':
                push_r = new_r + dr
                push_c = new_c + dc
                # check if the space behind the box is free (ATTEMPT PUSH)
                if new_state.is_valid_pos(push_r, push_c) and new_state.board[push_r][push_c] in [' ', '.']:
                    # the spot the box was on becomes empty
                    new_state.board[new_r][new_c] = ' '
                    # the spot the box is pushed to becomes a box or box on goal
                    if (push_r, push_c) in new_state.goals:
                        new_state.board[push_r][push_c] = '*' # Box on a goal
                    else:
                        new_state.board[push_r][push_c] = '$' # Box on the floor
                    # update player position
                    new_state.player_pos = (new_r, new_c)
                    moves_list.append(f"PUSH {direction_name}")
                    return new_state
                # ELSE, if PUSH fails, ATTEMPT PULL
                else:
                    # Check if player's original spot is free for the box to be pulled into
                    if new_state.board[player_r][player_c] in [' ', '.']:
                        # The box's current spot becomes the new player position
                        new_state.player_pos = (new_r, new_c)
                        # The box's spot becomes empty (or a goal if it was on one)
                        if (new_r, new_c) in new_state.goals:
                            new_state.board[new_r][new_c] = '.'
                        else:
                            new_state.board[new_r][new_c] = ' '
                        # The player's old spot is now occupied by the pulled box
                        if (player_r, player_c) in new_state.goals:
                            new_state.board[player_r][player_c] = '*'
                        else:
                            new_state.board[player_r][player_c] = '$'
                        moves_list.append(f"PULL {direction_name}")
                        return new_state

    # If no valid move was found after trying all directions, return the original state
    return current_state


def solve_sokoban_sa(initial_state):
    moves_list = []  # List to store moves

    results = simulated_annealing(
        Tmax=100,
        Tmin=0.0001,
        R=0.005,
        k=75,
        data=initial_state,
        get_initial_solution=get_initial_sokoban_solution,
        get_random_neighbor=lambda state, data: get_random_neighbor_sokoban(state, moves_list),
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
#for i, move in enumerate(results['moves'], 1):
 #   print(f"{i}. {move}")
print(f"\nTotal moves: {len(results['moves'])}")