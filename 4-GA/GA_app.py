import random
import numpy as np
from GA import GA
from sokobanClass import SokobanState

POSSIBLE_MOVES = [1, 2, 3, 4]  # 1:UP, 2:DOWN, 3:LEFT, 4:RIGHT
CHROMOSOME_LENGTH = 120


# --- Chromosome & Fitness ---

# genereate a random chromosome
def get_initial_sokoban_chromosome(data):
    return [random.choice(POSSIBLE_MOVES) for _ in range(CHROMOSOME_LENGTH)]


# applies a move to a certain game, 1,2,3,4 being possible moves
def move(state, move_id):

    player_r, player_c = state.player_pos

    dr = 0
    dc = 0
    match move_id:
        case 1:
            dr = -1
        case 2:
            dr = 1
        case 3:
            dc = -1
        case 4:
            dc = 1

    # new position after moving
    new_r = player_r + dr
    new_c = player_c + dc

    if not state.is_valid_pos(new_r, new_c) or state.board[new_r][new_c] == '#':
        return False

    # check if its empty or target meaning it can move
    if state.board[new_r][new_c] in [' ', '.']:
        state.player_pos = (new_r, new_c)
        return True

    # if box or box on target, it pushes
    elif state.board[new_r][new_c] in ['$', '*']:
        push_r, push_c = new_r + dr, new_c + dc
        if state.is_valid_pos(push_r, push_c) and state.board[push_r][push_c] in [' ', '.']:
            state.board[push_r][push_c] = '*' if (push_r, push_c) in state.goals else '$'
            state.board[new_r][new_c] = '.' if (new_r, new_c) in state.goals else ' '
            state.player_pos = (new_r, new_c)
            return True
    return False


def eval_cost_function(state, data):
    # huge cost if is deadlock, we DONT want deadlocks
    if state.deadlock():
        return float('inf')

    total_cost = 0
    boxes = []
    open_goals = [g for g in state.goals if state.board[g[0]][g[1]] == '.']

    for i in range(state.rows):
        for j in range(state.cols):
            if state.board[i][j] == '$':
                boxes.append((i, j))

    if not boxes: return 0

    for box in boxes:
        min_distance = float('inf')
        if not open_goals:
            min_distance = 100  # Penalize if no open goals are left for this box
        else:
            for goal in open_goals:
                distance = abs(box[0] - goal[0]) + abs(box[1] - goal[1])
                min_distance = min(min_distance, distance)
        total_cost += min_distance * 10  # Weight box-to-goal distance heavily

    player_to_box_distances = [abs(state.player_pos[0] - b[0]) + abs(state.player_pos[1] - b[1]) for b in boxes]
    total_cost += min(player_to_box_distances)

    return total_cost


# simulates a move sequence and returns a fitness score (higher is better) (like 1/cost)
def sokoban_fitness_function(chromosome, data):

    initial_state = data['initial_state']
    sim_state = initial_state.clone()

    for move_action in chromosome:
        if not move(sim_state, move_action):
            break  # Stop simulation if an invalid move is made

    cost = eval_cost_function(sim_state, data)
    fitness = 1.0 / (1.0 + cost)
    return fitness


def is_sokoban_solved_ga(best_fitness, data):
    """Checks if the best fitness score corresponds to a solved puzzle."""
    return abs(best_fitness - 1.0) < 1e-9


# --- Genetic Operators (Selection, Crossover, Mutation) ---

def selection_tournament(population, pop_fitness, k=3):
    """Selects parents using tournament selection."""
    new_population = []
    pop_size = len(population)
    for _ in range(pop_size):
        participants_indices = np.random.choice(range(pop_size), k, replace=False)
        participants_fitness = [pop_fitness[i] for i in participants_indices]

        winner_local_idx = np.argmax(participants_fitness)
        winner_global_idx = participants_indices[winner_local_idx]
        new_population.append(population[winner_global_idx])
    return new_population


def crossover_single_point(data, population, cross_prob):
    """Performs single-point crossover on pairs of parents."""
    new_population = []
    for i in range(0, len(population), 2):
        parent1 = population[i]
        if i + 1 < len(population):
            parent2 = population[i + 1]
        else:
            new_population.append(parent1)  # Handle odd population size
            continue

        if random.random() < cross_prob:
            point = random.randint(1, len(parent1) - 1)
            child1 = parent1[:point] + parent2[point:]
            child2 = parent2[:point] + parent1[point:]
            new_population.extend([child1, child2])
        else:
            new_population.extend([parent1, parent2])
    return new_population


def mutation_simple(data, population, mut_prob):
    """Mutates individuals by changing a gene with a given probability."""
    for individual in population:
        for i in range(len(individual)):
            if random.random() < mut_prob:
                individual[i] = random.choice(POSSIBLE_MOVES)
    return population


# Setup Initial Board
board = [
    '#######',
    '# P   #',
    '#   $.#',
    '#     #',
    '#######'
]

initial_board_layout = []
player_start_pos = None

for r_idx, row_str in enumerate(board):
    row_chars = list(row_str)
    if 'P' in row_chars:
        c_idx = row_chars.index('P')
        player_start_pos = (r_idx, c_idx)
        row_chars[c_idx] = ' '  # Player is on a floor tile
    initial_board_layout.append(row_chars)

if player_start_pos is None:
    raise ValueError("Player 'P' not found in board.")

initial_game_state = SokobanState(initial_board_layout, player_start_pos)

print("Initial State:")
initial_game_state.display()

# Define GA Parameters
POP_SIZE = 250
CROSS_PROB = 0.85
MUT_PROB = 0.02
MAX_GENERATIONS = 200

# 3. Setup the 'data' dictionary for the GA function
data = {
    'initial_state': initial_game_state,
    'optimum': 1.0,  # Target fitness for plotting
}

# 4. Run the GA
results = GA(
    data=data,
    tmax=MAX_GENERATIONS,
    pop_size=POP_SIZE,
    cross_prob=CROSS_PROB,
    mut_prob=MUT_PROB,
    select=selection_tournament,
    cross=crossover_single_point,
    mutate=mutation_simple,
    get_initial_solution=get_initial_sokoban_chromosome,
    eval_func=sokoban_fitness_function,
    is_optimum=is_sokoban_solved_ga,
    sense='maximize'
)

# 5. Display Results
print("\n--- GA Run Complete ---")
if is_sokoban_solved_ga(results['Cost'], data):
    print("SUCCESS: A solution was found!")
    winning_moves = results['u']
    print("Winning Move Sequence (first part):", winning_moves[:30])  # Print a snippet

    # Simulate and display the winning sequence
    final_sim_state = initial_game_state.clone()
    for move_action in winning_moves:
        if not move(final_sim_state, move_action):
            break

    print("\nFinal board state after applying winning moves:")
    final_sim_state.display()
else:
    print("FAILURE: No solution found within the given generations.")
    print(f"Best fitness achieved: {results['Cost']:.4f}")