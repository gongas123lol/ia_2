import numpy as np
import matplotlib.pyplot as plt

def GA(data, tmax, pop_size, cross_prob, mut_prob,
       select, cross, mutate, get_initial_solution,
       eval_func, is_optimum, sense):
    """
    4-GA: Genetic Algorithm
    """
    num_evaluations = 0
    found_optimum = False

    # Initialize the population P(0), at random.
    population = get_initial_population(data, pop_size, get_initial_solution)
    # Evaluate P(0)
    pop_fitness = evaluate_population(data, population, eval_func)
    num_evaluations += pop_size

    fit = []
    mean_fit = []

    # Get best fitness
    best_fitness = get_best_fitness(pop_fitness, sense)[0]
    fit.append(best_fitness)
    mean_fit.append(np.mean(pop_fitness))

    t = 0
    while t < tmax and not found_optimum:
        t += 1
        # Step 2: Selection
        population = select(population, pop_fitness)
        # Step 3: Crossover
        population = cross(data, population, cross_prob)
        # Step 4: Mutation
        population = mutate(data, population, mut_prob)
        # Step 5: Evaluation
        pop_fitness = evaluate_population(data, population, eval_func)
        num_evaluations += pop_size

        best_fitness = get_best_fitness(pop_fitness, sense)[0]
        fit.append(best_fitness)
        mean_fit.append(np.mean(pop_fitness))

        if is_optimum(best_fitness, data):
            found_optimum = True

    # Get best solution
    best_fitness, best_idx = get_best_fitness(pop_fitness, sense)
    best_solution = population[best_idx[0]]

    print('BestCost:', best_fitness)
    print('NumEvaluations:', num_evaluations)

    # Plotting
    generations = np.arange(1, len(fit) + 1)

    plt.figure(1)
    plt.plot(fit)
    plt.title('Best Fitness over Generations')

    plt.figure(2)
    optimum = data.get('optimum', 1)  # Avoid div by zero if not present
    plt.plot(generations, np.array(fit) / optimum * 100, 'k-', label='Pop Max')
    plt.plot(generations, np.array(mean_fit) / optimum * 100, 'k:', label='Pop Mean')
    plt.xlabel('Generation no.')
    plt.ylabel('Fitness (%)')
    plt.ylim([50, 110])
    plt.legend()
    plt.show()

    return {
        'NumEvaluations': num_evaluations,
        'Cost': best_fitness,
        'tmax': tmax,
        'popSize': pop_size,
        'crossProb': cross_prob,
        'mutProb': mut_prob,
        'u': best_solution,
        's': best_solution,
        'Fit': fit
    }


def get_best_fitness(fitness_list, sense):
    fitness_array = np.array(fitness_list)
    if sense == 'maximize':
        best_val = np.max(fitness_array)
        indices = np.where(fitness_array == best_val)[0]
    elif sense == 'minimize':
        best_val = np.min(fitness_array)
        indices = np.where(fitness_array == best_val)[0]
    else:
        raise ValueError("sense must be 'maximize' or 'minimize'")
    return best_val, indices


def get_initial_population(data, pop_size, get_initial_solution):
    return [get_initial_solution(data) for _ in range(pop_size)]


def evaluate_population(data, population, eval_func):
    return [eval_func(individual, data) for individual in population]
