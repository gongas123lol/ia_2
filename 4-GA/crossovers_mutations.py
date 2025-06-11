from random import random

import numpy as np

from GA_app import CHROMOSOME_LENGTH, POSSIBLE_MOVES


def selection_tournament(population, pop_fitness, k=3):
    """Selects the next generation using tournament selection."""
    new_population = []
    pop_size = len(population)
    for _ in range(pop_size):
        # Select k random individuals for the tournament
        participants_indices = np.random.choice(range(pop_size), k)
        participants_fitness = [pop_fitness[i] for i in participants_indices]

        # The winner is the one with the highest fitness
        winner_local_idx = np.argmax(participants_fitness)
        winner_global_idx = participants_indices[winner_local_idx]
        new_population.append(population[winner_global_idx])
    return new_population


# --- CROSSOVER: Single-Point Crossover ---
def crossover_single_point(data, population, cross_prob):
    """Performs single-point crossover on pairs of parents."""
    new_population = []
    # Loop through parents in pairs
    for i in range(0, len(population), 2):
        parent1 = population[i]
        # Ensure we don't go out of bounds if population size is odd
        parent2 = population[i + 1] if i + 1 < len(population) else parent1

        if random.random() < cross_prob:
            point = random.randint(1, CHROMOSOME_LENGTH - 1)
            child1 = parent1[:point] + parent2[point:]
            child2 = parent2[:point] + parent1[point:]
            new_population.extend([child1, child2])
        else:
            # No crossover, just copy parents
            new_population.extend([parent1, parent2])
    return new_population


# --- MUTATION: Simple Random Mutation ---
def mutation_simple(data, population, mut_prob):
    """Mutates individuals by changing a gene with a given probability."""
    for i in range(len(population)):
        for j in range(CHROMOSOME_LENGTH):
            if random.random() < mut_prob:
                # Replace the gene with a new random move
                population[i][j] = random.choice(POSSIBLE_MOVES)
    return population