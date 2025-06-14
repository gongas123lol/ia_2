# AI-translated matlab to python code

import math
import random
import matplotlib.pyplot as plt


def simulated_annealing(Tmax, Tmin, R, k,
                        data, get_initial_solution, get_random_neighbor, eval_func,
                        is_optimum, sense):
    """
    Simulated Annealing algorithm for optimization.

    Args:
        Tmax (float): Maximum initial temperature.
        Tmin (float): Minimum temperature (stopping criterion).
        R (float): Cooling rate (for exponential decay).
        k (int): Number of iterations at each temperature step.
        data: Any additional data required by the solution/evaluation functions.
        get_initial_solution (function): A function that returns an initial solution.
                                          Signature: solution = get_initial_solution(data)
        get_random_neighbor (function): A function that returns a random neighbor of a solution.
                                        Signature: neighbor = get_random_neighbor(current_solution, data)
        eval_func (function): A function that evaluates the cost/fitness of a solution.
                              Signature: cost = eval_func(solution, data)
        is_optimum (function): A function that checks if a solution is optimal.
                               Signature: is_optimal = is_optimum(cost, data)
        sense (str): 'minimize' or 'maximize' to indicate the optimization goal.

    Returns:
        dict: A dictionary containing the results of the SA run.
    """

    # Rate increment variable
    t = 0
    # Step 1: Make T = Tmax
    T = Tmax
    # Number of evaluations
    num_evaluations = 0
    # Variable used to specify stop criteria
    found_optimum = False

    # Choose a solution u (at random) and compute fu = f(u)
    u = get_initial_solution(data)
    fu = eval_func(u, data)
    print(f'Initial cost: {fu}')
    # Increment number of evaluations
    num_evaluations += 1

    F = [fu]  # To store costs at each step for plotting

    while not found_optimum:
        # Step 2: Repeat k times
        i = 0
        while i < k and not found_optimum:
            # Select a neighbor of u, say v.
            v = get_random_neighbor(u, data)
            # Evaluate v
            fv = eval_func(v, data)
            # Increment number of evaluations
            num_evaluations += 1

            # If f(v) is better than f(u) make u = v;
            # Else make u = v with probability p

            # Calculate difference based on sense
            if sense == 'minimize':
                dif = fv - fu  # We want a smaller fv
            elif sense == 'maximize':
                dif = fu - fv  # We want a larger fv
            else:
                raise ValueError("Sense must be 'minimize' or 'maximize'")

            #print(f'fu (sol) = {fu}, fv (new neighbor) = {fv}')

            if dif < 0:  # This means fv is "better" (lower for minimize, higher for maximize)
                #print('Neighbor accepted (better)')
                u = v
                fu = fv
            else:
                prob = calculate_probability(fu, fv, T, sense)
                x = random.random()  # Generate a random number between 0 and 1
                #print(f'Probability: {prob:.4f}, Random: {x:.4f}')
                if x <= prob:
                    # Accept this solution even if it's worse
                    #print('Neighbor accepted (worse, by probability)')
                    u = v
                    fu = fv
                #else:
                 #   print('Neighbor rejected')

            # Make i = i+1.
            i += 1

            F.append(fu)

            # if optimum found then stop.
            if is_optimum(fu, data):
                found_optimum = True

        if not found_optimum:
            # Step 3: Make t = t+1; Set T = T(t)
            t += 1
            T = calculate_temperature(t, Tmax, R)
            print(f'Current Temperature: {T:.4f}')
            # If T < Tmin Stop.
            if T < Tmin:
                print('Temperature fell below Tmin. Stopping.')
                break

    print('\n--- SA Results ---')
    print(f'Best Cost: {fu}')
    print(f'Number of Evaluations: {num_evaluations}')

    results = {
        'T': T,
        'NumEvaluations': num_evaluations,
        'Cost': fu,
        'Tmax': Tmax,
        'Tmin': Tmin,
        'R': R,
        'k': k,
        'final_solution': u,
        'Cost_History': F
    }

    plt.figure(figsize=(10, 6))
    plt.plot(F)
    plt.title('Cost History During Simulated Annealing')
    plt.xlabel('Iteration')
    plt.ylabel('Cost')
    plt.grid(True)
    plt.show()

    return results


def calculate_temperature(t, Tmax, R):
    """
    Calculates the new temperature using exponential decay.
    """
    return Tmax * math.exp(-R * t)


def calculate_probability(fu, fv, T, sense):
    """
    Calculates the acceptance probability.
    """
    if T == 0:  # Avoid division by zero if T somehow becomes 0
        return 0.0

    if sense == 'maximize':
        # Probability for accepting a worse solution (lower fv) when maximizing
        # We want exp( (fv - fu) / (T * fu) )
        # If fv < fu, (fv-fu) is negative.
        # If fu is negative (possible with some costs), this could invert logic.
        # Assuming fu is generally positive for typical costs.
        # A common approach for maximization is exp( (new_fitness - current_fitness) / T )
        return math.exp((fv - fu) / T)
    elif sense == 'minimize':
        # Probability for accepting a worse solution (higher fv) when minimizing
        # We want exp( (fu - fv) / (T * fu) )
        # If fv > fu, (fu-fv) is negative.
        # If fu is negative (possible with some costs), this could invert logic.
        # A common approach for minimization is exp( (current_cost - new_cost) / T )
        return math.exp((fu - fv) / T)
    else:
        raise ValueError("Sense must be 'minimize' or 'maximize'")
