import numpy as np
import nashpy as nash
A = np.array([[2999940.0, 2999850, 2999940.0, 2999940.0,2999910.0], [ 2000000.0,2000000 ,2000000.0, 2000000.0 ,2000000.0], [83000.0 ,  85900 ,  86600.0 ,  83300.0  , 86700.0], [168633.3 , 166400 , 167433.3 , 164266.7 , 165533.3],[1000000.0, 1000000, 1000000.0, 1000000.0, 1000000.0]])
B = np.array([[2999850.0, 2000000,  85900.0,  166400.0, 1000000.0],[2999940.0, 2000000,   86600.0,  167433.3, 1000000.0],[2999940.0, 2000000,   83300.0,  164266.7, 1000000.0],[2999910.0, 2000000,   86700.0 , 165533.3 ,1000000.0],[2999880.0, 2000000,   83900.0,  165333.3, 1000000.0]])
game = nash.Game(A, B)
iterations = 100
np.random.seed(0)
play_counts = tuple(game.fictitious_play(iterations=iterations)) 
#play_counts, distributions = play_counts_and_distribuions[-1]
print(play_counts)
play_counts[-1]
import matplotlib.pyplot as plt
plt.figure() 
play_counts = np.array(play_counts) + 1e-8  # add a small positive value to avoid dividing by zero
probabilities = [row_play_counts / np.sum(row_play_counts) for row_play_counts, col_play_counts in play_counts]
for number, strategy in enumerate(zip(*probabilities)):
    plt.plot(strategy, label=f"$s_{number}$")  
plt.xlabel("Iteration")  
plt.ylabel("Probability")  
plt.title("Actions taken by row player")  
plt.legend() 
plt.show()

probabilities = [col_play_counts / np.sum(col_play_counts) for row_play_counts, col_play_counts in play_counts]
for number, strategy in enumerate(zip(*probabilities)):
    plt.plot(strategy, label=f"$s_{number}$")  
plt.xlabel("Iteration")  
plt.ylabel("Probability")  
plt.title("Actions taken by column player")  
plt.legend() 
plt.show()