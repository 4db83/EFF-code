import numpy as np

# Define a 3x4 matrix of random values
random_probs = np.random.rand(3, 4)

# Normalize the values so that they sum to 1
random_probs /= random_probs.sum()

random_probs