import numpy as np

def sigm(z):
    return 1 / (1 + pow(np.e, -z))

def ensure_dim(M, arr):
    if np.ndim(M) != len(arr):
        return False

    for i, dim in enumerate(M.shape):
        if(arr[i] != dim):
            return False
    
    return True

class my_nn:
    def __init__(self, W1, b1, W2, b2):
        self.W1 = W1
        self.b1 = b1
        self.W2 = W2
        self.b2 = b2

    def predict(self, x):
        # TODO calc output & check dimensionality
        
        A1 = x * self.W1.T + self.b1
        O1 = sigm(A1)
        A2 = O1 * self.W2.T + self.b2
        O2 = sigm(A2)

        return O2


