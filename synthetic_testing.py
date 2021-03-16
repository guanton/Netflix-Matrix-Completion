import numpy as np
from scipy.sparse import random
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import math

""" This code is for testing our Frank-Wolfe algorithm on synthetic data.
We use random matrix initializations for the synthetic data.
"""


def conditional_gradient(O, B, max_iter, tol, tau):
    """Parameters:
    -----------
        O: the filter of 1s and 0s
        B: the known submatrix of X
        max_iter: maximum number of iterations
        tol: threshold for stopping algorithm
    -----------
    Returns approximately completed matrix
    """
    # first, we generate a valid x_0 using two random unit vectors u,v. We may initialize this differently as well
    u = np.random.rand(n, 1)
    u = tau * u / np.linalg.norm(u)
    v = np.random.rand(m, 1)
    v = tau * v / np.linalg.norm(v)
    v = np.transpose(v)
    x_k = tau * np.matmul(u, v)
    # retrieve non-zero entries of mask O for line search
    omega = np.transpose(np.nonzero(O))
    for k in range(max_iter):
        # gradient at x_k is the filtered x_k subtracted by B
        grad = np.multiply(O, x_k) - B
        x_k_ = tau * oracle(grad)
        if np.linalg.norm(x_k_) <= tol:
            break
        # line search
        num = 0
        denom = 0
        for i, j in omega:
            num = num + (x_k[i, j] - B[i, j]) * (x_k[i, j] - x_k_[i, j])
            denom = denom + (x_k_[i, j] - x_k[i, j]) ** 2
        step_size = num / denom
        if step_size > 1:
            step_size = 1
        if step_size < 0:
            step_size = 0
        # update step
        x_k = x_k + step_size * (x_k_ - x_k)
    return x_k


def conditional_gradient_t_classic(O, B, max_iter, tol, tau):
    """Parameters:
    -----------
        O: the filter of 1s and 0s
        B: the known submatrix of X
        max_iter: maximum number of iterations
        tol: threshold for stopping algorithm
    -----------
    Returns approximately completed matrix using classic step size and array tracking convergence to solution in norm

    """
    # first, we generate a valid x_0 using two unit vectors u,v
    u = np.random.rand(n, 1)
    u = tau * u / np.linalg.norm(u)
    v = np.random.rand(m, 1)
    v = tau * v / np.linalg.norm(v)
    v = np.transpose(v)
    x_k = tau * np.matmul(u, v)
    trace = []
    for k in range(max_iter):
        # gradient at x_k is the filtered x_k subtracted by B
        grad = np.multiply(O, x_k) - B
        x_k_ = tau * oracle(grad)
        if np.linalg.norm(x_k_) <= tol:
            break
        step_size = 2 / (k + 2)
        x_k = x_k + step_size * (x_k_ - x_k)
        trace.append(0.5 * np.linalg.norm(x_k - X) ** 2)
    return x_k, trace


def conditional_gradient_t_optimal(O, B, max_iter, tol, tau):
    """Parameters:
    -----------
        O: the filter of 1s and 0s
        B: the known submatrix of X
        max_iter: maximum number of iterations
        tol: threshold for stopping algorithm
     -----------
    Returns approximately completed matrix using optimal step size and array tracking convergence to solution in norm
    """
    # first, we generate a valid x_0 using two unit vectors u,v
    u = np.random.rand(n, 1)
    u = tau * u / np.linalg.norm(u)
    v = np.random.rand(m, 1)
    v = tau * v / np.linalg.norm(v)
    v = np.transpose(v)
    x_k = tau * np.matmul(u, v)
    trace = []
    omega = np.transpose(np.nonzero(O))
    for k in range(max_iter):
        # gradient at x_k is the filtered x_k subtracted by B
        grad = np.multiply(O, x_k) - B
        x_k_ = tau * oracle(grad)
        if np.linalg.norm(x_k_) <= tol:
            break
        # line search
        num = 0
        denom = 0
        for i, j in omega:
            num = num + (x_k[i, j] - B[i, j]) * (x_k[i, j] - x_k_[i, j])
            denom = denom + (x_k_[i, j] - x_k[i, j]) ** 2
        step_size = num / denom
        if step_size > 1:
            step_size = 1
        if step_size < 0:
            step_size = 0
        x_k = x_k + step_size * (x_k_ - x_k)
        trace.append(0.5 * np.linalg.norm(x_k - X) ** 2)
    return x_k, trace


def oracle(G):
    """LMO: returns argmin for linear minimization problem
    """
    x, y = power_method(-G, 0.0000001)
    return x.reshape((len(x), 1)).dot(y.reshape((1, len(y))))


def power_method(A, eps):
    """Power method.

        Computes approximate top left and right singular vector.
    Parameters:
    -----------
        A : array {m, n},
            input matrix
        eps: stopping threshold
    Returns:
    --------
        x, y : (m,), (n,)
            two arrays representing approximate top left and right
            singular vectors.
    """
    m, n = A.shape
    x = np.random.rand(m)
    x = x / np.linalg.norm(x)
    prev_x = np.zeros(m)
    y = A.T.dot(x)
    y = y / np.linalg.norm(y)
    while np.linalg.norm(x - prev_x) > eps:
        prev_x = x
        x = A.dot(y)
        x = x / np.linalg.norm(x)
        y = A.T.dot(x)
        y = y / np.linalg.norm(y)
    return x, y


"""The following code graphs convergence plots, comparing the classic step size with the optimal step size
"""
n, m = 500, 1000
X = np.random.randint(1, 6, size=(n, m))
O = random(n, m, density=0.15).toarray()
O = (O > 0.5).astype(int)
B = np.multiply(O, X)
maxiters = 40
X_0, convs_o = conditional_gradient_t_optimal(O, B, maxiters, 0.000001, tau=1800)
X_1, convs = conditional_gradient_t_classic(O, B, maxiters, 0.000001, tau=1800)
t = np.arange(maxiters)
plt.plot(t, convs_o, color='red')
plt.plot(t, convs, color='green')
plt.yscale('log', base=2)
plt.ylabel('Difference in Squared Frobenius Norm to solution X', fontsize=20)
plt.xlabel('# iterations', fontsize=20)
plt.title('Convergence to Solution for Matrix Completion Approximation', fontsize=35)
red_patch = mpatches.Patch(color='red', label='optimal step size')
green_patch = mpatches.Patch(color='green', label='standard step size')
plt.legend(handles=[red_patch, green_patch])
plt.show()
