# List 4
## Task 1: Action of the Graph Laplacian

1. Compute the matrices:

$$
A = \begin{bmatrix}
0 & 1 & 0 \\
1 & 0 & 1 \\
0 & 1 & 0
\end{bmatrix}
$$

$$
D = \begin{bmatrix}
1 & 0 & 0 \\
0 & 2 & 0 \\
0 & 0 & 1
\end{bmatrix}
$$

$$
L = D - A =
\begin{bmatrix}
1 & -1 & 0 \\
-1 & 2 & -1 \\
0 & -1 & 1
\end{bmatrix}
$$

2. Compute the vector:
 
$$
Lx =
\begin{bmatrix}
1 & -1 & 0 \\
-1 & 2 & -1 \\
0 & -1 & 1
\end{bmatrix}
\begin{bmatrix}1\\0\\0\end{bmatrix}
=
\begin{bmatrix}1\\-1\\0\end{bmatrix}
$$

- Which vertices have non-zero values?  
vertices 1 and 2

- On which values does (Lx)i depend?  
on $x_i$ and values at neighbors of $i$
$$
(Lx)_i = \sum_{j\sim i}(x_i - x_j)
$$

- Describe in words what operation Lx performs  
It computes sum of differences between each vertex value and neighboring values


3. Compute:

$$
Lx = \begin{bmatrix}1\\-1\\0\end{bmatrix}
$$

so

$$
L^2x =
\begin{bmatrix}
1 & -1 & 0 \\
-1 & 2 & -1 \\
0 & -1 & 1
\end{bmatrix}
\begin{bmatrix}1\\-1\\0\end{bmatrix}
=
\begin{bmatrix}2\\-3\\1\end{bmatrix}
$$

- Does a non-zero value appear at vertex 3?  
yes

- Does L2x depend only on neighbors, or also on more distant vertices?  
Also on more distant vertices (up to distance $2$ in the original signal $x$), because second application could use results calculated from neighbors of neighbors.

4. Formulate a general rule describing the behavior of Lkx.


Each multiplication by $L$ mixes information across neighbors signals. Therefore, after $k$ applications, $(L^k x)_i$ can depend on values of $x$ from vertices within graph distance at most $k$ from $i$.

So powers of the Laplacian progressively spread information farther across the graph while still being built from local neighbor differences at each step.

