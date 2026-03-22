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


## Task 2: Weighted Graph Laplacian

$$
w_{12}=1,\quad w_{23}=2
$$

$$
x=\begin{bmatrix}1\\0\\0\end{bmatrix}
$$

1. Construct the matrices:

$$
W=
\begin{bmatrix}
0 & 1 & 0 \\
1 & 0 & 2 \\
0 & 2 & 0
\end{bmatrix}
$$

$$
D=
\begin{bmatrix}
1 & 0 & 0 \\
0 & 3 & 0 \\
0 & 0 & 2
\end{bmatrix}
$$

$$
L=D-W=
\begin{bmatrix}
1 & -1 & 0 \\
-1 & 3 & -2 \\
0 & -2 & 2
\end{bmatrix}
$$

2. Compute:

$$
Lx=
\begin{bmatrix}
1 & -1 & 0 \\
-1 & 3 & -2 \\
0 & -2 & 2
\end{bmatrix}
\begin{bmatrix}1\\0\\0\end{bmatrix}
=
\begin{bmatrix}1\\-1\\0\end{bmatrix}
$$

- Compare with unweighted case  
The result is the same for this $x$  

- Which influence is stronger?  
from 2 to 3 (because $w_{23}=2 > w_{12}=1$)  

3. Compute:

$$
L^2x=L(Lx)=
\begin{bmatrix}
1 & -1 & 0 \\
-1 & 3 & -2 \\
0 & -2 & 2
\end{bmatrix}
\begin{bmatrix}1\\-1\\0\end{bmatrix}
=
\begin{bmatrix}2\\-4\\2\end{bmatrix}
$$

- Does the value at vertex 3 depend on the weight w23? Explain  
yes, values at 3rd row of $L$ depend on $w_{23}$, so the result of $L^2x$ at vertex 3 depends on $w_{23}$

4. Show that:

$$
x^TLx=\sum_{(i,j)\in E}w_{ij}(x_i-x_j)^2
$$

Starting from $L=D-W$:

$$
x^TLx=x^TDx-x^TWx
=\sum_{i} D_{ii}x_i^2 - \sum_{i,j} W_{ij}x_ix_j
=\sum_{i} \left(\sum_{j} w_{ij}\right)x_i^2 - \sum_{i,j} w_{ij}x_ix_j
=\sum_{i,j} w_{ij}x_i^2 - \sum_{i,j} w_{ij}x_ix_j
$$

Since $w_{ij}=w_{ji}$:

$$
x^TLx=\frac{1}{2} \sum_{i,j} w_{ij}x_i^2 + \frac{1}{2} \sum_{i,j} w_{ij}x_j^2 - \sum_{i,j} w_{ij}x_ix_j
=\frac{1}{2} \sum_{i,j} w_{ij}(x_i^2 + x_j^2 - 2x_ix_j)
=\frac{1}{2} \sum_{i,j} w_{ij}(x_i-x_j)^2
$$

Graph is undirected - edge $(i,j)$ is the same as edge $(j,i)$, so we count each edge twice in the sum

$$
x^TLx=\sum_{(i,j)\in E} w_{ij}(x_i-x_j)^2
$$

5. Interpret the role of weights in the expression above. What happens when $w_{ij}$ is very large?

The term $w_{ij}(x_i-x_j)^2$ penalizes differences across edge $(i,j)$ proportionally to $w_{ij}$

- If $w_{ij}$ is very large, then any difference $|x_i-x_j|$ becomes very expensive, so minimization focuses on obtaining $x_i\approx x_j$
- If $w_{ij}$ is small, that edge weakly constrains the values

