# List 1
## Task 1
1. Show that every element of the attention matrix A is strictly positive.

From the properties of the softmax function, each of its values is positive - therefore, the matrix A, on which softmax has been applied to each element, will be positive.

More formally, let

$$
S = \frac{QK^T}{\sqrt{d_k}}
$$

Since attention is computed row-wise with softmax

$$
A_{ij} = \frac{e^{S_{ij}}}{\sum_l e^{S_{il}}}
$$

For every $i,j$, we have $e^{S_{ij}} > 0$. The denominator $\sum_l e^{S_{il}}$ is a sum of positive terms, so it is also strictly positive. Therefore

$$
A_{ij} > 0
$$

for every $i,j$, which proves that every element of the attention matrix $A$ is strictly positive.

2. Prove that the sum of elements in each row of A is equal to 1.

Softmax is applied row-wise, so sum of each row is 1.

More formally, for a fixed row $i$

$$
A_{ij} = \frac{e^{S_{ij}}}{\sum_l e^{S_{il}}}
$$

Hence

$$
\sum_j A_{ij}
= \sum_j \frac{e^{S_{ij}}}{\sum_l e^{S_{il}}}
= \frac{\sum_j e^{S_{ij}}}{\sum_l e^{S_{il}}}
= 1
$$

Thus, the sum of elements in every row of $A$ is equal to 1.

3. Provide a probabilistic interpretation of the element Aij in the context of the attention mechanism.

The element $A_{ij}$ can be interpreted as the probability that token $i$ attends to token $j$.

For a fixed query position $i$

$$
A_{ij} = \exp\left(\frac{q_i \cdot k_j}{\sqrt{d_k}}\right)
/
\sum_l \exp\left(\frac{q_i \cdot k_l}{\sqrt{d_k}}\right)
$$

Since $A_{ij} > 0$ for every $j$ and $\sum_j A_{ij} = 1$, the $i$-th row of $A$ forms a probability distribution over all key positions $j$.

Therefore, $A_{ij}$ represents how likely it is that, when processing token $i$, the model focuses on token $j$. Larger values of $A_{ij}$ mean stronger attention paid by token $i$ to token $j$.
