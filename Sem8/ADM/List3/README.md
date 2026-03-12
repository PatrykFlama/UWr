# List 1
## Task 1
1. Show that every element of the attention matrix A is strictly positive.

From the properties of the softmax function, each of its values is positive - therefore, the matrix A, on which softmax has been applied to each element, will be positive.

<!-- TODO: formula based proof -->

2. Prove that the sum of elements in each row of A is equal to 1.

Softmax is applied row-wise, so sum of each row is 1.

<!-- TODO: formula based proof -->

3. Provide a probabilistic interpretation of the element Aij in the context of the attention mechanism.


