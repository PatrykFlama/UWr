[back](../)

# List 3
## Task 1
### Subtask A
Prove that the MLE for variance is equal to
$$
\widehat{\sigma}_n^2=\frac{1}{N} \sum_{i=1}^N\left(y_i-x_i \widehat{\beta}_N\right)^2
$$

___

$$ 
\frac{\partial}{\partial \sigma^2} l\left(\beta, \sigma^2 ; y, X\right)=0 
$$

$$ \frac{\partial}{\partial \sigma^2} l\left(\beta, \sigma^2 ; y, X\right) $$
$$ = \frac{\partial}{\partial \sigma^2} \left(-\frac{N}{2} \ln (2 \pi)-\frac{N}{2} \ln \left(\sigma^2\right)-\frac{1}{2 \sigma^2} \sum_{i=1}^N\left(y_i-x_i \beta\right)^2\right) $$
$$ = -\frac{N}{2 \sigma^2}+\frac{1}{2 \sigma^4} \sum_{i=1}^N\left(y_i-x_i \beta\right)^2 $$
$$ = \frac{1}{2 \sigma^4} \left(\sum_{i=1}^N\left(y_i-x_i \beta\right)^2-N \sigma^2\right) $$
$$ = \frac{1}{2 \sigma^4} \left((y-X \beta)^{\top}(y-X \beta)-N \sigma^2\right) $$

which is equal to zero only if  

$$
\sum_{i=1}^N\left(y_i-x_i \beta\right)^2-N \sigma^2=0
$$

thus

$$
\sigma^2=\frac{1}{N} \sum_{i=1}^N\left(y_i-x_i \beta\right)^2
$$

### Subtask B
1. Find the form of the loglikelihood.
2. Compute the gradient of logistic function with respect to $\beta$

You can either tex your solution and put it in this notebook or attach photos of your solution.

___

$$ \mathrm{P}\left(y_i=1 \mid x_i\right)=S\left(x_i \beta\right) $$
$$ S(t)=\frac{1}{1+\exp (-t)} $$

$$ \mathrm{P}\left(y_i=0 \mid x_i\right)=1-S\left(x_i \beta\right) $$

The vector of coefficients $\beta$ is the parameter to be estimated by maximum likelihood.
We assume that the estimation is carried out with an IID sample comprising $N$ data points

#### 1. Find the form of the loglikelihood.

$$ P(y \mid x; \beta) = S(x\beta)^y (1-S(x\beta)^{1-y}) $$

$$ L(\beta) = P(y | x;\beta) $$
$$ = \prod_{i=1}^m P(y_i | x_i; \beta) $$
$$ = \prod_{i=1}^m S(x_i\beta)^{y_i} (1-S(x_i\beta)^{1-y_i}) $$

$$ l(\beta) = \ln L(\beta) $$
$$ = \ln \prod_{i=1}^m S(x_i\beta)^{y_i} (1-S(x_i\beta)^{1-y_i}) $$
$$ = \sum_{i=1}^m \ln S(x_i\beta)^{y_i} (1-S(x_i\beta)^{1-y_i}) $$

We will maximize log likelihood (using gradient ascent) to find the best $\beta$.

#### 2. Compute the gradient of logistic function with respect to $\beta$
$$ \frac{\partial S(x\beta)}{\partial \beta} = \frac{\partial}{\partial \beta} \left(\frac{1}{1+\exp (-x\beta)}\right) $$
$$ = -\frac{1}{(1+\exp(-x\beta))^2} \frac{\partial}{\partial \beta}\exp(-x\beta) $$
$$ = \frac{\exp(-x\beta) x}{(1+\exp(-x\beta))^2} $$

