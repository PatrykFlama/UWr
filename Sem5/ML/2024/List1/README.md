# Whats bootstrap?
imagine that we tested the drug on some group of people and measured how much it helped each person  
how can we create better estimate of how effective the drug is, without running more tests (which are expensive)?  
we can use bootstraping:  
* from the original data randomly sample n elements (same number as the original data size), with replacement, to create new dataset - that will be **bootstrapped dataset**  
* now we can calculate the mean (or other statistic) of the bootstrapped dataset
* keep track of the mean (or other statistic) 

this process is called **bootstraping**  
after repeating bootstrapping many times, we end up with wide variety of means (or other statistics), which **estimates the full distribution of the statistic**  

this gives us sens of what might happen if we redid the experiment  

# What is Cross Validation?
imagine that we have some data and we want to train a model on it  
but which model should we use?  
cross validation allows us to compare different models on the same data, giving us sense of how well each model would perform in practice  

lets focus on one model; we need to split our data into two parts - training and testing  
we will train the model on the training data and then test it on the testing data, saving the perfomance of the model  
but how to choose on which data to train and on which to test?  

here comes cross validation:  
lets divide the data into k groups (folds)  
we will train the model on k-1 groups and test it on the remaining group  
we will repeat this process k times, each time using different group as testing data and keeping track of how well the model did on the testing data    

in the end each part of data will be used for testing and we can compare different models to see which one is the best  

for example, if we divide the data into 5 groups, we will train the model on 4 groups and test it on the remaining group, repeating this process 5 times - this is called **5-fold cross validation**  
in the extreme case we can use one sample as testing data and the rest as training data - this is called **leave-one-out cross validation**

note: instead of comparing models, we can compare one mode that needs some tuning parameter - this way we can compare each parameter and choose the best one  


# Data dimension reduction
## PCA
<!-- TODO -->

## t-SNE
rough idea: each data point is attracted to other close data points and repelled by data points that are far away - we do this to every point in multiple iterations and we end up with clustered data where similar data points are close to each other and dissimilar data points are far away from each other  
the t-sne name comes from probability distribution that is used to model the attraction to other data points  
