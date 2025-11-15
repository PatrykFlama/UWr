# List 2


## Task  1


## Task  3
### concept 1:
greedy construct sentence and test all possible words for one place at a time - all other words should be either the chosen ones, or random  
additionally apply beam search to test _k_ best sentences

### concept 2:
genetic alg - choose at random _k_ samples, with each iteration generate _c*k_ new samples by random modifications: choose random word to modify, modify it at random. keep _k_ best samples

### concept 3:
keep _k_ samples  
calculate the words probability and choose the least probable one for modification (may be modified at random or using the most probable word)


## Task  4
> lets take the given corpus of prefixes ans calculate some _n-grams_ - then we can use them to generate fitting words, with guarantee tht we will only generate words starting on the same letter

load prefixes, choose one at random - then we could just take the tokens probability, but choose for random generation only those which start on same letter (kinda like task 2)



