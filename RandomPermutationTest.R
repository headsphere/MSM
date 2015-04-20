
## @knitr RandomPermutationTest

RandomPermutationTest <- function (a, b) {
  # Combine the two datasets into a single dataset
  # i.e., under the null hypothesis, there is no difference between the two groups
  combined = c(a,b)
  
  # Observed difference
  t.observed = t.test(a, b)$statistic
  
  number_of_permutations = 1000
  
  t.random = NULL
  for (i in 1 : number_of_permutations) {
    # Sample from the combined dataset without replacement
    shuffled = sample (combined, length(combined))
    
    a.random = shuffled[1 : length(a)]
    b.random = shuffled[(length(a) + 1) : length(combined)]
    
    # Null (permuated) difference
    t.random[i] = t.test(a.random, b.random)$statistic
  }
  
  # P-value is the fraction of how many times the permuted difference is equal or more extreme than the observed difference
  
  pvalue = sum(abs(t.random) >= abs(t.observed)) / number_of_permutations
}
