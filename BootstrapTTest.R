## @knitr BootstrapTTest

BootstrapTTest <- function (z, y) 
{
  B = 1000
  
  boot.t = numeric(B)
  
  x = c(z, y)
  
  n = length(z)
  m = length(y)
  
  t.obs =  t.test(z, y)$statistic
  
  for(i in 1:B)
  {
    shuffled = sample(x, n + m, replace = TRUE)
    
    zstar = shuffled[1 : n]
    ystar = shuffled[(n + 1) : (n + m)]  
    
    boot.t[i] = t.test(zstar, ystar)$statistic
  }
  
  return (sum(boot.t > t.obs)/B)
}