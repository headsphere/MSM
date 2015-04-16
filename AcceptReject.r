#Example: Simulate Beta(2.7, 6.3 using a Uniform(0,1) envelope

#alpha <- 2.7
#beta <- 6.3
#Nsim = 1000
#successCount = SimulateAcceptReject(Nsim, 
#                        f_fn = function (x) {dbeta(x,alpha,beta)},
#                        g_fn = function (y) {dunif(y)},
#                        randg_fn = function (Nsim) {runif(Nsim)})


## @knitr SimulateAcceptReject
SimulateAcceptReject <- function(Nsim, f_fn, g_fn, randg_fn, x_interval = c(0,1), plot = TRUE) {
  #M is found by finding the maximum of f(x)/g(x) over [0,1]
  M = optimize(f=function(x){f_fn(x)/g_fn(x)},interval=x_interval,maximum=TRUE)$objective
  
  if(plot == TRUE){  
    #graphing logic
    ylim <- c(0, M * g_fn(randg_fn(1)))
    xlim <- x_interval
    curve(f_fn(x),from = x_interval[1],to = x_interval[2], xlim = xlim, ylim = ylim)
    par(new=T)
  }
  
  x = NULL
  successCount = 0
  while(successCount < Nsim){
    x_star = randg_fn(1)
    y_star = runif(1, max = M * g_fn(x_star)) # y* drawn from proposal function g
    #accept x* if y* <= f(x*)
    if(y_star <= f_fn(x_star)){ 
      successCount = successCount + 1
      x[successCount] = x_star
      
      if(plot == TRUE){  
        #plot the accepted points
        points(x_star, y_star, xlab = NA, ylab = NA, xaxt='n', yaxt='n', xlim = xlim, ylim = ylim, col="blue")
      }
    }
    else{
      if(plot == TRUE){  
        #plot the rejected points
        points(x_star, y_star, xlab = NA, ylab = NA, xaxt='n', yaxt='n', xlim = xlim, ylim = ylim, col="red")
      }
    }
  }
  return(x)
}
