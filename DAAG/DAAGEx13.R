# simulate discrete MC according to transistion matrix P, given the number of iterations and th initial state
mc_simulation <- function( P, n, initial ) 
{
  # number of possible states
  s_states <- nrow(P)
  
  # store the states through time
  states <- numeric(n)
  
  #initialize var for first state
  states[1] <- initial
  
  for(t in (2:n))
  {
    # simulate next state
    p <- P[states[t-1], ]
    
    # next state is drawn from multinomial
    states[t] <- which(rmultinom(1, 1, p) == 1)
  }
  return(states)
}

P <- t(matrix(c( 0.6, 0.2, 0.2,
                 0.2, 0.4, 0.4,
                 0.4, 0.3, 0.3 ), nrow = 3, ncol = 3))

n_iter <- 1000

# simulate chain
MC <- numeric(n_iter)
MC <- mc_simulation( P, n_iter, 3 )

#matplot(MC, type = "p", col = "red", ylim = c(0,4), 
#        ylab = 'state', xlab='time')

result <- t(matrix(c("Sun", "Cloud", "Rain",
                     table(MC)/n_iter), nrow = 3, ncol = 2))
result
