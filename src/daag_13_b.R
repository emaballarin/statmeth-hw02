library("lattice")
library("zoo")

set.seed(160898)

# Simulate a discrete MC according to transistion matrix P, given the number of iterations and the initial state
mc_simulation <- function(P, n, initial)
{
  # Number of possible states
  s_states <- nrow(P)
  
  # States through time
  states <- numeric(n)
  
  # Initial state
  states[1] <- initial
  
  for(t in (2:n))
  {
    # Next state
    p <- P[states[t-1],]
    states[t] <- which(rmultinom(1, 1, p) == 1)
  }
  
  return(states)
}

#transition matrix
P <- t(matrix(c( 0.6, 0.2, 0.2,
                 0.2, 0.4, 0.4,
                 0.4, 0.3, 0.3 ), nrow = 3, ncol = 3))

# plot theresult
plotmarkov <-
  function(n=1000, start=1, window=100, transition=P, npanels=5){
    xc2 <- mc_simulation(transition, n, start)
    mav0 <- rollmean(as.integer(xc2==0), window)
    mav1 <- rollmean(as.integer(xc2==0), window)
    npanel <- cut(1:length(mav0), breaks=seq(from=1, to=length(mav0),
                                             length=npanels+1), include.lowest=TRUE)
    df <- data.frame(av0=mav0, av1=mav1, x=1:length(mav0),
                     gp=npanel)
    print(xyplot(av0+av1 ~ x | gp, data=df, layout=c(1,npanels),
                 type="l", par.strip.text=list(cex=0.65),
                 scales=list(x=list(relation="free"))))
  }

# number of calculated states
n_iter <- 1000

plotmarkov(n_iter, 1, 100, P, 3)
