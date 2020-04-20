library("ggplot2")

set.seed(140898)

R <- 1000
n <- 10
sigma <- 1

samples <- array(0, c(1, R, n))
for (i in 1:R){
  samples[1, i, ] <- rnorm(n, 0, 1)
}


samples_stat <- array(0, c(1, 2, R))
samples_stat[1, 1, ] <- apply(samples[1, , ], 1, mean)
samples_stat[1, 2, ] <- apply(samples[1, , ], 1, var)

store <- array(0, c(1, R))
for(i in (1:R))
{
  store[i] <- ((sum(samples[1, i, ]) - samples_stat[1, 1, i])^2)/(n)
}

# sb^2
par (mfrow=c(1,1), oma=c(0,0,0,0))


hist(samples_stat[1, 2, ], breaks= 40, probability = TRUE, 
     xlab=expression(s^2), main= bquote(s^2), cex.main=1.5)
hist(store, breaks= 80, probability = TRUE, 
     xlab=expression(sb^2), main= bquote(sb^2), cex.main=1.5)
curve(((n-1)/sigma^2) * dchisq(x * ((n-1)/sigma^2), df = n - 1),
      add = TRUE, col="red", lwd=2, main="N(0,1)")

