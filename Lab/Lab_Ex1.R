# REQUIRED FUNCTIONS #

# Biased sample-variance, done in an efficient manner
var_b <- function(x)
{
  var(x)*(length(x)-1)/length(x)
}

# EXERCISE #

set.seed(123)    # Be reproducible

R <- 1000        # Replications
n <- 10          # Samples per Replication

sigma <- 1       # Normal StdDev


samples <- array(0, c(R, n))
for (i in 1:R){
  samples[i, ] <- rnorm(n, 0, sigma)    # Can we be more efficient without rewriting everything?
}

samples_stat <- array(0, R)
samples_stat <- apply(samples, 1, var_b)


par(mfrow=c(1,1), oma=c(0,0,0,0))

hist(samples_stat, breaks= 40, probability = TRUE,    # Biased sample-variance (hist)
     xlab=expression(sb^2), main= bquote(sb^2), cex.main=1.5, ylim = c(0, 1.5))

# Biased sample-variance (line)
curve(((n)/sigma^2) * dchisq(x * ((n)/sigma^2), df = n - 1),
      add = TRUE, col="red", lwd=2, main="N(0,1)")

# UN-biased sample-variance (line)
curve(((n-1)/sigma^2) * dchisq(x * ((n-1)/sigma^2), df = n - 1),
      add = TRUE, col="blue", lwd=2, main="N(0,1)")

# Make the "bias" more apparent
segments(1, 0, 1, 1.2,
         col="orange", lwd=2)

# Legend
legend(x, y=NULL, legend, fill, col, bg)