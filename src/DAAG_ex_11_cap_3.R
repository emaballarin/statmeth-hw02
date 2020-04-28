# DAAG EXERCISES

## ex. 11 cap. 3

aberrant_crypt <- c(87, 53, 72, 90, 78, 85, 83)
n <- length(aberrant_crypt)
M <- 10000
crypt_mean <-mean(aberrant_crypt)
crypt_var <- var(aberrant_crypt)

paste("aberrant crypt mean:", crypt_mean)
paste("aberrant crypt variance:", crypt_var)

vec_mean <- c()
vec_var <- c()
for (i in 0:M-1){
  x <- rpois(n, 78.28)
  vec_mean <- c(vec_mean, mean(x))
  vec_var <- c(vec_var, var(x))
}

hist(x = abs(vec_mean -vec_var), 
     main = "Difference between mean and variance", 
     xlab = "|mean - var|", 
     ylab = "frequency")

# the histogram shows the absolute difference between means and variances,
# note how the largest part of the samples have this difference equals to zero.
# So, we may conclude that mean and variance are almost the same.

# fisher test
dispersion_ab <- sum((aberrant_crypt - crypt_mean)^2)/crypt_mean
p_value <- pchisq(dispersion_ab, df= n-1, lower.tail= FALSE)
paste("p-value:", p_value)

# the test gives a p-value = 0.056 so we may accept the null hypothesis i.e.
# the aberrant_crypt sample may occur from a Poisson distribution.