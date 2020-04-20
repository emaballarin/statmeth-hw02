set.seed(101)
  
# observations and cells 
n <- 50
K <- 4

##### here is the pro alone ####
# generate the values
y <- sample( 1:K, n, replace=TRUE, prob =c( 1/16, 3/16, 5/16, 7/16) )
observed <- table(y)
expected <- c( n*(1/16), n*(3/16), n*(5/16), n*(7/16) )
x2 <- sum( (observed-expected)^(2)/expected)
# manually compute the p-value
pchisq(x2, df =K-1, lower.tail =FALSE )
# same result with the chisq.test function
chisq.test( observed, p = c( 1/16, 3/16, 5/16, 7/16) )


##### joined with others ####
# we're in 7 now
m <- 7
friends <- array(0, c(m, n))
# 6 are bad 
for (j in (1:(m-1)))
{
  friends[j, ] <- sample(1:K, n, replace=TRUE, prob=c(7/16, 5/16, 3/16, 1/16))
}
# here the pro
friends[m, ] <- sample(1:K, n, replace=TRUE, prob=c(1/16, 3/16, 5/16, 7/16))

# observed values
observed <- array(0, c(m, K))
for (j in (1:m))
{
  observed[j, ] <- table(friends[j, ]) 
}

# expected values
expected <- array(0, c(m, K))
for (j in (1:m-1))
{
  expected[j, ] <- c( n*(7/16), n*(5/16), n*(3/16), n*(1/16) )
}
expected[m, ] <- c( n*(1/16), n*(3/16), n*(5/16), n*(7/16) )

# let's compute the Pearson's test storing the difference between observed and expected values in a matrix
difference <- array(0, c(m, K))
for (j in (1:m))
{
  for (i in (1:K))
  {
    difference[j, i] = ((observed[j, i] - expected[j, i])^2/expected[j, i])
  }
}

# manually compute the p-value
x2 <- sum(difference)
pchisq(x2, df = K-1, lower.tail =FALSE )

