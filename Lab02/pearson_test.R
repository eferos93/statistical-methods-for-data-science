set.seed(101)

#num of observations
n <- 50
#num of categories
K <- 4
#number of "players"
m <- 6

nullHypo <- c(7/16, 5/16, 3/16, 1/16)

observed <- matrix(0, nrow = m, ncol = n)
for (i in 1:m) {
  observed[i,] <- sample(1:K, n, prob = nullHypo, replace = TRUE)
}

#p-value very high (0.8074)
print( chisq.test( table(observed), p = nullHypo ) )

# New player with better skills join
observed <- rbind( observed, 
                   sample(1:K, n, prob = c(1/16, 3/16, 5/16, 7/16), replace = TRUE) 
                 )

#now the p-value is incredibly low
print( chisq.test( table(observed), p = nullHypo)  )


