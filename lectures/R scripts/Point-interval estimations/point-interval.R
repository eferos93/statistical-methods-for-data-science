library(tidyverse)
library(MASS)
M <- 100000; n1 <- 20; n2 <- 200; y1 <- y2 <- rep(NA, M)
for (i in 1:M) {
  y1[i] <- n1 %>% rpois(.,1) %>% mean
  y2[i] <- n2 %>% rpois(.,1) %>% mean
}
par(mfrow=c(1,2))
y1 %>% hist.scott(., xlim=c(0,2), main="", xlab = ""); abline(v=1,col=2)
y2 %>% hist.scott(., xlim=c(0,2), main="", xlab = ""); abline(v=1,col=2)

  
#-------------------------------------------

n <- 100; mat.y <- matrix(NA, nrow = M, ncol = 2)
for (i in 1:M) {
  y <- n %>% rnorm(.,5)
  mat.y[i,] <- c(mean(y), median(y))
}
par(mfrow=c(1,1))
plot(density(mat.y[,1]), type="l", main="") # black line
lines(density(mat.y[,2]), col=3) #green line