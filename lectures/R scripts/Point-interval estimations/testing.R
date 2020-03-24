library(tidyverse)
install.packages("DAAG")
library(DAAG)
data <- pair65
y <- pair65$heated - pair65$ambient
#test for the mean == 0

help("t.test")
y %>% t.test

#null distr (dt is t-student)
n <- y %>% length
curve(dt(x,8), xlim=c(-6,6))
t_obs <- mean(y)/sqrt(var(y)/n)

alpha <- 0.05
quantile_t.1 <-qt(alpha/2,8)
quantile_t.2 <- qt(1-alpha/2, 8)

segments(quantile_t.1, -0.1, quantile_t.1,
         dt(quantile_t.1,8), col = "red")
segments(quantile_t.2, -0.1, quantile_t.2,
         dt(quantile_t.2, 8), col = "red")

#it falls in the area of rejection
segments(t_obs, -0.1, t_obs,
         dt(t_obs, 8), col = "blue")

#p-value
p_value <-2*(1-pt(t_obs, n-1))


#temptative to make independent samples 
#comparison

x1 <- pair65$heated
x2 <- pair65$ambient
#what i obtain here is the opposite,
#very high p-value, very motivated to reject H0

t.test(x1, x2, var.equal = TRUE)
