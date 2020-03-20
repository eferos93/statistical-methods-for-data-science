# *******************************************************************************************#
# ******************************** Discrete random variables ********************************#
# *******************************************************************************************#
###########################
## Binomial distribution ##
###########################
##1. compute the probability to obtain 5 "Heads" tossing a fair coin 20 times ----------------
choose(20,5)*.5^5*.5^(20-5)


##2. compute P(X=x) for all the values of x. Is it a p.m.f.? Make a plot ---------------------
x <- 0:20
pbin <- choose(20,x)*.5^x*(1-.5)^(20-x)
pbin

sum(pbin) #is it a p.m.f.?

plot(x, pbin, type="h", main="Bin(20, 0.5)", 
     xlab="x", ylab="f(x)")




##3. write a function to compute probabilities varying x, n and p ----------------------------
binomial <- function(x,n,p){
    choose(n,x)*p^x*(1-p)^(n-x)
}



##4. compute the probability to obtain two times the score 6 rolling 15 times a dice ---------
##   choose the right build-in function in R and verify your result --------------------------
binomial(2,15,1/6)
dbinom(2,15,1/6)



##5. using your function compute P(X<=3), verify your result with the one --------------------
##   obtained using the build-in function (use X~Bin(10,.2)) ---------------------------------
n <- 10
p <- .2
sum(binomial(0:3,10,.2))
pbinom(3,10,0.2)



##6. compute the probability to observe a number of success bewteen 3 (excluded) and 5 -------
##   for a r.v. distributed as a Bin(20,.3), use your function and the build-in function -----
x1 <- 3
x2 <- 5
n  <- 20
p  <- .3
p.le.x2 <- pbinom(x2,n,p) 
p.le.x1 <- pbinom(x1,n,p) 

prob.interval <- p.le.x2 - p.le.x1
prob.interval

sum(binomial(0:x2,n,p))-sum(binomial(0:x1,n,p))



##7. plot p.m.f. and c.d.f. of the previous point---------------------------------------------
par1 <- par(mfrow=c(1,2)) 
bin1 <- function(x) dbinom(x,size=20,prob=.3)

plot(0:n, bin1(0:n),type="h",xlab="x",ylab="p.m.f.",main = " ")
bin2 <- function(x) pbinom(x,size=n,prob=.3)
plot(-1:21, bin2(-1:21), type="s",xlab="x",ylab="c.d.f.",
     main = " ")



##8. plot the binomial distribution for n=20 and p=.3 and then p=.6---------------------------
x  <- c(0:20)
y0 <- binomial(x,20,.3)
y1 <- binomial(x,20,.6)
plot(x+.1,y0,xlim=c(0,20),
     ylim=c(0,.2),type="h",lwd=3,
     xlab="x",ylab="p(x)")
par(new=T)
plot(x-.1,y1,xlim=c(0,20),
     ylim=c(0,.2),type="h",lwd=3,
     xlab=" ",ylab=" ", col="red")

# or
plot(x, y0, type="h", ylim=c(0,.2), 
     xlab="x", ylab="p(x)")
points(x-.1, y1, type="h", col=2)



##9. compare p.m.f. varying p ----------------------------------------------------------------
par2 <- par(mfrow=c(2,2))
x <- 0:20 
n <- 20
plot(x,dbinom(x, n, .1),type="h",
     xlab="x",ylab="f(x)",ylim = c(0,.35),
     main ="Bin(n=20, p=.1)")
plot(x,dbinom(x, n, .3),type="h",
     xlab="x",ylab="f(x)",ylim = c(0,.35),
     main ="Bin(n=20, p=.3)")
plot(x,dbinom(x, n, .5),type="h",
     xlab="x",ylab="f(x)",ylim = c(0,.35),
     main ="Bin(n=20, p=.5)")
plot(x,dbinom(x, n, .9),type="h",
     xlab="x",ylab="f(x)",ylim = c(0,.35),
     main ="Bin(n=20, p=.9)")



##10. rbinom ---------------------------------------------------------------------------------
set.seed(123) 
a <- rbinom(n=20,size=1,prob=.5)
sum(a)
sum(a)/20 #p=.5

b <- rbinom(n=100,size=1,prob=.5)
sum(b)
sum(b)/100


###Build in functions recap:
#dbinom(x, size, prob, log = FALSE)                         #p.m.f.
#pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)    #c.d.f.
#qbinom(p, size, prob, lower.tail = TRUE, log.p = FALSE)    #quantile
#rbinom(n, size, prob)                                      #random samples

#well done! go back to the slides 

##########################
## Poisson distribution ##
##########################
##1. write a function to compute the probabilities of a Poisson r.v. -------------------------
poisson <- function(x,lambda){
  lambda^x*exp(-lambda)/factorial(x)
}


##2. plot the p.m.f. for X~Pois(2) -----------------------------------------------------------
x <- c(0:10)

y <- poisson(x,2)
plot(x, y, type="h", xlab="x",ylab="f(x)", 
     main="Pois(2)")


##3. compare the plots varying the value of the parameter lambda -----------------------------
par(mfrow=c(1,3))
plot(x, poisson(x,lambda=0.5), type="h", xlab="x", ylab="f(x)",
     main=expression(paste("Poisson: ", lambda, " = 0.5")))

plot(x, poisson(x,lambda=2), type="h", xlab="x", ylab="f(x)",
     main=expression(paste("Poisson: ", lambda, " = 2")))

plot(x, poisson(x,lambda=3.5), type="h", xlab="x", ylab="f(x)",
     main=expression(paste("Poisson: ", lambda, " = 3.5")))


##4. compute P(X = 0) for X ∼ Pois(3) --------------------------------------------------------
poisson(0, 3)
dpois(0,3)

##5. compute P(X<=4) for X ∼ Pois(3) using your function and verify your result with --------- 
##   the build-in function -------------------------------------------------------------------
sum(dpois(0:4,3))
ppois(4,3)


#well done! go back to the slides 


# *******************************************************************************************#
# ******************************* Continuous random variables *******************************#
# *******************************************************************************************#

#########################
## Normal distribution ##
#########################
##1. plot (on a single plot) the density functions of: ---------------------------------------
## -a standard normal distribution 
## -a normal distribution with mean 0 and std.dev 1.5
## -a normal distribution with mean 0 and variance 4 -----------------------------------------

x <- seq(-6, 10, length = 100)
plot(x, dnorm(x), type="l", xlab="x", ylab= "f(x)") 
curve(dnorm(x, mean=1, sd=1.5), add=TRUE, lty=2, col=2) 
points(x, dnorm(x, mean=3, sd=2), type="l", col=3)

##2.for X~N(70,25) compute: ------------------------------------------------------------------
## -P(X<=90) and P(65<=X<=75)
## -IQR --------------------------------------------------------------------------------------

mu    <- 70
sigma <- 5

pnorm(90, mu, sigma)
#or 
z <- (90-mu)/sigma
pnorm(z)

z1 <- (65-mu)/sigma
z2 <- (75-mu)/sigma
pnorm(z2)-pnorm(z1)

Q3  <- qnorm(0.75, mu, sigma)
Q1  <- qnorm(0.25, mu, sigma)
IQR <- Q3-Q1 
IQR

##3. Mean and variance of linear transformations ---------------------------------------------

a <- 5 
b <- 2 
x <- rnorm(100,1,2)
y <- a+b*x
par(mfrow=c(1,2))
hist(x)
hist(y)

#
mean(x)
mean(y)
a+2*mean(x)

#
var(x)
var(y)
b^2*var(x)

##4. compare theoretical distribution and data: p.d.f., c.d.f., qqplot -----------------------
## p.d.f
set.seed(123)
x<-rnorm(1000)
hist(x, freq=FALSE, breaks=30, xlab=" ",ylab="f(x)", main="") 
curve(dnorm(x), add=T, col=4, lwd=3)

# c.f.d
set.seed(123)
ecdf_norm <- ecdf(rnorm(150))
tt <- seq(from=-3, to=3, by=.01)
plot(ecdf_norm, verticals=TRUE, do.p=FALSE,main="ECDF and CDF",
     xlim=c(-2.5,6))
lines(tt, pnorm(tt), col=2, lty=2, lwd=2)

# what if we apply the log transformation to the rv?
set.seed(123)
ecdf_norm <- ecdf(exp(rnorm(150))) #lognormal
tt <- seq(from=-3, to=6, by=.01)
plot(ecdf_norm, verticals=TRUE, do.p=FALSE,main="ECDF and CDF",
     xlim=c(-2.5,6))
lines(tt, pnorm(tt), col=2, lty=2, lwd=2)

hist(rnorm(150))
hist(exp(rnorm(150)))

# qqplot: compare quantiles of the theoretical distribution and
# quantiles of the ecdf
?qqplot()
set.seed(123)
qqnorm(exp(rnorm(150)), ylim=c(-2,15))
abline(a=0,b=1)
qqnorm(rnorm(150)) 
abline(a=0,b=1)

qqnorm(rnorm(1500)) 
abline(a=0,b=1)

##5. comparison of the normal, t and cauchy distributions
par(mfrow=c(2,2))
qqnorm(rt(1000,1))
abline(a=0,b=1)

qqnorm(rt(1000,4))
abline(a=0,b=1)

qqnorm(rt(1000,10))
abline(a=0,b=1)

qqnorm(rt(1000,1000))
abline(a=0,b=1)

#hist
xx <- seq(-5, 5, l=1000)
plot(xx, dnorm(xx, 0, 1), xlab ="x", ylab ="f(x)", type ="l") 
for(i in c(50,25,5,1)) lines(xx, dt(xx, i), col = (i+1))


#well done! go back to the slides 

#############################
## Bivariate distributions ##
#############################

## Perspective plot
# bivariate normal density with mu_x=mu_y=0 and sigma_x=sigma_y=1
# it is a function of the correlation coeff rho
BivNorm <- function(x, y, rho){
  a <- (2*pi*sqrt(1-rho^2))^(-1) 
  a*exp(-.5*(1)*(1-rho^2)^(-1)*(x^2+y^2-2*rho*x*y))
}

#define a range for the axis
xx <- seq(-3, 3, 0.1)
yy <- seq(-3, 3, 0.1)

z <- outer(xx, yy, BivNorm, rho=0.5)
persp(xx, yy, z, theta=30, phi=30, xlab="x", 
      ylab="y", zlab="f(x,y)")

## Contour plot
par(mfrow=c(1,3))
rho<- -0.5
z<-outer(xx,yy, BivNorm, rho=rho)
contour(xx,yy,z,main="rho=-0.5")
rho<- 0
z<-outer(xx,yy,BivNorm, rho=rho)
contour(xx,yy,z, main="rho=0")
rho<- 0.9
z<-outer(xx,yy,BivNorm, rho=rho)
contour(xx,yy,z,main="rho=0.9")


## Sum of random variables: the sum of 10 iid rv following an exponential distr
## with parameter .1 is distributed as...?
nsim <- 1000 
n <- 10
x <- c()
for (i in 1:nsim){
  x[i] <-sum(rexp(n,.1)) 
  }

hist(x, prob=TRUE, ylim=c(0,.015), ylab="f(x)",
     main="", breaks=30, cex.main=.9) 
curve(dgamma(x, 10, .1), type="l", col=2, add=T, lwd=2)

### the sum of 10 iid Z^2, Z~N(0,1) is distributed as...?
nsim <- 1000 #iterations
n <-10

for (i in 1:nsim){
  x[i] <-sum(rnorm(n)^2) 
}

hist(x, prob=TRUE, ylim=c(0,.15), ylab="f(x)",
     main="", breaks=30, cex.main=.9) 
curve(dchisq(x, 10), type="l", col=2, add=T, lwd=2)

#well done! go back to the slides 





