
## ----weibull_log, echo=TRUE-----------------------------------------------------------------
# log-likelihood function
log_lik_weibull <- function( data, param){
  -sum(dweibull(data, shape = param[1], scale = param[2], log = TRUE))
}



## ----weibull_log_2, echo=FALSE--------------------------------------------------------------
y <- c(155.9, 200.2, 143.8, 150.1,152.1, 142.2, 147, 146, 146,
       170.3, 148, 140, 118, 144, 97)
n <- length(y)

#define parameters grid
gamma <- seq(0.1, 15, length=100)
beta <- seq(100,200, length=100)
parvalues <- expand.grid(gamma,beta)
llikvalues <- apply(parvalues, 1, log_lik_weibull, data=y)
llikvalues <- matrix(-llikvalues, nrow=length(gamma), ncol=length(beta),
                     byrow=F)
conf.levels <- c(0,0.5,0.75,0.9,0.95,0.99)

#contour plot
contour(gamma, beta, llikvalues-max(llikvalues),
        levels=-qchisq(conf.levels, 2)/2,
        xlab=expression(gamma),
        labels=as.character(conf.levels),
        ylab=expression(beta), cex.lab=2)
title('Weibull relative log likelihood')

#image
image(gamma,beta,llikvalues-max(llikvalues),zlim=c(-6,0),
      col=terrain.colors(20),xlab=expression(gamma),
      ylab=expression(beta), cex.lab=2)
title('Weibull relative log likelihood')




## ----weibull_parameters, echo=TRUE----------------------------------------------------------

gammahat<-uniroot(function(x) n/x+sum(log(y))-n*
                    sum(y^x*log(y))/sum(y^x),
                  c(1e-5,15))$root
betahat<- mean(y^gammahat)^(1/gammahat)
weib.y.mle<-c(gammahat,betahat)
#first element is the MLE for the shape gamma, second element the MLE for the scale beta
weib.y.mle





## ----weibull_parameters_2, echo=FALSE, results="hide"---------------------------------------

#observed information matrix
jhat<-matrix(NA,nrow=2,ncol=2)
jhat[1,1]<-n/gammahat^2+sum((y/betahat)^gammahat*
                              (log(y/betahat))^2)
jhat[1,2]<-jhat[2,1]<- n/betahat-sum(y^gammahat/betahat^(gammahat+1)*
                                       (gammahat*log(y/betahat)+1))
jhat[2,2]<- -n*gammahat/betahat^2+gammahat*(gammahat+1)/
  betahat^(gammahat+2)*sum(y^gammahat)
solve(jhat)

#se of the mle
mle.se<-sqrt(diag(solve(jhat)))
mle.se




## ----weibull_numerical, echo=TRUE-----------------------------------------------------------
weib.y.nlm<-nlm(log_lik_weibull,c(0,0),hessian=T,data=y)


## ----weibull_reparam, echo=TRUE-------------------------------------------------------------

omega <- function(theta) log(theta)
theta <- function(omega) exp(omega)
log_lik_weibull_rep <- function(data, param) log_lik_weibull(data, theta(param))
weib.y.nlm<-nlm(log_lik_weibull_rep,c(0,0),hessian=T,data=y)
weib.y.nlm
theta(weib.y.nlm$estimate)
weib.y.mle



## ----weibull_profile, echo=TRUE-------------------------------------------------------------
weib.y.mle<-optim(c(1,1),fn=log_lik_weibull,hessian=T,
                  method='L-BFGS-B',lower=rep(1e-7,2),
                  upper=rep(Inf,2),data=y)
gamma <- seq(0.1, 15, length=100)
beta <- seq(100,200, length=100)
parvalues <- expand.grid(gamma,beta)
llikvalues <- apply(parvalues, 1, log_lik_weibull, data=y)
llikvalues <- matrix(-llikvalues, nrow=length(gamma), ncol=length(beta),
                     byrow=F)
conf.levels <- c(0,0.5,0.75,0.9,0.95,0.99)

#contour plot
contour(gamma, beta, llikvalues-max(llikvalues),
        levels=-qchisq(conf.levels, 2)/2,
        xlab=expression(gamma),
        labels=as.character(conf.levels),
        ylab=expression(beta))
title('Weibull profile log-likelihood')

beta.gamma<- sapply(gamma,function(x) mean(y^x)^(1/x))
lines(gamma, beta.gamma, lty='dashed',col=2)
points(weib.y.mle$par[1],weib.y.mle$par[2])


## ----weibull_interval, echo=TRUE------------------------------------------------------------
log_lik_weibull_profile  <- function(data, gamma){
  beta.gamma <- mean(data^gamma)^(1/gamma)
  log_lik_weibull( data, c(gamma, beta.gamma) )
}

log_lik_weibull_profile_v <-Vectorize(log_lik_weibull_profile, 'gamma'  )

plot(function(x) -log_lik_weibull_profile_v(data=y, x)+weib.y.mle$value,
     from=0.1,to=15,xlab=expression(gamma),
     ylab='profile relative log likelihood',ylim=c(-8,0))
conf.level<-0.95
abline(h=-qchisq(conf.level,1)/2,lty='dashed',col=2)

conf.level<-0.95
lrt.ci1<-uniroot(function(x) -log_lik_weibull_profile_v(y, x)+
                   weib.y.mle$value+
                   qchisq(conf.level,1)/2,
                 c(1e-7,weib.y.mle$par[1]))$root
lrt.ci1<-c(lrt.ci1,uniroot(function(x) -log_lik_weibull_profile_v(y,x)+
                             weib.y.mle$value+
                             qchisq(conf.level,1)/2,
                           c(weib.y.mle$par[1],15))$root)
segments( lrt.ci1[1],-qchisq(conf.level,1)/2, lrt.ci1[1],
          -log_lik_weibull_profile_v(y, lrt.ci1[1]), col="red", lty=2  )
segments( lrt.ci1[2],-qchisq(conf.level,1)/2, lrt.ci1[2],
          -log_lik_weibull_profile_v(y, lrt.ci1[2]), col="red", lty=2  )
points(lrt.ci1[1], -qchisq(0.95,1)/2, pch=16, col=2, cex=1.5)
points(lrt.ci1[2], -qchisq(0.95,1)/2, pch=16, col=2, cex=1.5)
segments( lrt.ci1[1],
          -8.1, lrt.ci1[2],
          -8.1, col="red", lty =1, lwd=2  )
text(7,-7.5,"95% Deviance CI",col=2)






## ----normal, echo=TRUE----------------------------------------------------------------------
#input values


#true mean
theta_sample <- 2
#likelihood variance
sigma2 <- 2
#sample size
n <- 10
#prior mean
mu <- 7
#prior variance
tau2 <- 2

#generate some data
set.seed(123)
y <- rnorm(n,theta_sample, sqrt(sigma2))

#posterior mean
mu_star <- ((1/tau2)*mu+(n/sigma2)*mean(y))/( (1/tau2)+(n/sigma2))
#posterior standard deviation
sd_star <- sqrt(1/( (1/tau2)+(n/sigma2)))


curve(dnorm(x, theta_sample, sqrt(sigma2/n)),xlim=c(-4,15), lty=2, lwd=1, col="black", ylim=c(0,1.4), 
      ylab="density", xlab=expression(theta))

curve(dnorm(x, mu, sqrt(tau2) ), xlim=c(-4,15), col="red", lty=1,lwd=2,  add =T)
curve(dnorm(x, mu_star, sd_star), 
      xlab=expression(theta), ylab="", col="blue", lwd=2, add=T)  
legend(8.5, 0.7, c("Prior", "Likelihood", "Posterior"), 
       c("red", "black", "blue", "grey" ), lty=c(1,2,1),lwd=c(1,1,2), cex=1)


## ----normal_stan, echo=TRUE-----------------------------------------------------------------

library(rstan)
#launch Stan model
data<- list(N=n, y=y, sigma =sqrt(sigma2), mu = mu, tau = sqrt(tau2))
fit <- stan(file="normal.stan", data = data, chains = 4, iter=2000)
#extract Stan output
sim <- extract(fit)

#draw the true analytical posterior and the Stan simulated posterior

par(mfrow=c(1,2), pty ="m", oma=c(0,0,0,0))
curve(dnorm(x, theta_sample, sqrt(sigma2/n)),xlim=c(-4,15), lty=2, lwd=1, col="black", ylim=c(0,1.2), 
      ylab="density", xlab=expression(theta), cex.lab=2)

curve(dnorm(x, mu, sqrt(tau2) ), xlim=c(-4,15), col="red", lty=1,lwd=2,  add =T)
curve(dnorm(x, mu_star, sd_star), 
      xlab=expression(theta), ylab="", col="blue", lwd=2,
      cex.lab=2, add=T)  

legend(5, 1, c("Prior", "Likelihood", "True Posterior"), 
       c("red", "black", "blue", "blue" ), lty=c(1,2,1),lwd=c(1,1,2), cex=0.8)


curve(dnorm(x, theta_sample, sqrt(sigma2/n)),xlim=c(-4,15), lty=2, lwd=1, col="black", ylim=c(0,1.2), 
      ylab="density", xlab=expression(theta), cex.lab=2)
curve(dnorm(x, mu, sqrt(tau2) ), xlim=c(-4,15), col="red", lty=1,lwd=2,  add =T)
lines(density(sim$theta, adj=2), col ="blue", lwd=2, lty =1)
legend(5, 1, c("Prior", "Likelihood", "Stan Posterior"),
       c("red", "black", "blue", "blue" ), lty=c(1,2,1),lwd=c(1,1,2), cex=0.8)



## ----bayesplot, echo=TRUE-------------------------------------------------------------------
posterior <- as.array(fit)


## ----normal_stan_unif, warning=FALSE, message = FALSE, results='hide', echo=TRUE------------

#launch Stan model with the uniform prior in place of normal prior

data2<- list(N=n, y=y, sigma =sqrt(sigma2), a=-10, b=10)
fit2 <- stan(file="normal_uniform.stan", data = data2, chains = 4, iter=2000,
             refresh=-1)

#extract the Stan output
sim2 <- extract(fit2)

#plot the Stan posterior

curve(dnorm(x, theta_sample, sqrt(sigma2/n)),xlim=c(-12,12), lty=2, lwd=1, col="black", ylim=c(0,1.2), 
      ylab="density", xlab=expression(theta))
curve(dunif(x, -10,10  ), xlim=c(-12,12), col="red", lty=1,lwd=2,  add =T)
lines(density(sim2$theta, adj=2), col ="blue", lwd=2, lty =1)
legend(5, 1, c("Prior", "Likelihood", "Stan Posterior"), 
       c("red", "black", "blue", "blue" ), lty=c(1,2,1),lwd=c(1,1,2), cex=0.8)



## ----traceplot,  warning=FALSE, message = FALSE, results='hide', echo =TRUE-----------------

data2<- list(N=n, y=y, sigma =sqrt(sigma2), a=-10, b=10)
fit2 <- stan(file="normal_uniform.stan", data = data2, chains = 4, iter=2000,
             refresh=-1)
sim2 <- extract(fit2)

#traceplot
traceplot(fit2, pars ="theta")
theta_est <- mean(sim2$theta)
theta_est




## ----areas, echo =TRUE----------------------------------------------------------------------
library("bayesplot")
library("rstanarm")
library("ggplot2")



#MCMC areas
posterior <- as.matrix(fit2)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")

mcmc_areas(posterior, 
           pars = "theta", 
           prob = 0.8) + plot_title




## ----bipar, warning=FALSE, message = FALSE, results='hide', echo=TRUE-----------------------

#launch biparametric Stan model

data3<- list(N=n, y=y, a=-10, b=10)
fit3 <- stan(file="biparametric.stan", data = data3, chains = 4, iter=2000,
             refresh=-1)

#extract stan output for biparametric model

sim3 <- extract(fit3)
posterior_biv <- as.matrix(fit3)

theta_est <- mean(sim3$theta)
sigma_est <- mean(sim3$sigma)
c(theta_est, sigma_est)
traceplot(fit3, pars=c("theta", "sigma"))

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")

mcmc_areas(posterior_biv, 
           pars = c("theta","sigma"), 
           prob = 0.8) + plot_title





## ----testing, warning=FALSE, message = FALSE, results='hide', echo=TRUE---------------------


library(LearnBayes)
data(soccergoals)

y <- soccergoals$goals

#write the likelihood function via the gamma distribution


lik_pois<- function(data, theta){
  n <- length(data)
  lambda <- exp(theta)
  dgamma(lambda, shape =sum(data)+1, scale=1/n)
}

prior_gamma <- function(par, theta){
  lambda <- exp(theta)
  dgamma(lambda, par[1], rate=par[2])*lambda  
}

prior_norm <- function(npar, theta){
  lambda=exp(theta)  
  (dnorm(theta, npar[1], npar[2]))
  
}

lik_pois_v <- Vectorize(lik_pois, "theta")
prior_gamma_v <- Vectorize(prior_gamma, "theta")
prior_norm_v <- Vectorize(prior_norm, "theta")


#likelihood
curve(lik_pois_v(theta=x, data=y), xlim=c(-1,4), xlab=expression(theta), ylab = "density", lwd =2 )
#prior 1
curve(prior_gamma_v(theta=x, par=c(4.57, 1.43)), lty =2, col="red", add = TRUE, lwd =2)
#prior 2 
curve(prior_norm_v(theta=x, npar=c(1, .5)), lty =3, col="blue", add =TRUE, lwd=2)
#prior 3 
curve(prior_norm_v(theta=x, npar=c(2, .5)), lty =4, col="green", add =TRUE, lwd =2)
#prior 4 
curve(prior_norm_v(theta=x, npar=c(1, 2)), lty =5, col="violet", add =TRUE, lwd =2)
legend(2.6, 1.8, c("Lik.", "Ga(4.57,1.43)", "N(1, 0.25)", "N(2,0.25)","N(1, 4)" ),
       lty=c(1,2,3,4,5), col=c("black", "red", "blue", "green", "violet"),lwd=2, cex=0.9)


## ----testing_2, warning=FALSE, message = FALSE, results='hide', echo=TRUE-------------------

logpoissongamma <- function(theta, datapar){
  data <- datapar$data
  par <- datapar$par
  lambda <- exp(theta)
  log_lik <- log(lik_pois(data, theta))
  log_prior <- log(prior_gamma(par, theta))
  return(log_lik+log_prior)
}

logpoissongamma.v <- Vectorize( logpoissongamma, "theta")


logpoissonnormal <- function( theta, datapar){
  data <- datapar$data
  npar <- datapar$par
  lambda <- exp(theta)
  log_lik <- log(lik_pois(data, theta))
  log_prior <- log(prior_norm(npar, theta))
  return(log_lik+log_prior)
}  
logpoissonnormal.v <- Vectorize( logpoissonnormal, "theta")

#log-likelihood
curve(log(lik_pois(y, theta=x)), xlim=c(-1,4),ylim=c(-20,2), lty =1,
      ylab="log-posteriors", xlab=expression(theta))
#log posterior 1
curve(logpoissongamma.v(theta=x, list(data=y, par=c(4.57, 1.43))), col="red", xlim=c(-1,4),ylim=c(-20,2), lty =1, add =TRUE)
#log posterior 2
curve(logpoissonnormal.v( theta=x, datapar <- list(data=y, par=c(1, .5))), lty =1, col="blue",  add =TRUE)
#log posterior 3
curve(logpoissonnormal.v( theta=x, datapar <- list(data=y, par=c(2, .5))), lty =1, col="green", add =TRUE, lwd =2)
#log posterior 4
curve(logpoissonnormal.v( theta=x, list(data=y, par=c(1, 2))), lty =1, col="violet", add =TRUE, lwd =2)
legend(2.6, 1.3, c( "loglik", "lpost 1", "lpost 2", "lpost 3", "lpost 4" ),
       lty=1, col=c("black", "red", "blue", "green", "violet"),lwd=2, cex=0.9)


## ----testing_3, warning=FALSE, message = FALSE, echo=TRUE-----------------------------------
datapar <- list(data=y, par=c(4.57, 1.43))
fit1 <- laplace(logpoissongamma, .5, datapar)
datapar <- list(data=y, par=c(1, .5))
fit2 <- laplace(logpoissonnormal, .5, datapar)
datapar <- list(data=y, par=c(2, .5))
fit3 <- laplace(logpoissonnormal, .5, datapar)
datapar <- list(data=y, par=c(1, 2))
fit4 <- laplace(logpoissonnormal, .5, datapar)

postmode <- c(fit1$mode, fit2$mode, fit3$mode, fit4$mode )
postsds <- sqrt(c(fit1$var, fit2$var, fit3$var, fit4$var))
logmarg <- c(fit1$int, fit2$int, fit3$int, fit4$int)
cbind(postmode, postsds, logmarg)


## ----testing_4, warning=FALSE, message = FALSE, echo=TRUE-----------------------------------
BF_matrix <- matrix(1, 4,4)
for (i in 1:3){
  for (j in 2:4){
    BF_matrix[i,j]<- exp(logmarg[i]-logmarg[j])
    BF_matrix[j,i]=(1/BF_matrix[i,j]) 
  }
}

round_bf <- round(BF_matrix,3)
round_bf



