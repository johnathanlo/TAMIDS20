######MLE functions######
dDelays <- function(x, para){
  p = para[1]; mu = para[2]; sd = para[3]; rate = para[4];
  if(x>0){
    return(p*dexp(x, rate = rate) + (1-p)*dnorm(x, mean = mu, sd = sd))
  }else{
    return((1-p)*dnorm(x, mean = mu , sd = sd))
  }
}

loglikDelays <- function(x, para){###para = c(p, mu, sd, rate)
  p = para[1]; mu = para[2]; sd = para[3]; rate = para[4];
  likelihoods <-sapply(x,FUN = dDelays, para = para)
  return(-sum(log(likelihoods)))
}

MLE_BDEDIST <- function(x){
  return(optim(c(.5, -10, 5, 1/24), loglikDelays, x=x)) 
}

PLOT_BDE <- function(par, n, data){
  bernvec <- rbinom(n,1,par[1])
  modelvec <- bernvec*rexp(n,par[4]) + (1-bernvec)*rnorm(n,par[2], par[3])
  par(mfrow = c(2,1))
  hist(modelvec, breaks = 100, xlim = c(-200,200), main = "Histogram of simulated values")
  hist(data, breaks = 200, xlim = c(-200,200))
}

optimFun <- function(x, para){
  return(-sum(dgumbel(x,loc=para[1],scale = para[2],log=TRUE)))
}
