# get a sample of size N from normal distribution, calculate t-stat
tstat <- function(N){
  s = rnorm(N)
  mean(s)/sqrt(var(s)/N)
}

# get the portion larger than 2 when N==5
set.seed(1, sample.kind = 'Rounding')
tstats = replicate(1000, tstat(5))
mean(tstats > 2)



# repeat sampling with different Ns, compare with t-distribution and qqnorm
library(rafalib)
mypar(5,2)
for(N in seq(5, 50, 5)){
  B = 1000
  tstats = replicate(B, tstat(N))
  
  B = 100
  ps = seq(1/(B+1), B/(B+1), len=B)
  ts = qt(ps, df=N-1)
  LIM = c(-4.5, 4.5)
  
  title = paste0("t-distribution for N=",N)
  qqplot(ts, tstats, main=title, xlab = 't-quantiles', ylab = 'sample-quantiles', xlim=LIM, ylim = LIM)
  abline(0,1)
  
  # title = paste0("qqnorm for N=",N)
  # qqnorm(tstats, main=title, xlim=LIM, ylim = LIM)
  # abline(0,1)

}

# MC for two sample tstats, compare with t-distribution and qqnorm
tstat2 <- function(N){
  s1 = rnorm(N)
  s2 = rnorm(N)
  (mean(s1) - mean(s2)) / sqrt(var(s1)/N + var(s2)/N)
}

MCvsT <- function(N){
  # MC tstats
  B = 2000
  tstats2 = replicate(B, tstat2(N))
  # from t-distribution
  ps = seq(1/(B+1), B/(B+1), len=B)
  Ts = qt(ps, df=2*N-2)
  LIM = c(-3.5, 3.5)
  
  # title=paste0("Two sample MC t-stats vs t-distribution N=", N)
  # qqplot(Ts, tstats2, main=title, xlim=LIM, ylim=LIM)
  # abline(0,1)
  
  title = paste0("Two sample MC t-stats vs normal dist N=", N)
  qqnorm(tstats2, main=title, xlim=LIM, ylim=LIM)
  abline(0,1)
}
mypar(5,2)
sapply(seq(5, 50, 5), MCvsT)

# sample is from binomial distribution instead of normal distribution
tstat3 <- function(N){
  s = sample(c(-1,1), N, replace=TRUE)
  mean(s)/sqrt(var(s)/N)
}
B=2000
mypar(5,2)
binomialVSt <- function(N){
  tstats3 = replicate(B, tstat3(N))
  ps = seq(1/(1+B), B/(1+B), len=B)
  Ts = qt(ps, df=N-1)
  LIM = c(-1,4)
  
  title=paste0("binomial MC vs t-dist N=", N)
  qqplot(Ts, tstats3, main=title, xlim=LIM, ylim=LIM)
  abline(0,1)
  
  # title=paste0("binomial MC vs norm-dist N=", N)
  # qqnorm(tstats3, main=title, xlim=LIM, ylim=LIM)
  # abline(0,1)
}
sapply(seq(15, 1000, len=10), binomialVSt)

# sample median distribution (normal distribution with SD > 1/sqrt(N))
getMedian <- function(N){
  s = rnorm(N)
  median(s)
}
B = 2000; N = 30
ms = replicate(B, getMedian(N))
sd(ms)
qqnorm(ms)
qqline(ms)
