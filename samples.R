# one observation of two samples
url <-  "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
dat = read.csv(url)
controls <- filter(dat, Diet=='chow') %>% select(Bodyweight) %>% unlist
treatment <- filter(dat, Diet == 'hf') %>% select(Bodyweight) %>% unlist
obs = mean(treatment) - mean(controls)
t.test(treatment, controls)

# sampling from population
dat = read.csv('https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv')
hff = dat[dat$Diet == 'hf' & dat$Sex == 'F', 'Bodyweight']
controlf = dat[dat$Diet == 'chow' & dat$Sex == 'F', 'Bodyweight']

# whether a sample rejects null
reject <- function(N, alpha = 0.05){
  treatment = sample(hff, N)
  control = sample(controlf, N)
  pval = t.test(treatment, control)$p.val
  pval < alpha
}

# repeat sampling for B times and apply the same experiments to different sample sizes N, calculate power
B = 2000; 
Nseq = seq(5, 50, 5)
power = sapply(Nseq, function(N) {r = replicate(B, reject(N)); mean(r)})
plot(Nseq, power, main='Power by sample size', xlab='Sample size')
lines(Nseq, power)

# Monte Carlo simulation of  p-values under null 
pop1 = rnorm(1000)
pop2 = rnorm(1000)
pval <- function(N){
  s1 = sample(pop1, N)
  s2 = sample(pop2, N)
  t.test(s1, s2)$p.val
}
pvals = replicate(B, pval(25))
hist(pvals)

# tstat simulations
tstat <- function(N){
  s = rnorm(N)
  mean(s) / sd(s) * sqrt(N)
}
qqcheck <- function(B=2000, N){
  tstats = replicate(B, tstat(N))
  qqnorm(tstats)
  abline(0,1)
}
sapply(seq(5, 50, 5), qqcheck)





# large tstat simulation: tstat > 2 vs what's in t-distribution
tstatl <- function(N, cutoff = 2){
  s = rnorm(N)
  tstat = mean(s) * sqrt(N) / sd(s)
  tstat > 2
}
MCportion <- function(N, cutoff=2){
  large = replicate(B, tstatl(N))
  mean(large)
}
Ns = seq(5, 50, 5)
MCportions = sapply(Ns, MCportion)
Tportions = sapply(Ns, function(N) 1-pt(2, N-1))
plot(Tportions, MCportions, ylim=c(0.02,0.07), xlim=c(0.02,0.07))
abline(0,1)
names(MCportions) <- Ns
MCportions


# chance of getting a 6 from 1:100 dice: abs(Z) > 2
m = 100; N = 100
p = 1/m
se = sqrt(p * (1-p) / N)
sampleZ <- function(){
  x = sample(1:m, N, replace=TRUE)
  (mean(x== 6) -p)/ se
}
set.seed(1, sample.kind='Rounding')
Zs = replicate(10000, sampleZ())
mean(abs(Zs) > 2)

qqnorm(Zs)
abline(0,1)

