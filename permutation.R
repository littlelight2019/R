babies <- read.table('https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt', header=TRUE)
bwt.nonsmoke <-babies[babies$smoke == 0, 'bwt']
bwt.smoke <-babies[babies$smoke == 1, 'bwt']

# observed difference in mean
N=10
set.seed(1, sample.kind='Rounding')
nonsmokers <- sample(bwt.nonsmoke, N)
smokers <-sample(bwt.smoke, N)
obs <- mean(nonsmokers) - mean(smokers)

dat <- c(smokers, nonsmokers)
# permute 
permute <- function(N){
  shuffle <- sample(dat)
  smokersSet = shuffle[1:N]
  nonsmokersSet = shuffle[(N+1):(2*N)]
  mean(nonsmokersSet) - mean(smokersSet)
}
set.seed(1, sample.kind='Rounding')
permutes = replicate(1000, permute(N))
mean(abs(permutes) >= obs) #0.056; to avoid p-val of 0, add 1 to both numerator and denominator as in the next exercise

# repeat experiment with median
obsMedian <- median(nonsmokers) - median(smokers)
permuteMedian <- function(N){
  shuffle <- sample(dat)
  smokersSet <- shuffle[1:N]
  nonsmokersSet <- shuffle[(N+1): (2*N)]
  median(nonsmokersSet) - median(smokersSet)
}
set.seed(1, sample.kind = 'Rounding')
nulls = replicate(1000, permuteMedian(N))
(sum(abs(nulls) >= abs(obsMedian)) + 1) / (length(nulls) + 1) #0.029