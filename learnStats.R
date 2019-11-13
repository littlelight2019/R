#1.1 binomial distribution same as function dbinomial(x, n, p)
binomialP <- function(x, n, p){
  choose(n, x) * p^x * (1-p) ^ (n-x)
}
sum = 0 #probability of having 4 or less correct out of 12 multiple of 5 choices if choose by rondom
for(x in 0:4){
  p = binomialP(x, 12, 0.2)
  sum = sum + p
  print(paste('sum=', sum, '; p=', p))
}
print(sum)

#1.2. poisson distribution 
poissonP <- function(x, lambda){
  lambda^(x) * exp(-lambda) / factorial(x)
}
#man 12 cars crossing bridge per minute. what's the probability of having 17+ cars crossing the bridge for a given min?
sum = 0
for(i in 0:16){
  sum = sum + poissonP(i, 12)
}
1-sum #same as 1-ppois(6, lambda=12) or ppois(16, lambda=12, lower=FALSE)

#1.3. uniform distribution
#select 10 random numbers between one and three
runif(10,1,3)

#1.4. exponential distribution
# mean check out time is 3 min. what's the probability for a customer to finish check out within 2 min.
pexp(2, rate= 1/3)

#1.5. normal distribution
# what's the percentage of students scoring 84 or more if test scores fit a normal distribution of mean 72 and sd 5.2
pnorm(84, mean=72, sd=15.2, lower.tail = FALSE)

#1.6. Chi-squared distribution
# find the 95% of the Chi-squared distribution with 7 degrees of freedom
qchisq(p=.95, df = 7)

#1.7. Student t distribution
# find the 2.5th and 97.5th percentiles of the Student t distribution with 5 degrees of freedom
qt(c(.025, .975), df=5) #(-2.5706  2.5706)

#1.8. F distribution
# find the 95% percentile of the F distribution with (5,2) degrees of freedom
qf(.95, df1=5, df2=2) #19.296

#2.1 interval estimate of population mean with known variance
# MASS package Height: assume population standard deviation (sigma) as 9.48, 
# find margin of error and interval estimate at 95% confidence interval
# C.I. = mean +/- z(alpha/2) * sigma/sqrt(n), where sigma/sqrt(n) is standard error of mean, 

z = qnorm(.975); z
sem = 9.48/sqrt(length(na.omit(survey$Height))); sem # standard error of the mean
E = z * sem; E # error of margin
CI = mean(survey$Height, na.rm = TRUE) + c(-sem, sem); CI # confidence interval


#2.2 interval estimate of population mean with unknown variance
# find the margin of error and interval estimate at 95% confidence interval
# for the survey$Height
# mean +/- t(alpha/2) * s /sqrt(n), where s is standard deviation of the sample

t = qt(.975, df = length(na.omit(survey$Height)) - 1)
SE = sd(survey$Height, na.rm = TRUE) / sqrt(length(na.omit(survey$Height))) #standard error estimate
me = t * SE; me #margin of error
CI = mean(survey$Height, na.rm = TRUE) + c(-me, me); CI

# alternate solution
t.test(na.omit(survey$Height))

#2.3 sampling size needed under the requirement of population mean interval estimate at 1-alpha confidence level
# n = z(alpha/2) ^2 * sigma ^2 / E ^2 where z(alpha/2) is qnormal(1-alpha/2), 
# sigma^2 is population variance, and E is margin of error.
# assume population standard deviation of student height in survey is 9.48, find the sample size needed to achieve
# a 1.2 centimeters margin of error at 95% confidence level.

z = qnorm(.975); z
E = 1.2
sigma = 9.48
n = (z * sigma / E) ^ 2; n

#2.4 interval estimate of population proportion
