d = read.csv("femaleControlsPopulation.csv")
data = d$Bodyweight
hist(data)
plot(ecdf(data))

ps = seq(0.01, .99, by = 0.01)
qs = quantile(data, ps)

# calculate normal distribution percentiles
normalQs = qnorm(ps, mean(data), sd(data))
plot(normalQs, qs, xlab='normal percentiles', ylab='weight')
abline(0,1)
# use qqnorm and qqline
qqnorm(qs)
qqline(qs)
# use qqplot
qqplot(normalQs, qs)
abline(0,1)

# skewed data
library(downloader)
download('http://courses.edx.org/c4x/HarvardX/PH525.1x/asset/skew.RData', 'skew.RData')
load("skew.RData")
par(mfrow = c(3,3))
for(i in 1:9){
  title = paste0('Normal QQ plot for column', i)
  qqnorm(dat[,i], main=title)
}
par(mfrow=c(2,1))
hist(dat[,4]) # positive skew, long right tail
hist(dat[,9]) # negative skew, long left tail

# boxplot
par(mfrow=c(1,1))
boxplot(InsectSprays$count ~ InsectSprays$spray)
library(UsingR)
data(nym.2002, package='UsingR')
boxplot(nym.2002$time ~ nym.2002$gender)
par(mfrow=c(2,1))
hist(nym.2002$time[nym.2002$gender=='Male'])
hist(nym.2002$time[nym.2002$gender=='Female'])

# scatterplot
male = nym.2002[nym.2002$gender == 'Male',]
female = nym.2002[nym.2002$gender == 'Female',]
cor(male$age, male$time) #0.243
cor(female$age, female$time) #0.244
par(mfrow= c(2,1))
boxplot(split(male$time, round(male$age / 5) * 5), main = "male time vs age group")
boxplot(split(female$time, round(female$age / 5) * 5), main = "female time vs age group", xlab="age", ylab="time")
a = tapply(male$time, round(male$age /5)* 5, mean)
ageGroup = as.numeric(names(a))
plot(ageGroup, a, main = 'male')
b = tapply(female$time, round(female$age / 5) * 5, mean)
ageGroup = as.numeric(names(b))
plot(ageGroup, b, main= 'female')

# symmetry of log ratios
time = sort(nym.2002$time)
par(mfrow=c(2,1))
plot(time/median(time), ylim=c(1/4, 4))
abline(h=c(1/2, 1, 2))
plot(log2(time/median(time)), ylim=c(-1.5, 1.5))
abline(h=-1:1)
