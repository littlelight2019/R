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

# median, MAD, spearman correlation
data("ChickWeight")
plot(ChickWeight$Time, ChickWeight$weight, col = ChickWeight$Diet)
chick = reshape(ChickWeight, idvar = c("Chick", "Diet"), timevar = "Time", direction = "wide")
chick = na.omit(chick)
mean(c(3000, chick$weight.4)) / mean(chick$weight.4)
median(c(3000, chick$weight.4)) / median(chick$weight.4)
sd(c(3000, chick$weight.4)) / sd(chick$weight.4)
mad(c(3000, chick$weight.4)) / mad(chick$weight.4)
cor(chick$weight.4, chick$weight.21)
cor(chick$weight.4, chick$weight.21, method='spearman')
cor(c(3000, chick$weight.4), c(3000, chick$weight.21)) / cor(chick$weight.4, chick$weight.21)
cor(c(3000, chick$weight.4), c(3000, chick$weight.21), method='spearman') / cor(c(3000, chick$weight.4), c(3000, chick$weight.21), method='spearman')

# Mann-Whitney-Wilcoxon test
x = chick$weight.4[chick$Diet == 1]
y = chick$weight.4[chick$Diet == 4]
t.test(x, y)
wilcox.test(x, y)
# wilcox.test is not affected by one outlier added while t.test is
t.test(c(200, x), y)$p.val
wilcox.test(c(200, x), y)$p.val
# wilcox is insensitive to magnitude of difference once two groups are completely separated
par(mfrow=c(1,3))
boxplot(x,y)
boxplot(x, y+10)
boxplot(x, y+100)
t.test(x, y+10)$statistic - t.test(x, y+100)$statistic #68
wilcox.test(x,y+10)$statistic - wilcox.test(x, y+100)$statistic #0
