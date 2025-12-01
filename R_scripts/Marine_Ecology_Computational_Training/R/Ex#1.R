install.packages("vegan")
library(vegan)

# Simple operations in R
# First, create a data vector
a = c(10,2,19,24,6,23,47,24,54,77)
# Now compute some simple formulas
mediavettore=mean(a) # calculates the arithmetic mean
medianvettore=median(a) # calculates the median
var(a) # calculates the variance
sd(a) # calculates the standard deviation
max(a) # returns the largest value in the sequence
min(a) # returns the smallest value in the sequence
sum(a) # sums all values
sum(a)*sum(a) # calculates the square of the sum of all values
sum(a*a) # calculates the sum of the squares of all values
sum(rank(a)) # sum of the ranks assigned to the variables contained in a
sum(rank(a)[1:6]) # sum of the ranks assigned to the first 6 values of a

# One-sample Student's t test
# Comparison of the sample mean with a known value, population variance unknown.
t.test(a)

# Two-sample t test #1
# Comparison of the means of two groups of independent samples
# drawn from two populations with unknown variance, homogeneous sample variances.

# To solve this problem we apply a two-sample Student's t test,
# assuming the two samples are drawn from populations that follow a
# Gaussian distribution (if this assumption is not reasonable, use
# the nonparametric Wilcoxon-Mann-Whitney test). Before running the t test,
# it is necessary to assess the sample variances of the two groups, i.e., perform
# Fisher’s F test to verify homoscedasticity (homogeneity of variances).

a = c(175, 168, 168, 190, 156, 181, 182, 175, 174, 179)
b = c(185, 169, 173, 173, 188, 186, 175, 174, 179, 180)
var.test(a,b)

t.test(a,b)

# Suppose we want to study whether there is a correlation between 2 data series.
# We use Pearson's R test, which returns how "strong" the correlation is
# and the t test value to assess the significance of Pearson's R coefficient.
cor.test(a,b)
# Regression analysis is used when we want to derive a statistical model from sample data
# that predicts the values of a dependent variable (Y) from the values of an independent variable (X).
height = c(175, 168, 170, 171, 169, 165, 165, 160, 180, 186)
weight = c(80, 68, 72, 75, 70, 65, 62, 60, 85, 90)
model = lm(formula = height ~ weight) # I'll answer tomorrow
model
summary(model)
model2 = lm(formula = height ~ weight, x=TRUE, y=TRUE) # I'll answer tomorrow
summary(model2)

par(pty="s", mfrow=c(1,2))
plot(height ~ weight, pch=19, las=1, col="black")
plot(height ~ weight, pch=19, las=1, col="black")
abline(model)

pippo = cbind(a,a,a,a)
pippo[,2:3]
pippo[1:7,2:3]
