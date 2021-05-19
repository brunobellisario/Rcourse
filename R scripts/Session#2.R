#DATA ANALYSIS WITH R
#SESSION#2 - Descriptive statistics

#Transforming data
#The packages used in this chapter include:
#"car"
#"MASS"
#"rcompanion"
#The following commands will install these packages if they are not already installed:
if(!require(psych)){install.packages("car")}
if(!require(MASS)){install.packages("MASS")}
if(!require(rcompanion)){install.packages("rcompanion")}
#This example uses hypothetical data of river water turbidity.
#Turbidity is a measure of how cloudy water is due to suspended material in the water.
#Water quality parameters such as this are often naturally log-normally distributed:
#values are often low, but are occasionally high or very high.
Turbidity = c(1.0, 1.2, 1.1, 1.1, 2.4, 2.2, 2.6, 4.1, 5.0,
10.0, 4.0, 4.1, 4.2, 4.1, 5.1, 4.5, 5.0, 15.2, 10.0, 20.0,
1.1, 1.1, 1.2, 1.6, 2.2, 3.0, 4.0, 10.5)
plot(Turbidity,type="b")
plotNormalHistogram(Turbidity)#Plot distribution
#The first plot is a histogram of the Turbidity values, with a normal curve superimposed.
#Looking at the gray bars, this data is skewed strongly to the right (positive skew),
#and looks more or less log-normal. The gray bars deviate noticeably from the red normal curve.
qqnorm(Turbidity,
       ylab="Sample Quantiles for Turbidity")
qqline(Turbidity, 
       col="red")
#The second plot is a normal quantile plot (normal Q–Q plot).
#If the data were normally distributed, the points would follow the red line fairly closely.

#Square root transformation
#Since the data is right-skewed, we will apply common transformations for right-skewed data:
#square root, cube root, and log.
#The square root transformation improves the distribution of the data somewhat.
T_sqrt = sqrt(Turbidity)
plotNormalHistogram(T_sqrt)
#Cube root transformation
#The cube root transformation is stronger than the square root transformation.
T_cub = sign(Turbidity) * abs(Turbidity)^(1/3)   # Avoid complex numbers for some cube roots
plotNormalHistogram(T_cub)
#Log transformation
#The log transformation is a relatively strong transformation.
#Because certain measurements in nature are naturally log-normal,
#it is often a successful transformation for certain data sets.
#While the transformed data here does not follow a normal distribution very well,
#it is probably about as close as we can get with these particular data.
T_log = log(Turbidity)
plotNormalHistogram(T_log)

#Let's compare distributions
par(pty="s",mfrow=c(2,2))#The par() function allows customizing the plot:
#pty - is a parameter to force to plot to be square (s) or
#to use all available space in thr plot window (m)
#mfrow=c(rows,columns) set how to subdivide the plot area in rows and columns
plotNormalHistogram(Turbidity)
plotNormalHistogram(T_sqrt)
plotNormalHistogram(T_cub)
plotNormalHistogram(T_log)

#Preprocessing data. Normalization & Standardization##

#We’ll use the built-in R dataset iris to illustrate
#how to normalize or scale variables in R:
#The Iris flower data set or Fisher's Iris data set is a
#multivariate data set introduced by the British statistician,
#eugenicist and biologist Ronald Fisher in his 1936 paper
#The use of multiple measurements in taxonomic problems
#as an example of linear discriminant analysis
#The data set consists of 50 samples from each of three species of Iris
#(Iris setosa, Iris virginica and Iris versicolor).
#Four features were measured from each sample:
#the length and the width of the sepals and petals, in centimeters.

#view first six rows of iris dataset
head(iris)
#define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
#apply Min-Max normalization to first four columns in iris dataset
iris_norm <- as.data.frame(lapply(iris[1:4], min_max_norm))
#view first six rows of normalized iris dataset
head(iris_norm)
#standardize Sepal.Width
iris$Sepal.Width.std <- (iris$Sepal.Width - mean(iris$Sepal.Width)) / sd(iris$Sepal.Width)
head(iris)
#Let's have a look to the data
plotNormalHistogram(iris$Sepal.Width)
plotNormalHistogram(iris_norm$Sepal.Width)
plotNormalHistogram(iris$Sepal.Width.std)

#The values of Sepal.Width are now scaled such that the mean is 0
#and the standard deviation is 1. We can even verify this if we’d like:
#find mean of Sepal.Width.std
mean(iris$Sepal.Width.std)#basically 0
sd(iris$Sepal.Width.std)

#R has several built-in functions, as well as function in specific packages
#able to preprocess the data
#For instance, the function "scale()" 
iris_standardize <- as.data.frame(scale(iris[1:4]))#standardize first 4 columns
#we must specify which columns we want to standardize because the "scale" function
#by default is applied to each column. 
#REMEMBER THAT THE DATASET IS NOT HOMOGENOUS AS IT CONTAINS NON NUMERIC DATA
#THE FIELD SPECIES

#Let's introduce another useful package for statistical analysis, vegan,
library(vegan)
#The function "decostand()" provides some popular (and effective)
#standardization methods for community ecologists.
iris_norm_by_decostand=decostand(iris[1:4],"normalize")
iris_stdn_by_decostand=decostand(iris[1:4],"standardize")
#The values of Sepal.Width are now scaled such that the mean is 0
#and the standard deviation is 1. We can even verify this if we’d like:
mean(iris_stdn_by_decostand$Sepal.Width)
sd(iris_stdn_by_decostand$Sepal.Width)

#Central trend measures
#In R language, arithmetic mean can be calculated by mean() function
#Let's use the iris dataset and measure the mean of Sepal.Length
mean(iris$Sepal.Length)
#The same applies if we want to measure the mean of other iris characteristics
mean(iris$Sepal.Width)
#Median in statistics is another measure of central tendency which represents
#the middlemost value of a given set of values.
#In R language, median can be calculated by median() function.
median(iris$Sepal.Length)
median(iris$Sepal.Width)
#Note how mean and median for both sepal length and width are almost similar,
#reflecting the quasi-normal distribution of the data

#The mode of a given set of values is the value that is repeated most in the set.
#There can exist multiple mode values in case if there are two or more values with matching maximum
#frequency.
#Example 1: Single-mode value
#In R language, there is NO function to calculate mode.
#So, modifying the code to find out the mode for a given set of values.
# Generate frequency table
y <- table(iris$Sepal.Length)
# Print frequency table
print(y)
# Mode of x
m <- names(y)[which(y == max(y))]
# Print mode
print(m)
#Once again, the mode is almost coincident to both the mean and median
#further evidence of the quasi-normal distribution

#TIPS
#In R, if we want to apply a function, let's say the mean, over each column of a dataframe or
#matrix, we can use the function apply()
apply(iris[,1:4],2,mean)#Here, the first argument is the dataset,
#for which we have to specify only NUMERIC columns. The second argument is the MARGIN
#1 - rows; 2 - columns on which to apply the function.
#The third argument is the function. Can be a built-in function but also an ad-hoc function 
#created by us

#Measures of variance
#Range
#Range is the difference between maximum and minimum value of a data set.
#In R language, max() and min() is used to find the same, unlike range() function
#that returns the minimum and maximum value of data set.
range(iris$Sepal.Length)
(max(iris$Sepal.Length)-min(iris$Sepal.Length))
#Interquartile Range
#In R language, there is built-in function to calculate the interquartile range of data set.
IQR(iris$Sepal.Length)
#Standard Deviation
#Standard deviation in statistics measures the spreaness of data values with respect
#to mean and mathematically, is calculated as square root of variance.
#In R we can use the built-in function sd() to measure the standard deviation
sd(iris$Sepal.Length)#....or
#by using the square root of function var()
sqrt(var(iris$Sepal.Length))
#The five number of a dataset
#In R, we can visualize the distribution of a given variable as a boxplot, which
#give us information on:

#Lowest value
#Q1: 25th percentile
#Q2: the median
#Q3: 75th percentile
#Highest value (Q4)
boxplot(iris$Sepal.Length,las=1)#this is the distribution of ALL sepal lengths,
#without considering the species to which belong.
#To visualize how a given variable distribute in different groups, we can use
boxplot(iris$Sepal.Length~iris$Species,las=1)

#Skewness and kurtosis in R are available in the "moments" package
#Install the package
install.packages("moments")
#load the library
if(!require(moments)){install.packages("moments")}
skewness(iris$Sepal.Length)
kurtosis(iris$Sepal.Length)
plotNormalHistogram(iris$Sepal.Length)
#let's check for asymmetry in the distribution of petal width for each species
#First we need to subdivide the dataset based on species
Setosa=iris[iris$Species=="setosa",]
Versicolor=iris[iris$Species=="versicolor",]
Virginica=iris[iris$Species=="virginica",]
#Now we can plot the distribution
plotNormalHistogram(Setosa$Petal.Width)
plotNormalHistogram(Versicolor$Petal.Width)
plotNormalHistogram(Virginica$Petal.Width)
#Now measure the asymmetry in data distribution
skewness(Setosa$Petal.Width)
skewness(Versicolor$Petal.Width)
skewness(Virginica$Petal.Width)
kurtosis(Setosa$Petal.Width)
kurtosis(Versicolor$Petal.Width)
kurtosis(Virginica$Petal.Width)

#Confidence interval
#The "groupwiseMean" function in the "rcompanion" package can produce confidence
#intervals both by traditional and bootstrap methods, for grouped and ungrouped data. 

#Ungrouped data
#Ungrouped data is indicated with a 1 on the right side of the formula,
#or the group = NULL argument.
groupwiseMean(Petal.Width ~ 1, data   = iris, conf   = 0.95, digits = 3)
#Grouped data
#Imagine we want to compute the CI for each iris species
groupwiseMean(Petal.Width ~ Species, data   = iris, conf   = 0.95, digits = 3)
#Bootstrapped means by group
#In the "groupwiseMean" function, the type of confidence interval is requested
#by setting certain options to TRUE.
#These options are traditional, normal, basic, percentile...
#The boot option reports an optional statistic, the mean by bootstrap.
#The R option indicates the number of iterations to calculate each bootstrap statistic.
GWM=groupwiseMean(Petal.Width ~ Species, data   = iris, conf   = 0.95, digits = 3,
R= 10000,boot= TRUE)

#Optional: Other functions for traditional confidence intervals for means
#Functions that produce confidence intervals for means by the traditional
#method include t.test, CI in Rmisc, and MeanCI in DescTools.
#If needed, we can install the DescTools package with the command
if(!require(DescTools)){install.packages("DescTools")}
t.test(iris$Petal.Width,conf.level=0.95)
MeanCI(iris$Petal.Width,conf.level=0.95)
