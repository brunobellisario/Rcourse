#The package "lsr" could be of utility in this section
if(!require(lsr)){install.packages("lsr")}
#For an example of using the p-value for hypothesis testing,
#imagine you have a coin you will toss 100 times.
#We will set as 5 the number of winning throws
#The null hypothesis is that the coin is fair—that is
#that it is equally likely that the coin will land on heads as
#land on tails.
#The alternative hypothesis is that the coin is not fair.
#Let’s say for this experiment you throw the coin 100 times
#and it lands on heads 95 times out of those hundred.
#The p-value in this case would be the probability of
#getting 95, 96, 97, 98, 99, or 100 heads
#or 0, 1, 2, 3, 4, or 5 heads, assuming that the null hypothesis is true. 
binom.test(5, 100, 0.5)

#Passing and failing example
#As another example, imagine we are considering two classrooms,
#and we have counts of students who passed a certain exam.
#We want to know if one classroom had statistically more passes or failures than the other.
#In our example each classroom will have 10 students.
#The data is arranged into a contingency table.

#Classroom  Passed  Failed
#A          8       2
#B          3       7

#We will use Fisher’s exact test to test if there is an association between
#Classroom and the counts of passed and failed students.
#The null hypothesis is that there is no association between Classroom and Passed/Failed,
#based on the relative counts in each cell of the contingency table.

Input =("
 Classroom  Passed  Failed
 A          8       2
 B          3       7
")
Matrix = as.matrix(read.table(textConnection(Input),header=TRUE,row.names=1))
Matrix
fisher.test(Matrix)
#The reported p-value is 0.070.  If we use an alpha of 0.05, then the p-value
#is greater than alpha, so we fail to reject the null hypothesis.
#That is, we did not have sufficient evidence to say that there is an association between
#Classroom and Passed/Failed.

#Effect sizes and practical importance
#Practical importance and statistical significance
#It is important to remember to not let p-values be the only guide for drawing conclusions.
#It is equally important to look at the size of the effects you are measuring, as well
#as take into account other practical considerations like the costs of choosing a certain
#path of action.
#For example, imagine we want to compare the SAT scores of two SAT preparation classes
#with a t-test.

Class.A = c(1500, 1505, 1505, 1510, 1510, 1510, 1515, 1515, 1520, 1520)
Class.B = c(1510, 1515, 1515, 1520, 1520, 1520, 1525, 1525, 1530, 1530)
t.test(Class.A, Class.B)

#The p-value is reported as 0.003, so we would consider there to be a significant
#difference between the two classes (p < 0.05).
#But we have to ask ourselves the practical question, is a difference of 10 points
#on the SAT large enough for us to care about?
#What if enrolling in one class costs significantly more than the other class?
#Is it worth the extra money for a difference of 10 points on average?

#Sizes of effects
#It should be remembered that p-values do not indicate the size of the effect being studied.
#It shouldn’t be assumed that a small p-value indicates a large difference between groups,
#or vice-versa. 
#For example, in the SAT example above, the p-value is fairly small, but the size of the effect
#(difference between classes) in this case is relatively small (10 points, especially small
#relative to the range of scores students receive on the SAT).
#Conversely, there could be a relatively large size of the effects, but if there is a lot of
#variability in the data or the sample size is not large enough, the p-value could be
#relatively large. 
#In this example, the SAT scores differ by 100 points between classes,
#but because the variability is greater than in the previous example,
#the p-value is not significant.

Class.C = c(1000, 1100, 1200, 1250, 1300, 1300, 1400, 1400, 1450, 1500)
Class.D = c(1100, 1200, 1300, 1350, 1400, 1400, 1500, 1500, 1550, 1600)
t.test(Class.C, Class.D)
boxplot(cbind(Class.C, Class.D))

#Effect size statistics
#One way to account for the effect of sample size on our statistical tests is
#to consider effect size statistics.These statistics reflect the size of the effect
#in a standardized way, and are unaffected by sample size.
#An appropriate effect size statistic for a t-test is Cohen’s d.
#It takes the difference in means between the two groups and divides by the pooled
#standard deviation of the groups.  Cohen’s d equals zero if the means are the same,
#and increases to infinity as the difference in means increases relative to the standard deviation.

cohensD(Class.C, Class.D,
        method = "raw")
#A Cohen’s d of 1 suggests that the two means differ by one pooled standard deviation.
#A Cohen’s d of 0.5 suggests that the two means differ by one-half the pooled standard deviation.

#Independent and paired values
#Box plot and summary statistics by group

if(!require(FSA)){install.packages("FSA")}
data(iris)#here, we will recall the ORIGINAL dataset of iris

Summarize(Sepal.Length ~ Species, data=iris, digits=3)
boxplot(Sepal.Length ~ Species, data=iris,ylab="Sepal length (cm)")

#Bar plot to show paired differences
#The previous summary statistics, however, do not capture the paired nature of the data.
#Instead, we want to investigate the difference between setosa and versicolor species for each
#individual.  We can calculate this difference, and use a bar plot to visualize the difference.

Setosa  = iris$Sepal.Length[iris$Species=="setosa"]#subset datasets
Virginica  = iris$Sepal.Length[iris$Species=="virginica"]
Difference = Setosa - Virginica
barplot(Difference,   
        col="dark gray", 
        xlab="Observation",
        ylab="Difference (Setosa – Versicolor)")


#Paired t-test and unpaired t-test
#t-tests are discussed later.It isn’t important that you understand the test fully at this point.
#In this example, a t-test that ignores the pairing of observations found no difference
#between the mean length for left hand and right hand, whereas the t-test that accounts
#for the paired observations found a significant difference.
#On average the right hands were about 0.2 cm longer than their paired left hands.

testMatrix <- subset(iris , Species == "setosa" | Species == "virginica")
t.test(Sepal.Length ~ Species,data=testMatrix,paired = FALSE)
t.test(Sepal.Length ~ Species,data=testMatrix,paired = TRUE)
plotNormalHistogram(Difference)
### Distribution of differences is probably close enough to normal 
### for paired t-test

#PARAMETRIC statistics
#Assumptions in parametric statistics
#1.Random sampling
#2.Independent observation
#3.Normal distribution of data and residuals

#One sample t-test
#A histogram of the data can be examined to
#determine if the data are sufficiently normal.
if(!require(psych)){install.packages("psych")}
x = iris[iris$Species=="setosa",]
library(rcompanion)
plotNormalHistogram(x$Petal.Length)
#Normal quantile plot of data
qqnorm(x$Petal.Length)
qqline(x$Petal.Length, col="red")

t.test(x$Petal.Length,              #data
       mu=5,           #a number indicating the true value of the mean.
       conf.level=0.95  #confidence level of the interval.
       )
#Effect size
cohensD(x$Petal.Length,mu=5)

#Cohen's D could also be calculated manually from the mean, mu, and standard deviation.
Mean = mean(x$Petal.Length)
Mu   = 5
Sd   = sd(x$Petal.Length)
Diff = Mean - Mu
CohenD = (Mean - Mu) / Sd

#Box plot with default value
if(!require(ggplot2)){install.packages("ggplot2")}

ggplot(data=x, 
       aes(x = Species, y =Petal.Length)) +
        geom_boxplot() +
        geom_point(aes(x = 1, y = 5), 
                   colour="blue",
                   size = 8,
                   shape = "+") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold")) +
        theme(axis.text = element_text(face = "bold"))

#Two-sample t-test
#Histograms for data by group
#Histograms of each species could be examined.
Setosa = iris$Sepal.Length[iris$Species == "setosa"]
Versicolor = iris$Sepal.Length[iris$Species == "versicolor"]
Virginica = iris$Sepal.Length[iris$Species == "virginica"]
plotNormalHistogram(Setosa)
plotNormalHistogram(Versicolor)
plotNormalHistogram(Virginica)

#The lattice package could also be used,
#but adding normal curves to histograms by groups
#requires some extra code.
if(!require(lattice)){install.packages("lattice")}
histogram(~ Sepal.Length | Species,
          data   = iris,
          type   = "density", 
          layout = c(1,3),           ###  columns and rows of individual plots
          
          panel=function(x, ...) {
            panel.histogram(x, ...)
            
            panel.mathdensity(dmath = dnorm, 
                              col   = "blue",
                              lwd   = 2,
                              args  = list(mean=mean(x),
                                           sd=sd(x)), ...)})

#...or, we can visulaize the distributions by using boxplots
boxplot(Sepal.Length~Species,data = iris)
#Two-sample unpaired t-test
t.test(Sepal.Length~Species,data = iris)#Gives an error
t.test(Setosa,Virginica)
t.test(Setosa,Versicolor)
t.test(Virginica,Versicolor)

#Effect size
#Cohen’s d can be used as an effect size statistic for a two-sample t-test. 
cohensD(Setosa,Virginica)
cohensD(Setosa,Versicolor)
cohensD(Virginica,Versicolor)

#Paired t-test
#It compares the means of two populations of paired observations by testing if the
#difference between pairs is statistically different from zero.
#An assumption of many statistical tests is that observations are independent of one another.
#This means that the value for one observation is unlikely to be influenced by the value of another observation. 
#When observations are not independent, thei are called paired, dependent, or correlated.
#Dependent samples commonly arise in a few situations. 
#One is repeated measures, in which the same subject is measured on multiple dates.
#This is like the people height. 
#A second is when we are taking multiple measurements of the same individual.
#Imagine students fill out a financial literacy knowledge questionnaire both before and after
#completing a home financial management workshop.  Each student’s score before and after was paired by student.
Input = ("
Time    Student  Score
Before  a         65
Before  b         75
Before  c         86
Before  d         69
Before  e         60
Before  f         81
Before  g         88
Before  h         53
Before  i         75
Before  j         73
After   a         77
After   b         98
After   c         92
After   d         77
After   e         65
After   f         77
After   g        100
After   h         73
After   i         93
After   j         75
")
Data = read.table(textConnection(Input),header=TRUE)

#Histogram of difference data
#A histogram with a normal curve imposed will be used to check if the paired differences
#between the two populations is approximately normal in distribution.
#First, two new variables, Before and After, are created by extracting the values of Score
#for observations with the Time variable equal to Before or After, respectively.
#Note that for this code to make sense, the first observation for Before is student a and
#the first observation for After is student a, and so on
Before = Data$Score[Data$Time=="Before"]
After  = Data$Score[Data$Time=="After"]
Difference = After - Before
x = Difference
plotNormalHistogram(x,
                    xlab="Difference (After - Before)")
#Plot the paired data
#Scatter plot with one-to-one line
#Paired data can visualized with a scatter plot of the paired cases. 
#In the plot below, points that fall above and to the left of the blue line
#indicate cases for which the value for After was greater than for Before.
#Note that the points in the plot are jittered slightly so that points that would
#fall directly on top of one another can be seen.
#First, two new variables, Before and After, are created by extracting the values of
#Score for observations with the Time variable equal to Before or After, respectively.
#A variable Names is also created for point labels.
Names  = Data$Student[Data$Time=="Before"]
plot(Before, jitter(After),    # jitter offsets points so you can see them all
     pch = 16,                 # shape of points
     cex = 1.0,                # size of points
     xlim=c(50, 110),          # limits of x-axis
     ylim=c(50, 110),          # limits of y-axis
     xlab="Before",            # label for x-axis
     ylab="After")             # label for y-axis
text(Before, After, labels=Names,  # Label location and text
     pos=3, cex=1.0)               # Label text position and size
abline(0, 1, col="blue", lwd=2)     # line with intercept of 0 and slope of 1

#Bar plot of differences
#Paired data can also be visualized with a bar chart of differences. 
#In the plot below, bars with a value greater than zero indicate cases for which the
#value for After was greater than for Before.
#New variables are first created for Before, After, and their Difference.

barplot(Difference,                             # variable to plot
        col="dark gray",                        # color of bars
        xlab="Observation",                     # x-axis label
        ylab="Difference (After – Before)",     # y-axis label
        names.arg=Names)                        # labels for bars

t.test(Score ~ Time,
       data   = Data,
       paired = TRUE)
cohensD(Score ~ Time, 
        data   = Data,
        method = "paired")
