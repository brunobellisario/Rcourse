# DATA ANALYSIS WITH R
# SESSION#5 - Statistical Inference#3
# 
# Basic nonparametric statisatics
# Nonparametric testing
# Often ecological data is messy stuff that fails to meet our statistical assumptions. 
# So what do we do when this is the case? One option is nonparametric tests.
# These tests have relaxed assumptions about normality and homogeneity of variances.
# Sampling still needs to be random and independent though!
#   
# When to use nonparametric tests?
# The most likely scenario to use nonparametric tests is you wanted to use a one -
# way ANOVA or t - test, but the assumptions aren’t met. Then we use a nonparametric test.
# There is significant debate in the statistical field on if this is an appropriate use of 
# nonparametric tests.
# 
# Some experimental designs are actually well - suited to nonparametric analysis, especially 
# ones with naturally skewed or otherwise nonnormal distributions.
# Often nonparametric tests are described as less ‘powerful’ (the ability to detect a significant 
# difference) than parametric tests (like ANOVA, t-test)

# Wilcoxon test (Mann-Whitney U)
# This is the nonparametric equivalent of a t-test and goes by several names - Wilcoxon, 
# Mann-Whitney, etc.
# It is very similar to a K-W test, except that you only compare between two groups.
# We will use the iris dataset again, but this time use the subset function to toss out 
# the versicolor species, so that we only have two groups (species) to compare between. 
# Note: the ‘!=’ means ‘not equal to’. 
irisSubset <- subset(iris,Species!="versicolor")
# Wilcoxon Test Code
# Same structure here as with the last K-W method: our response variable (Petal.Length) 
# explained by our explanatory variable (Species).
wilcox.test(Petal.Length~Species,data=irisSubset) #using subsetted data
# There is a significant difference between the two groups!

# Kruskal - Wallis Test
# The Kruskal - Wallis Test (K - W) is roughly equivalent to one - way ANOVA.
# The K-W is used to test for differences among groups. It is often said that it tests 
# for differences in median among groups - this is not technically true. It is more like 
# it tests for differences in distribution of ranked data. 
# The simplest way to understand the K - W (and related tests) is that it looks for differences 
# between medians of groups, instead of means.

# K - W Code
# The code for K - W test is super easy and there are several methods for the code. 
#For this tutorial, we will use the freely available iris dataset from the R world. 

# First method
# No packages are needed for the function kruskal.test. The first variable (add here) 
# is the numeric response variable. The second variable (add here) is our categorical 
#explanatory variable. I will show two equivalent ways of coding this way, the first using 
#the attach function to ‘attach’ the data. The second way directly references the dataframe 
#for each variable.

#First Way
data(iris) #makes it so that R references the iris dataframe by default (we can also use "attach")
kruskal.test(iris$Petal.Length,iris$Species)

# Second Method
# Same function here, but now we write out the formula of the K-W test. 
# The ~ means ‘given’ or ‘explained by’. We directly reference the iris dataset with 
#the data argument.
kruskal.test(Petal.Length~Species,data=iris)

# K-W results
# Each of these K-W methods produces the same result - a table that tells us the Kruskal X2 
# of this test is 130.41, there were two degrees of freedom, and a super small p-value.
# So at least one of the groups has significantly different petal lengths from the others! 
# But which group could it be?
#   
# Post-hoc multiple comparisons for K-W
# Dunn test for multiple comparisons of groups
# If the K –W test is significant, a post-hoc analysis
# can be performed to determine which groups differ from each other group.
library(FSA)
DT = dunnTest(Sepal.Length ~ Species,
              data=iris,
              method="bh")      # Adjusts p-values for multiple comparisons;
# See ?dunnTest for options

#Plot of medians and confidence intervals
#The following code uses the groupwiseMedian function to produce a
#data frame of medians for each speaker along with the 95% confidence intervals
#for each median with the percentile method.  These medians are then plotted,
#with their confidence intervals shown as error bars.
#The grouping letters from the multiple comparisons are added.
library(rcompanion)
library(ggplot2)
Sum = groupwiseMedian(Sepal.Length ~ Species,
                      data       = iris, 
                      conf       = 0.95, 
                      R          = 5000,
                      percentile = TRUE, 
                      bca        = FALSE, 
                      digits     = 3)
X     = 1:3
Y     = Sum$Percentile.upper + 0.2
Label = c("a", "b", "a")
ggplot(Sum,                ### The data frame to use.
       aes(x = Species,
           y = Median)) +
  geom_errorbar(aes(ymin = Percentile.lower,
                    ymax = Percentile.upper),
                width = 0.05, 
                size  = 0.5) +
  geom_point(shape = 15, 
             size  = 4) +
  theme_bw() +
  theme(axis.title   = element_text(face  = "bold")) +
  ylab("Median Likert score") +
  annotate("text", 
           x = X, 
           y = Y, 
           label = Label)

# Effect size
# Statistics of effect size for the K - W test provide the degree
# to which one group has data with higher ranks than another group.
# They are related to the probability that a value from one group will be
# greater than a value from another group.
# Unlike p-values, they are not affected by sample size.
# epsilon-squared
epsilonSquared(x = iris$Sepal.Length,g = iris$Species)
#Freeman’s theta
freemanTheta(x = iris$Sepal.Length,g = iris$Species)

# Friedman Test in R
# The Friedman test is a non-parametric alternative to the one-way repeated measures ANOVA test. 
# It extends the Sign test in the situation where there are more than two groups to compare.
# Friedman test is used to assess whether there are any statistically significant differences 
# between the distributions of three or more paired groups. It’s recommended when the normality 
# assumptions of the one-way repeated measures ANOVA test is not met or when the dependent variable 
#is measured on an ORDINAL scale.

# The Friedman test is essentially a 2-way analysis of variance used on non-parametric data. 
# The test only works when you have completely balanced design. You can also use Friedman 
# for one-way repeated measures types of analysis.
# Here is an example of a data file which has a 2-way unreplicated block design.
Input =("
  Count Month Year
 '2' 1 2004  
 '48' 1  2005
 '40' 1  2006
 '3' 2   2004
 '120' 2  2005
 '81' 2 2006
 '2' 3 2004
 '16' 3  2005
 '36'  3 2006
 '7'  4 2004
 '21' 4 2005
 '17' 4 2006
 '2' 5 2004
 '14' 5 2005
 '17' 5 2006
")
amphibian = read.table(textConnection(Input),header=TRUE)
# What you have here are data on surveys of amphibians. The first column (count) represents 
# the number of individuals captured. The final column is the year that the survey was conducted. 
# The middle column (month) shows that for each year there were 5 survey events in each year. 
# What you have here is a replicated block design. Each year is a “treatment” (or “group”) whilst 
# the month variable represents a “block”. This is a common sort of experimental design; the blocks 
# are set up to take care of any possible variation and to provide replication for the treatment. 
# In this instance you wish to know if there is any significant difference due to year.
#
# The Friedman test allows you to carry out a test on these data. You need to determine which 
# variable is the group and which the block. The friedman.test() function allows you to perform 
# the test. One way is to specify the groups and the blocks like so:
attach(amphibian)
friedman.test(Count, groups = Year, blocks=Month)
detach(amphibian)
friedman.test(Count ~ Year | Month, data= amphibian)
# Here the vertical bar “splits” the predictor variables into group | block. 
# It is important to get the group and block variables the correct way around!
# Post Hoc testing for Friedman tests
# 
# There is a real drop in statistical power when using a Friedman test compared to anova. 
# Although there are methods that enable post-hoc tests, the power is such that obtaining 
# significance is well-nigh impossible in most cases.
# 
# You can try pairwise.wilcox.test() with various groupings, e.g.:
pairwise.wilcox.test(amphibian$Count, g = amphibian$Year)
# In this analysis any other grouping is impossible (as there is no replication).

# Aligned Ranks Transformation ANOVA
# Aligned ranks transformation ANOVA (ART anova) is a nonparametric approach that
# allows for multiple independent variables, interactions, and repeated measures
# Test dataset
# Let’s generate some test data where we actually know what the effects are. Specifically,
library(ARTool)
library(emmeans)    #emmeans, contrast

data(InteractionTestData, package = "ARTool")
df = InteractionTestData    #save some typing
# Let's plot
palette = c("#1b9e77", "#d95f02", "#7570b3")
names(palette) = c("C", "D", "E")
library(dplyr) 
df %>%
  ggplot(aes(x = X1, y = Y, color = X2)) +
  geom_violin(trim = FALSE, adjust = 1.5) +
  geom_point(pch = "-", size = 4) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(aes(group = X2), fun = mean, geom = "line", size = 1) +
  stat_summary(aes(x = 1.5, group = NA), fun = mean, geom = "point", size = 9, pch = "+") +
  scale_y_continuous(breaks = seq(-6, 10, by = 2), minor_breaks = -6:10) +
  scale_color_manual(guide = FALSE, values = palette) +
  coord_cartesian(ylim = c(-6, 10)) +
  facet_grid(. ~ X2)

m.art = art(Y ~ X1*X2, data = df)#ART model
anova(m.art)
# Contrast tests of main effects
marginal=art.con(m.art, "X1:X2",adjust="tukey")

#Post-hoc comparisons for main effects
if(!require(emmeans)){install.packages("emmeans")}
if(!require(multcomp)){install.packages("multcomp")}
pairs(marginal,adjust = "tukey")
cld.art=cld(marginal,alpha=0.05,Letters=letters,adjust="tukey")
plot(marginal)





