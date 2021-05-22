#DATA ANALYSIS WITH R
#SESSION#4 - Statistical Inference#2

#Let's start wit ANOVA
#ANOVA is a statistical test for estimating how a quantitative dependent variable changes 
#according to the levels of one or more categorical independent variables. 
#ANOVA tests whether there is a difference in means of the groups at each level of the 
#independent variable.
#The null hypothesis (H0) of the ANOVA is no difference in means, and the 
#alternate hypothesis (Ha) is that the means are different from one another.
#We will walk you through the process of a one-way ANOVA (one independent variable) 
#and a two-way ANOVA (two independent variables).
#Our sample dataset contains observations from an imaginary study of the effects of 
#fertilizer type and planting density on crop yield.

#One-way ANOVA example
#In the one-way ANOVA, we test the effects of 3 types of fertilizer on crop yield.

#Two-way ANOVA example
#In the two-way ANOVA, we add an additional independent variable: planting density.
#We test the effects of 3 types of fertilizer and 2 different planting densities on crop yield.

#We will also include examples of how to perform and interpret a two-way 
#ANOVA with an interaction term, and an ANOVA with a blocking variable.

#First, install the packages you will need for the analysis (this only needs to be done once):
install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))

#Then load these packages into your R environment (do this every time you restart the R program):
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

crop.data <- read.csv("crop.data.csv", header = TRUE, 
colClasses = c("factor", "factor", "factor", "numeric")#colClasses is a vector specifying which class
                                                       #each column belong to
                      )
str(crop.data)#we can check the classes

#Step 2: Perform the ANOVA test
#ANOVA tests whether any of the group means are different from the overall mean of 
#the data by checking the variance of each individual group against the overall variance 
#of the data. If one or more groups falls outside the range of variation predicted by the 
#null hypothesis (all group means are equal), then the test is statistically significant.
#We can perform an ANOVA in R using the aov() function. This will calculate the test 
#statistic for ANOVA and determine whether there is significant variation among the 
#groups formed by the levels of the independent variable.

#One-way ANOVA
#In the one-way ANOVA example, we are modeling crop yield as a function of the 
#type of fertilizer used. First we will use aov() to run the model, then we will
#use summary() to print the summary of the model.
one.way <- aov(yield ~ fertilizer, data = crop.data)
summary(one.way)

#The model summary first lists the independent variables being tested in the model 
#(in this case we have only one, ‘fertilizer’) and the model residuals (‘Residual’). 
#All of the variation that is not explained by the independent variables is called residual 
#variance.
#The rest of the values in the output table describe the independent variable and the residuals:
#1.Df column displays the degrees of freedom for the independent variable 
#(the number of levels in the variable minus 1), and the degrees of freedom 
#for the residuals (the total number of observations minus one and minus the number 
#of levels in the independent variables).
#2.Sum Sq column displays the sum of squares (a.k.a. the total variation between the 
#group means and the overall mean).
#3.Mean Sq column is the mean of the sum of squares, calculated by dividing the sum of 
#squares by the degrees of freedom for each parameter.
#4.The F-value column is the test statistic from the F test. This is the mean square 
#of each independent variable divided by the mean square of the residuals. 
#The larger the F value, the more likely it is that the variation caused by the independent 
#variable is real and not due to chance.
#5.Pr(>F) column is the p-value of the F-statistic. This shows how likely it is that 
#the F-value calculated from the test would have occurred if the null hypothesis of 
#no difference among group means were true.
#6.The p-value of the fertilizer variable is low (p < 0.001), so it appears that the 
#type of fertilizer used has a real impact on the final crop yield.

#Two-way ANOVA
#In the two-way ANOVA example, we are modeling crop yield as a function of type of 
#fertilizer and planting density. First we use aov() to run the model, then we use 
#summary() to print the summary of the model.
two.way <- aov(yield ~ fertilizer + density, data = crop.data)
summary(two.way)

#Adding planting density to the model seems to have made the model better: 
#it reduced the residual variance (the residual sum of squares went from 35.89 to 30.765), 
#and both planting density and fertilizer are statistically significant (p-values < 0.001).

#Adding interactions between variables
#Sometimes you have reason to think that two of your independent variables have an interaction 
#effect rather than an additive effect.
#For example, in our crop yield experiment, it is possible that planting density affects 
#the plants’ ability to take up fertilizer. This might influence the effect of fertilizer 
#type in a way that isn’t accounted for in the two-way model.
#To test whether two variables have an interaction effect in ANOVA, simply use an asterisk 
#instead of a plus-sign in the model:
interaction <- aov(yield ~ fertilizer*density, data = crop.data)
summary(interaction)

#In the output table, the ‘fertilizer:density’ variable has a low sum-of-squares value and 
#a high p-value, which means there is not much variation that can be explained by the interaction 
#between fertilizer and planting density.

#Adding a blocking variable
#If you have grouped your experimental treatments in some way, or if you have a confounding 
#variable that might affect the relationship you are interested in testing, you should include 
#that element in the model as a blocking variable. The simplest way to do this is just to add 
#the variable into the model with a ‘+’.
#For example, in many crop yield studies, treatments are applied within ‘blocks’ in the field 
#that may differ in soil texture, moisture, sunlight, etc. To control for the effect of 
#differences among planting blocks we add a third term, ‘block’, to our ANOVA.
blocking <- aov(yield ~ fertilizer + density + block, data = crop.data)
summary(blocking)

#The ‘block’ variable has a low sum-of-squares value (0.486) and a high p-value (p = 0.48), 
#so it’s probably not adding much information to the model. It also doesn’t change the sum 
#of squares for the two independent variables, which means that it’s not affecting how much 
#variation in the dependent variable they explain.

#Step 3: Find the best-fit model
#There are now four different ANOVA models to explain the data. How do you decide which one to use?
#Usually you’ll want to use the ‘best-fit’ model – the model that best explains the variation in
#the dependent variable.
#The Akaike information criterion (AIC) is a good test for model fit. AIC calculates the 
#information value of each model by balancing the variation explained against the number of 
#parameters used.
#In AIC model selection, we compare the information value of each model and choose the one with 
#the lowest AIC value (a lower number means more information explained!)

library(AICcmodavg)
model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")
aictab(model.set, modnames = model.names)

#From these results, it appears that the two.way model is the best fit. 
#The two-way model has the lowest AIC value, and 71% of the AIC weight, which means that 
#it explains 71% of the total variation in the dependent variable that can be explained by 
#the full set of models.
#The model with blocking term contains an additional 15% of the AIC weight, but because 
#it is more than 2 delta-AIC worse than the best model, it probably isn’t good enough to
#include in your results.

#Step 4: Check for homoscedasticity
#To check whether the model fits the assumption of homoscedasticity, look at the 
#model diagnostic plots in R using the plot() function:
par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

#The diagnostic plots show the unexplained variance (residuals) across the range of the 
#observed data.
#Each plot gives a specific piece of information about the model fit, but it’s enough 
#to know that the red line representing the mean of the residuals should be horizontal 
#and centered on zero (or on one, in the scale-location plot), meaning that there are 
#no large outliers that would cause bias in the model.
#The normal Q-Q plot plots a regression between the theoretical residuals of a 
#perfectly-Homoscedasticity model and the actual residuals of your model, so the 
#closer to a slope of 1 this is the better. This Q-Q plot is very close, with only 
#a bit of deviation.
#From these diagnostic plots we can say that the model fits the assumption of Homoscedasticity
#If your model doesn’t fit the assumption of homoscedasticity, you can try the Kruskall-Wallis test instead.

#Step 5: Do a post-hoc test
#ANOVA tells us if there are differences among group means, but not what the differences are.
#To find out which groups are statistically different from one another, you can perform a 
#Tukey’s Honestly Significant Difference (Tukey’s HSD) post-hoc test for pairwise comparisons:
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

#From the post-hoc test results, we see that there are statistically significant differences
#(p < 0.05) between fertilizer groups 3 and 1 and between fertilizer types 3 and 2, but the 
#difference between fertilizer groups 2 and 1 is not statistically significant. 
#There is also a significant difference between the two different levels of planting density.

#Step 6: Plot the results in a graph
#When plotting the results of a model, it is important to display:
#1.the raw data
#2.summary information, usually the mean and standard error of each group being compared
#3.letters or symbols above each group being compared to indicate the groupwise differences.
#Find the groupwise differences
#From the ANOVA test we know that both planting density and fertilizer type are significant 
#variables. To display this information on a graph, we need to show which of the combinations 
#of fertilizer type + planting density are statistically different from one another.
#To do this, we can run another ANOVA + TukeyHSD test, this time using the interaction 
#of fertilizer and planting density. We aren’t doing this to find out if the interaction 
#term is significant (we already know it’s not), but rather to find out which group means 
#are statistically different from one another so we can add this information to the graph.
tukey.plot.aov<-aov(yield ~ fertilizer:density, data=crop.data)
#Instead of printing the TukeyHSD results in a table, we’ll do it in a graph.
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

#The significant groupwise differences are any where the 95% confidence interval doesn’t 
#include zero. This is another way of saying that the p-value for these pairwise differences 
#is < 0.05.
#From this graph, we can see that the fertilizer + planting density combinations which are 
#significantly different from one another are 3:1-1:1 
#(read as “fertilizer type three + planting density 1 contrasted with fertilizer type 1 + 
#planting density type 1”), 1:2-1:1, 2:2-1:1, 3:2-1:1, and 3:2-2:1.
#To summarize, we have 3 distinct groupings. Fertilizer 3, planting density 2 is different 
#from all of the other combinations, as is fertilizer 1, planting density 1. 
#All of the others are intermediate. So we can make three labels for our graph: 
#1.A (representing 1:1), 
#2.B (representing all the intermediate combinations), and 
#3.C (representing 3:2).

#Make a data frame with the group labels
#Now we need to make an additional data frame so we can add these groupwise differences 
#to our graph.
#First, summarize the original data using fertilizer type and planting density as grouping 
#variables.
mean.yield.data <- crop.data %>%
  group_by(fertilizer, density) %>%
  summarise(yield = mean(yield)
  )
#Next, add the group labels as a new variable in the data frame.
mean.yield.data$group <- c("a","b","b","b","b","c")
mean.yield.data

#Plot the raw data
two.way.plot <- ggplot(crop.data, aes(x = density, y = yield, group=fertilizer)) +
geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
two.way.plot
#Add the means and standard errors to the graph
two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.yield.data, aes(x=density, y=yield))
two.way.plot

#This is very hard to read, since all of the different groupings for fertilizer type 
#are stacked on top of one another.
#Split up the data
#To show which groups are different from one another, use facet_wrap() to split the data 
#up over the three types of fertilizer. To add labels, use geom_text(), and add the 
#group letters from the mean.yield.data dataframe you made earlier.
two.way.plot <- two.way.plot +
  geom_text(data=mean.yield.data, label=mean.yield.data$group, vjust = -8, size = 5) +
  facet_wrap(~ fertilizer)
two.way.plot

#Make the graph ready for publication
#In this step we will remove the grey background and add axis labels.
two.way.plot <- two.way.plot +
  theme_classic2() +
  labs(title = "Crop yield in response to fertilizer mix and planting density",
       x = "Planting density (1=low density, 2=high density)",
       y = "Yield (bushels per acre)")
two.way.plot

#An introduction to simple linear regression
#Regression models describe the relationship between variables by fitting a line to the 
#observed data. Linear regression models use a straight line, while logistic and nonlinear 
#regression models use a curved line. Regression allows you to estimate how a dependent variable 
#changes as the independent variable(s) change.
#Simple linear regression is used to estimate the relationship between two quantitative variables.
#You can use simple linear regression when you want to know:
#1.How strong the relationship is between two variables (e.g. the relationship between rainfall 
#and soil erosion).
#2.The value of the dependent variable at a certain value of the independent variable 
#(e.g. the amount of soil erosion at a certain level of rainfall).

#Assumptions of simple linear regression
#Simple linear regression is a parametric test, meaning that it makes certain assumptions 
#about the data. These assumptions are:
#1.Homogeneity of variance (homoscedasticity): the size of the error in our prediction 
#doesn’t change significantly across the values of the independent variable.
#2.Independence of observations: the observations in the dataset were collected using 
#statistically valid sampling methods, and there are no hidden relationships among observations.
#3.Normality: The data follows a normal distribution.
#Linear regression makes one additional assumption:
#The relationship between the independent and dependent variable is linear: the line of best 
#fit through the data points is a straight line (rather than a curve or some sort of grouping 
#factor).
#If your data do not meet the assumptions of homoscedasticity or normality, you may be able to 
#use a nonparametric test instead, such as the Spearman rank test.
income.data <- read.csv("income.data.csv", header = TRUE)

#Make sure your data meet the assumptions
hist(income.data$happiness)#Normality
plot(happiness ~ income, data = income.data)#Linearity
#Let's perform the regression
income.happiness.lm <- lm(happiness ~ income, data = income.data)
#Interpreting the results
#To view the results of the model, you can use the summary() function in R:
summary(income.happiness.lm)

#This output table first repeats the formula that was used to generate the results (‘Call’), 
#then summarizes the model residuals (‘Residuals’), which give an idea of how well the model 
#fits the real data.
#Next is the ‘Coefficients’ table. The first row gives the estimates of the y-intercept, 
#and the second row gives the regression coefficient of the model.
#Row 1 of the table is labeled (Intercept). This is the y-intercept of the regression equation, 
#with a value of 0.20. You can plug this into your regression equation if you want to predict 
#happiness values across the range of income that you have observed:
#happiness = 0.20 + 0.71*income ± 0.018
#The next row in the ‘Coefficients’ table is income. This is the row that describes the 
#estimated effect of income on reported happiness:
#The Estimate column is the estimated effect, also called the regression coefficient or r2 value. 
#The number in the table (0.713) tells us that for every one unit increase in income 
#(where one unit of income = $10,000) there is a corresponding 0.71-unit increase in reported 
#happiness (where happiness is a scale of 1 to 10).
#The Std. Error column displays the standard error of the estimate. This number shows how 
#much variation there is in our estimate of the relationship between income and happiness.
#The t value column displays the test statistic. Unless you specify otherwise, the test 
#statistic used in linear regression is the t-value from a two-sided t-test. 
#The larger the test statistic, the less likely it is that our results occurred by chance.
#The Pr(>| t |) column shows the p-value. This number tells us how likely we are to see 
#the estimated effect of income on happiness if the null hypothesis of no effect were true.
#Because the p-value is so low (p < 0.001), we can reject the null hypothesis and conclude 
#that income has a statistically significant effect on happiness.
#The last three lines of the model summary are statistics about the model as a whole. 
#The most important thing to notice here is the p-value of the model. 
#Here it is significant (p < 0.001), which means that this model is a good fit for the 
#observed data.

#To check whether the model fits the assumption of homoscedasticity, look at the 
#model diagnostic plots in R using the plot() function:
par(mfrow=c(2,2))
plot(income.happiness.lm)
par(mfrow=c(1,1))

#Residuals are the unexplained variance. They are not exactly the same as model error, 
#but they are calculated from it, so seeing a bias in the residuals would also indicate 
#a bias in the error.
#The most important thing to look for is that the red lines representing the mean of the 
#residuals are all basically horizontal and centered around zero. This means there are no 
#outliers or biases in the data that would make a linear regression invalid.
#In the Normal Q-Qplot in the top right, we can see that the real residuals from our model 
#form an almost perfectly one-to-one line with the theoretical residuals from a perfect model.
#Based on these residuals, we can say that our model meets the assumption of homoscedasticity.

#The package visreg has nice ways to plot linear models
install.packages("visreg")
library(visreg)
visreg(income.happiness.lm)

#Multiple linear regressions
heart.data <- read.csv("heart.data.csv", header = TRUE)

#In multiple regression we should test for independence of observations (aka no autocorrelation)
#Use the cor() function to test the relationship between your independent variables and make 
#sure they aren’t too highly correlated.
cor(heart.data$biking, heart.data$smoking)
hist(heart.data$heart.disease)#Normality assumption
plot(heart.disease ~ biking, data=heart.data)#Linearity
#Perform the regression
heart.disease.lm<-lm(heart.disease ~ biking + smoking, data = heart.data)
summary(heart.disease.lm)

#The estimated effect of biking on heart disease is -0.2, while the estimated effect 
#of smoking is 0.178.
#This means that for every 1% increase in biking to work, there is a correlated 0.2% decrease 
#in the incidence of heart disease. Meanwhile, for every 1% increase in smoking, there is 
#a 0.178% increase in the rate of heart disease.
#The standard errors for these regression coefficients are very small, and the 
#t-statistics are very large (-147 and 50.4, respectively). The p-values reflect these 
#small errors and large t-statistics. For both parameters, there is almost zero probability 
#that this effect is due to chance.

#To check whether the model fits the assumption of homoscedasticity, look at the 
#model diagnostic plots in R using the plot() function:
par(mfrow=c(2,2))
plot(heart.disease.lm)
par(mfrow=c(1,1))
#The residuals show no bias, so we can say our model fits the assumption of homoscedasticity.
visreg(heart.disease.lm, "smoking")
visreg(heart.disease.lm, "biking")

#Correlation and linear regressions
#Visualizing correlated variables
#The pairs function can plot multiple numeric or integer variables
#on a single plot to look for correlations among the variables.
pairs(heart.data)
pairs(income.data)


#Correlation coefficient can be computed using the functions cor() or cor.test():
#cor() computes the correlation coefficient
#cor.test() test for association/correlation between paired samples.
#It returns both the correlation coefficient and the significance level(or p-value) 
#of the correlation

cor(heart.data$biking,heart.data$heart.disease,method = c("pearson"))
cor(heart.data$biking,heart.data$heart.disease,method  = c("spearman"))
cor(heart.data$biking,heart.data$heart.disease,method  = c("kendall"))
#cor.test to check the significance of the correlation
cor.test(heart.data$biking,heart.data$heart.disease,method = c("pearson"))
cor.test(heart.data$biking,heart.data$heart.disease,method = c("spearman"))
cor.test(heart.data$biking,heart.data$heart.disease,method = c("kendall"))
#NOTE. In presence of ties, that is, those pairs of data between samples that have the same ordinal values,
#The spearman correlation gives you a warning, meaning that the correlation (and associated p-value)
#can be computed BUT with some marginal errors.
#Conversely, the Kendall's tau formula tries to mathematically manage their presence and, thus,
#Should be more appropriate in case of an extremely high number of ties in data set.
#If your data contain missing values, use the following R code to handle missing values by case-wise deletion.
cor(heart.data$biking,heart.data$heart.disease,method = "pearson", use = "complete.obs")

#Using rcorr() function in the Hmisc package to test for correlation between each columns of a matrix
if(!require(Hmisc)){install.packages("Hmisc")}
#Imagine we want to check for a correlation between the length and width of both sepal and petal in Iris setosa
heart.data_correlation=rcorr(as.matrix(heart.data),type="spearman")
#Possible ways to visualize correlations
if(!require(PerformanceAnalytics)){install.packages("PerformanceAnalytics")}
chart.Correlation(as.matrix(heart.data),
                  method="pearson",
                  histogram=TRUE,
                  pch=16)
#The correlogram
#Correlogram is a graph of correlation matrix. It is very useful to highlight the most correlated
#variables in a data table. In this plot, correlation coefficients is colored according to the value.
#Correlation matrix can be also reordered according to the degree of association between variables.
#The R corrplot package is used here.
if(!require(corrplot)){install.packages("corrplot")}
#corrplot needs a matrix of correlation values to plot your graph
#Since the rcorr() function returns two objects (r an p values) under a list structure
#we need to extract the correlation one
#let's check if the object "sepal_petal_correlation" is a list
is.list(heart.data_correlation)
#TRUE
R=heart.data_correlation$r#with this command we will select the matrix of correlation values
P=heart.data_correlation$P#with this command we will select the matrix of p values
#Now, we can plot our correlation matrix in different ways
corrplot(R, method="circle")
corrplot(R, method="pie")
corrplot(R, method="color")
corrplot(R, method="number")

#Specialized the insignificant value according to the significant level
corrplot(R, type="upper", order="hclust", p.mat = P, sig.level = 0.05,
         insig = "blank")
corrplot(R, type="upper", order="hclust", p.mat = P, sig.level = 0.05)

#QUICK EXERCISE######################################################################
#Let's check when linear models and anova look like the same
#####
