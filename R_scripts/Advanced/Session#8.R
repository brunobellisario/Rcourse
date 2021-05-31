#DATA ANALYSIS WITH R
#SESSION#4 - More on regressions and introduction to ML

#A reminder on linear regressions in R
#A linear regression is a statistical model that analyzes the relationship between
#a response variable (often called y) and one or more variables and their interactions
#(often called x or explanatory variables). 
#Linear regression assumes that there exists a linear relationship between the response
#variable and the explanatory variables. This means that you can fit a line between
#the two (or more variables).
#Once again, we will use the iris dataset
data(iris)
plot(Sepal.Length~Petal.Length,data=iris)
linear.model.iris=lm(Sepal.Length~Petal.Length,data=iris)
summary(linear.model.iris)
new.iris=iris[iris$Species==c("versicolor","virginica"),]#with this function we will exclude specific columns from the data frame
plot(Sepal.Length~Petal.Length,data=new.iris)
linear.model.newiris=lm(Sepal.Length~Petal.Length,data=new.iris)
summary(linear.model.iris)
#Model selection
#The Akaike Information Criterion AIC
install.packages("AICcmodavg")
library(AICcmodavg)
list.models=list(Model.all=linear.model.iris,Model.new=linear.model.newiris)#we need to create a list of models
aictab(list.models)
#We will use AIC after...

#Poisson regression
#What Are Poisson Regression Models?
#Poisson Regression models are best used for modeling events where the outcomes are counts.
#Or, more specifically, count data: discrete data with non-negative integer values that 
#count something, like the number of times an event occurs during a given timeframe or the
#number of people in line at the grocery store.
#Count data can also be expressed as rate data, since the number of times an event occurs 
#within a timeframe can be expressed as a raw count (i.e. “In a day, we eat three meals”) 
#or as a rate (“We eat at a rate of 0.125 meals per hour”).
#Poisson Regression helps us analyze both count data and rate data by allowing us to 
#determine which explanatory variables (X values) have an effect on a given response 
#variable (Y value, the count or a rate). 
# vector of colors
colors <- c("Red", "Blue", "Gold", "Black", "Pink", "Green")
#Next, we’ll create a list for the distribution that will have different values for μ:
# declare a list to hold distribution values
poisson.dist=list()
#Then, we’ll create a vector of values for μ and loop over the values from μ each with
#quantile range 0-20, storing the results in a list:
a = c(1, 2, 3, 4, 5, 6) # A vector for values of u
for (i in 1:6) {
  poisson.dist[[i]] <- c(dpois(0:20, i)) # Store distribution vector for each 
  #corresponding value of u
}
#Finally, we’ll plot the points using plot(). plot() is a base graphics function in R. Another common way to plot data in R would be using the popular ggplot2 package; this is covered in Dataquest’s R courses. But for this tutorial, we will stick to base R functions.
# plot each vector in the list using the colors vectors to represent each value for u
plot(unlist(poisson.dist[1]), type = "o", xlab="y", ylab = "P(y)",
     col = colors[i])
for (i in 1:6) {
  lines(unlist(poisson.dist[i]), type = "o", col = colors[i])
}
# Adds legend to the graph plotted
legend("topright", legend = a, inset = 0.08, cex = 1.0, fill = colors, title = "Values of u")
#The plot above illustrates the shape of a poisson distribution

#Poisson Regression Models and GLMs
#Generalized Linear Models are models in which response variables follow
#a distribution other than the normal distribution. That’s in contrast to 
#Linear regression models, in which response variables follow normal distribution. 
#Let’s Get Modeling!
#We’re going to model Poisson Regression related to how frequently yarn breaks 
#during weaving. This data is found in the datasets package in R, so the first thing 
#we need to do is install the package using install.package("datasets") and load the 
#library with library(datasets):
install.packages("datasets")
library(datasets) # include library datasets after installation
#The datasets package includes tons of datasets, so we need to specifically 
#select our yarn data. Consulting the package documentation, we can see that 
#it is called warpbreaks, so let’s store that as an object.
data=warpbreaks
str(warpbreaks)
#We can view the dependent variable breaks data continuity by creating a histogram:
hist(data$breaks)
#Clearly, the data is not in the form of a bell curve like in a normal distribution.
#Let’s check out the mean() and var() of the dependent variable:
mean(data$breaks) # calculate mean
var(data$breaks) # calculate variance
#The variance is much greater than the mean, which suggests that we will have over-dispersion in the model.
#Let’s fit the Poisson model using the glm() command.
# model poisson regression using glm()
poisson.model= glm(breaks ~ wool + tension, data, family = poisson(link = "log"))
summary(poisson.model)

#Visualizing Findings Using jtools
#When you are sharing your analysis with others, tables are often not the best 
#way to grab people’s attention. Plots and graphs help people grasp your findings
#more quickly. The most popular way to visualize data in R is probably ggplot2 
#we’re also going to use an awesome R package called jtools that includes tools 
#for specifically summarizing and visualizing regression models. 
#Let’s use jtools to visualize poisson.model2.
# Install the package jtools if not already installed
install.packages("jtools")
# you may be asked to install 'broom' and 'ggstance' packages as well
install.packages("broom")
install.packages("ggstance")
# Include jtools library
library(jtools)
# plot regression coefficients for poisson.model2
plot_summs(poisson.model, scale = TRUE, exp = TRUE)

#More on Generalized Linear Models (GLM)
#Gaussian GLM
#A Gaussian GLM is simply a linear regression model and is widely used in ecology
#to model a continuous variable that is assumed to be normally distributed.
#Typical ecological data that can be modeled with a Gaussian GLM include growth
#and body size data, species distributions along environmental gradients and animal 
#and plant densities.
#Rule of thumb in using GLMs
#A simple empirical rule:
#Gaussian family : for continuous decimal data with normal distribution, like weight, length, et al.
#Poisson or quasipoisson family: for positive integer or small natural number like count, individual number, frequency.
#Binomial or quasibinomial family: binary data like 0 and 1, or proportion like survival number vs death number, positive frequency vs negative frequency, winning times vs the number of failtures, et al...
#Gamma family : usually describe time data like the time or duration of the occurrence of the event.
#If your model is overdispered, you should make a correction by choosing quasi-binomial or quasi-poisson.

#Compared to the linear model, one advantage of the generalized linear model is its ability to model different relationships
#between the response variable and the predictors. One challenge is knowing which link to use.


install.packages("GlmSimulatoR")
library(GlmSimulatoR)
library(ggplot2)
library(stats)
#Let's see how different models affect the goodness of GLM fit for other distributions
#gaussian distribution
gaussian=simulate_gaussian(
  N = 10000,
  link = "identity",
  weights = 1:3,
  xrange = 1,
  unrelated = 0,
  ancillary = 1
)
ggplot(gaussian, aes(x=Y)) +   geom_histogram(bins = 30)#plot
#binomial distribution
binomial=simulate_binomial(
  N = 10000,
  link = "logit",
  weights = c(0.1, 0.2),
  xrange = 1,
  unrelated = 0
)
ggplot(binomial, aes(x=Y)) +   geom_histogram(bins = 30)#plot
#poisson distribution
poisson=simulate_poisson(
  N = 10000,
  link = "log",
  weights = c(0.5, 1),
  xrange = 1,
  unrelated = 0
)
ggplot(poisson, aes(x=Y)) +   geom_histogram(bins = 30)#plot

#Now, we will use GLMs to model the relationship between variables in different simulated distribution, with different link functions
#and test the goodness of fit by means of the Akaike Information Criterion (AIC)
glm.gaussian.distr1=glm(Y ~ X1+X2+X3, data = gaussian, family = gaussian())
glm.gaussian.distr2=glm(Y ~ X1+X2+X3, data = gaussian, family = binomial())#not work with non binary data!
glm.gaussian.distr3=glm(Y ~ X1+X2+X3, data = gaussian, family = Gamma())
glm.gaussian.distr4=glm(Y ~ X1+X2+X3, data = gaussian, family = poisson())
list.model.glms=list(Gaussian.family=glm.gaussian.distr1,
                     Gamma.family=glm.gaussian.distr3,Poisson.family=glm.gaussian.distr4)#we need to create a list of models
aictab(list.model.glms)
#Let's take a picture of different GLMs
plot_summs(list.model.glms,scale=T,exp=T)

#Now let's do it with other distributions
#Binomial
glm.binomial.distr1=glm(Y ~ X1+X2, data = binomial, family = gaussian())
glm.binomial.distr2=glm(Y ~ X1+X2, data = binomial, family = binomial())
glm.binomial.distr3=glm(Y ~ X1+X2, data = binomial, family = Gamma())#non-positive values not allowed for the 'Gamma' family
glm.binomial.distr4=glm(Y ~ X1+X2, data = binomial, family = poisson())
list.model.glms=list(Gaussian.family=glm.binomial.distr1,Binomial.family=glm.binomial.distr2,
Poisson.family=glm.binomial.distr4)#we need to create a list of models
aictab(list.model.glms)
#Let's take a picture of different GLMs
plot_summs(list.model.glms,scale=T,exp=T)

#Poisson
glm.poisson.distr1=glm(Y ~ X1+X2, data = poisson, family = gaussian())
glm.poisson.distr2=glm(Y ~ X1+X2, data = poisson, family = binomial())#not work with non binary data!
glm.poisson.distr3=glm(Y ~ X1+X2, data = poisson, family = Gamma())
glm.poisson.distr4=glm(Y ~ X1+X2, data = poisson, family = poisson())
list.model.glms=list(Gaussian.family=glm.poisson.distr1,
Gamma.family= glm.poisson.distr3,Poisson.family=glm.poisson.distr4)#we need to create a list of models
aictab(list.model.glms)

#Generalize Additive Models (GAM)
#Here, we will use the dataset named "mcycle", which contains measurement of acceleration
#of a crash-test dummy head during a motorcycle crash.
#It contains measurements of acceleration (g) in the accel column and time (milliseconds)
#in the times column.
#The dataset is in the MASS library
install.packages("MASS")
library(MASS)
mcycle <- MASS::mcycle

# Examine the mcycle data frame
str(mcycle)
plot(mcycle)
#Data are clearly non linear
#First, we will fit a linear model
lm_mod <- lm(accel~times, data = mcycle)
# Visualize the model
termplot(lm_mod, partial.resid = TRUE, se = TRUE)#Plots regression terms against their predictors,
#optionally with standard errors and partial residuals added.
#Then, we will use the gam function to perform a non linear regression
#We need to install the mgcv package
install.packages("mgcv")
library(mgcv)
gam_mod <- gam(accel~ s(times), #s is a  smoothing parameter, balancing the model
                                #between likelihood and wiggliness to optimize model fit. 
               data = mcycle)
# Plot the results
plot(gam_mod, residuals = TRUE, pch = 1)

#Now, we will fit two models to the mcycle data with accel as a smooth function
#of times and a smoothing parameter of:
#s1 = 0.1
#s2 = 0.0001
gam_mod_s1 <- gam(accel ~ s(times), data = mcycle, sp = 0.1)
gam_mod_s2 <- gam(accel ~ s(times), data = mcycle, sp = 0.0001)
# Plot both models
par(mfrow = c(2, 1))
plot(gam_mod_s1, residuals = TRUE, pch = 19,col="red")
plot(gam_mod_s2, residuals = TRUE, pch = 19,col="dark green")

AIC(gam_mod_s1,gam_mod_s2)#basic AIC function

AIC(lm_mod,gam_mod_s1,gam_mod_s2)




