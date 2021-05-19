#Session#4
#Statistical inference#2

#One-way ANOVA
boxplot(Sepal.Length~Species,data = iris)
#Define linear model
#The model for the analysis of variance is specified with the
#lm function (the same for linear regressions!)
model = lm(Sepal.Length ~ Species,data = iris)
#Remember that if the independent variable are of nominal type,
#then the linear regression would become a one-way analysis of variance!!
summary(model) 

#Conduct analysis of variance
#An analysis of variance table is reported as the result of the
#Anova function in the car package.
#By default the Anova function uses type-II sum of squares,
#which are appropriate in common anova analyses.
#This table contains the p-value for each effect tested in the model.
if(!require(car)){install.packages("car")}
anova.model=Anova(model, type = "II")
#Histogram of residuals
x = residuals(model)
plotNormalHistogram(x)
plot(fitted(model),residuals(model))
#Post-hoc analysis:  mean separation tests
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(multcomp)){install.packages("multcomp")}

marginal = lsmeans(model,  ~ Species)
pairs(marginal,
      adjust="tukey")
CLD = cld(marginal,
          alpha   = 0.05,
          Letters = letters,    ### Use lower-case letters for .group
          adjust  = "tukey")    ### Tukey-adjusted comparisons

CLD
#Another way to perform the ANOVA in R, the "aov" function
fm <- aov(Sepal.Length ~ Species, data = iris)
summary(fm)
TPH=TukeyHSD(fm, "Species", ordered = TRUE)
plot(TPH)
plot(CLD)

### Plot
#by using the package ggplot we can perform a series of high quality
#and specific plots
if(!require(ggplot2)){install.packages("ggplot2")}
ggplot(CLD,
       aes(x= Species,y = lsmean,label = .group)) +
  geom_point(shape  = 15,
             size   = 4) +
  geom_errorbar(aes(ymin  =  lower.CL,
                    ymax  =  upper.CL),
                width =  0.2,
                size  =  0.7) +
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  ylab("") +
  geom_text(nudge_x = c(0, 0, 0),
            nudge_y = c(1, 1, 1),
            color   = "black")

#One-way ANOVA w/Block
#The iris dataset does not allow for blocks, since it misses
#a block-type column
#For the purpose of the exercise, we need to create a random column
#of a factor, by using the function "sample"
fakepar=sample(x = c("A","B","C"), # The string to randomize
               size = nrow(iris),  # the size of the vector
               replace = TRUE) # Sampling with replacement
#Now, wee need to add this column to the iris dataset
iris$fakepar=fakepar

model1 = lm(Sepal.Length ~ Species+fakepar,data = iris)
summary(model1)
anova.model1=Anova(model1, type = "II")
x1 = residuals(model1)
plotNormalHistogram(x1)
plot(fitted(model1),residuals(model1))

marginal1 = lsmeans(model1,  ~ Species)
pairs(marginal1,
      adjust="tukey")
CLD1 = cld(marginal1,
           alpha   = 0.05,
           Letters = letters,    ### Use lower-case letters for .group
           adjust  = "tukey")    ### Tukey-adjusted comparisons

CLD1

fm1 <- aov(Sepal.Length ~ Species+fakepar, data = iris)
summary(fm1)
TPH1=TukeyHSD(fm1, "Species", ordered = TRUE)
plot(TPH1)

#A two-way anova can investigate the main effects of each of
#two independent factor variables, as well as the effect of
#the interaction of these variables.

#Define linear model
#Note that the interaction of Species and fakepar is
#indicated in the model definition by Species:fakepar
model2 = lm(Sepal.Length ~ Species + fakepar + Species:fakepar,
            data = iris)
summary(model2)   ### Will show overall p-value and r-square
Anova(model2,type = "II") 

x2 = residuals(model2)
plotNormalHistogram(x2)
plot(fitted(model1),residuals(model1))

marginal2 = lsmeans(model2,  ~ Species)
pairs(marginal2,
      adjust="tukey")
CLD2 = cld(marginal2,
           alpha   = 0.05,
           Letters = letters,    ### Use lower-case letters for .group
           adjust  = "tukey")    ### Tukey-adjusted comparisons

CLD2

fm2 <- aov(Sepal.Length ~ Species + fakepar + Species:fakepar, data = iris)
summary(fm2)
TPH2=TukeyHSD(fm2, "Species", ordered = TRUE)
plot(TPH2)

#Correlation and linear regressions
#Visualizing correlated variables
#The pairs function can plot multiple numeric or integer variables
#on a single plot to look for correlations among the variables.
pairs(data=iris,
      ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width)


#Correlation coefficient can be computed using the functions cor() or cor.test():
#cor() computes the correlation coefficient
#cor.test() test for association/correlation between paired samples.
#It returns both the correlation coefficient and the significance level(or p-value) of the correlation

#As an example, we first subdivide the iris dataset by using the field Species,
#so as to create 3 different datasets for each species
Setosa.dat=iris[iris$Species == "setosa",]
Versicolor.dat=iris[iris$Species == "versicolor",]
Virginica.dat=iris[iris$Species == "virginica",]

cor(Setosa.dat$Petal.Width,Setosa.dat$Sepal.Width,method = c("pearson"))
cor(Setosa.dat$Petal.Width,Setosa.dat$Sepal.Width,method  = c("spearman"))
cor(Setosa.dat$Petal.Width,Setosa.dat$Sepal.Width,method  = c("kendall"))
#cor.test to check the significance of the correlation
cor.test(Setosa.dat$Petal.Width,Setosa.dat$Sepal.Width,method = c("pearson"))
cor.test(Setosa.dat$Petal.Width,Setosa.dat$Sepal.Width,method = c("spearman"))
cor.test(Setosa.dat$Petal.Width,Setosa.dat$Sepal.Width ,method = c("kendall"))
#NOTE. In presence of ties, that is, those pairs of data between samples that have the same ordinal values,
#The spearman correlation gives you a warning, meaning that the correlation (and associated p-value)
#can be computed BUT with some marginal errors.
#Conversely, the Kendall's tau formula tries to mathematically manage their presence and, thus,
#Should be more appropriate in case of an extremely high number of ties in data set.
#If your data contain missing values, use the following R code to handle missing values by case-wise deletion.
cor(Setosa.dat$Petal.Width,Setosa.dat$Sepal.Width,method = "pearson", use = "complete.obs")

#Using rcorr() function in the Hmisc package to test for correlation between each columns of a matrix
if(!require(Hmisc)){install.packages("Hmisc")}

#Imagine we want to check for a correlation between the length and width of both sepal and petal in Iris setosa
setosa.corr=as.matrix(Setosa.dat[,1:4])
sepal_petal_correlation=rcorr(setosa.corr,type="spearman")

#Possible ways to visualize correlations
if(!require(PerformanceAnalytics)){install.packages("PerformanceAnalytics")}
chart.Correlation(Setosa.dat[,1:4],
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
is.list(sepal_petal_correlation)
#TRUE
R_setosa=sepal_petal_correlation$r#with this command we will select the matrix of correlation values
P_setosa=sepal_petal_correlation$P#with this command we will select the matrix of p values
#Now, we can plot our correlation matrix in different ways
corrplot(R_setosa, method="circle")
corrplot(R_setosa, method="pie")
corrplot(R_setosa, method="color")
corrplot(R_setosa, method="number")

#Specialized the insignificant value according to the significant level
corrplot(R_setosa, type="upper", order="hclust", p.mat = P_setosa, sig.level = 0.05,
         insig = "blank")
corrplot(R_setosa, type="upper", order="hclust", p.mat = P_setosa, sig.level = 0.05)

#Linear regression
#Linear regression is a very common approach to model the relationship between two interval/ratio variables. 
#The method assumes that there is a linear relationship between the dependent variable and the independent 
#variable, and finds a best fit model for this relationship.

model.lm = lm(Sepal.Length ~ Petal.Length,
              data = iris)
summary(model.lm)

#Plot data with best fit line
plot(Sepal.Length ~ Petal.Length, 
     data=iris, 
     pch=16,
     xlab = "Petal.Length", 
     ylab = "Sepal.Length")

abline(model.lm,
       col = "blue",
       lwd = 2)
plot(fitted(model.lm), 
     residuals(model.lm))
#Plot of best fit line with confidence interval
ggplot(iris, aes(x=Petal.Length, y=Sepal.Length)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, color='#2C3E50')

