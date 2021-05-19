#Session#4
#Statistical inference#3

#The traditional non parametric tests presented in this book are primarily rank-based tests.
#Instead of using the numeric values of the dependent variable, the dependent variable is
#converted into relative ranks.
#For example, imagine we have the heights of eight students in centimeters.

Height = c(110, 132, 137, 139, 140, 142, 142, 145)
names(Height) = letters[1:8]
Height
rank(Height)

#One-sample Wilcoxon Signed-rank Test
if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(coin)){install.packages("coin")}

#The example answers the question, “Are Maggie’s scores significantly different from a ‘neutral’ score of 3?”
#The test will be conducted with the wilcox.test function, which produces a p-value for the hypothesis,
#as well a pseudo-median and confidence interval.

Input =("
  Speaker          Rater  Likert
 'Maggie Simpson'   1         3 
 'Maggie Simpson'   2         4
 'Maggie Simpson'   3         5
 'Maggie Simpson'   4         4
 'Maggie Simpson'   5         4
 'Maggie Simpson'   6         4 
 'Maggie Simpson'   7         4
 'Maggie Simpson'   8         3
 'Maggie Simpson'   9         2
 'Maggie Simpson'  10         5      
")
Data = read.table(textConnection(Input),header=TRUE)
### Create a new variable which is the likert scores as an ordered factor
Data$Likert.f = factor(Data$Likert,
                       ordered = TRUE)

#Summarize data treating Likert scores as factors
#Note that the variable we want to count is Likert.f, which is a factor variable. 
#Counts for Likert.f are cross tabulated over values of Speaker.
#The prop.table function translates a table into proportions. 

xtabs( ~ Speaker + Likert.f,
       data = Data)
XT = xtabs( ~ Speaker + Likert.f,
            data = Data)
prop.table(XT,
           margin = 1)
XT = xtabs(~ Likert.f, 
           data=Data)

barplot(XT,   
        col="dark gray", 
        xlab="Maggie's Likert",
        ylab="Frequency")

#Summarize data treating Likert scores as numeric
Summarize(Likert ~ Speaker,                                                     
          data=Data, 
          digits=3)
wilcox.test(Data$Likert, 
            mu=3,
            conf.int=TRUE,
            conf.level=0.95)
#Effect size
wilcoxonOneSampleR(Data$Likert, 
                   mu=3)

#One more example
if(!require(semTools)){install.packages("semTools")}
#the function mvrnonnorm will generate a two column matrix of non normal data
test=mvrnonnorm(20, c(1, 2), matrix(c(10, 2, 2, 5), 2, 2),
                skewness = c(5, 2), kurtosis = c(3, 3))[,1]
names(test) = letters[1:20]

plotNormalHistogram(test)
rank(test)
wilcox.test(test,conf.int=TRUE,conf.level=0.95)

#Two-sample Mann–Whitney U Test
#The two-sample Mann–Whitney U test is a rank-based test that compares values for two groups. 
#A significant result suggests that the values for the two groups are different.
#It is equivalent to a two-sample Wilcoxon rank-sum test.
#Indeed, in R, the syntax for the 2 sample Mann-Whitney U test is the same as the Wilcoxon rank-sum test
#Let's use again the iris dataset. We want to test if the values for the sepal length in setosa and virginica
#are different...
#subset the iris dataset by species (another way to do it...)
#We can extract information by species (setosa & virginica) and then bind togheter 
Setosa  = iris[iris$Species=="setosa",]
Virginica  = iris[iris$Species=="virginica",]
Dat=rbind(Setosa,Virginica)#rbind allows to unify two data frames or matrix BY ROWS. Data frames should have
#exactly the same number of columns!
wilcox.test(Sepal.Length~Species,data=Dat,conf.int=TRUE,conf.level=0.95)

#Kruskal–Wallis Test
#The Kruskal–Wallis test is a rank-based test that is similar to the Mann–Whitney U test,
#but can be applied to one-way data with more than two groups.
#Bar plots of data by group
histogram(~ Sepal.Length | Species,
          data=iris,
          layout=c(1,3)      #  columns and rows of individual plots
)
kruskal.test(Sepal.Length ~ Species,data = iris)
#Effect size
#Statistics of effect size for the Kruskal–Wallis test provide the degree
#to which one group has data with higher ranks than another group.
#They are related to the probability that a value from one group will be
#greater than a value from another group.
#Unlike p-values, they are not affected by sample size.
#epsilon-squared
epsilonSquared(x = iris$Sepal.Length,g = iris$Species)
#Freeman’s theta
freemanTheta(x = iris$Sepal.Length,g = iris$Species)

#Post-hoc test: Dunn test for multiple comparisons of groups
#If the Kruskal–Wallis test is significant, a post-hoc analysis
#can be performed to determine which groups differ from each other group. 
DT = dunnTest(Sepal.Length ~ Species,
              data=iris,
              method="bh")      # Adjusts p-values for multiple comparisons;
                                # See ?dunnTest for options

### Compact letter display
PT = DT$res
cldList(P.adj ~ Comparison,data = PT,threshold = 0.05)

#Plot of medians and confidence intervals
#The following code uses the groupwiseMedian function to produce a
#data frame of medians for each speaker along with the 95% confidence intervals
#for each median with the percentile method.  These medians are then plotted,
#with their confidence intervals shown as error bars.
#The grouping letters from the multiple comparisons are added.

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

#Friedman Test
#The Friedman test determines if there are differences among groups for two-way
#data structured in a specific way, namely in an unreplicated complete block design.
#In this design, one variable serves as the treatment or group variable, and another
#variable serves as the blocking variable.  It is the differences among treatments or
#groups that we are interested in.
#Friedman test example
#This example uses the formula notation indicating that sepal length is the dependent
#variable and Species is the independent variable.
#However, we need to create a fake parameter describing an hypothetic
#blocking variable.

#Aligned Ranks Transformation ANOVA
#Aligned ranks transformation ANOVA (ART anova) is a nonparametric approach that
#allows for multiple independent variables, interactions, and repeated measures
if(!require(ARTool)){install.packages("ARTool")}
#Midichlorians example
#This example reproduces the data used in the Scheirer–Ray–Hare Test chapter. 
### Assemble the data
Location = as.factor(c(rep("Olympia" , 6), rep("Ventura", 6), 
                       rep("Northampton", 6), rep("Burlington", 6)))
Tribe  = as.factor(c(rep(c("Jedi", "Sith"), 12)))
Midichlorians = c(10,  4, 12,  5, 15,  4, 15,  9, 15, 11, 18, 12, 
                  8, 13,  8, 15, 10, 17, 22, 22, 20, 22, 20, 25)
Data = data.frame(Tribe, Location, Midichlorians)
str(Data)
#ART model
model = art(Sepal.Length ~ Species + as.factor(fakepar) + Species:as.factor(fakepar),
            data = iris)
### Check the success of the procedure
anova(model)
#Post-hoc comparisons for main effects
model.lm = artlm(model, "Species")
if(!require(emmeans)){install.packages("emmeans")}
marginal = emmeans(model.lm, ~ Species)
pairs(marginal,adjust = "tukey")
cld(marginal,alpha=0.05,Letters=letters,adjust="tukey")

