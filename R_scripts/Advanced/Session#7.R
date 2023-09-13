#DATA ANALYSIS WITH R
#SESSION#7 - Multivariate analysis#2

#Unconstrained ordination
#NMDS is a tool to assess similarity between samples when considering multiple
#variables of interest. It is analogous to Principal Component Analysis (PCA) 
#with respect to identifying groups based on a suite of variables. 
#Unlike PCA though, NMDS is not constrained by assumptions of multivariate normality 
#and multivariate homoscedasticity.
# First load the vegan package
library(vegan)
nmds_results <- metaMDS(comm = iris[,1:4],  # Define the community data 
                        distance = "euclidean",       # Specify a bray-curtis distance
                        try = 100)               # Number of iterations 
stressplot(nmds_results)
#Plot the result
# First create a data frame of the scores from the individuals.
# This data frame will contain x and y values for where sites are located.
data_scores <- as.data.frame(scores(nmds_results$points))
# Now add the extra Species column
data_scores <- cbind(data_scores, iris$Species)
colnames(data_scores)[3] <- "Species"
plot(data_scores[,1:2],col=data_scores$Species,pch=19)
legend(-1,1,"pippo")
#Principal Component Analysis (PCA)
#Principal Component Analysis (PCA) is a useful technique for exploratory data analysis,
#allowing you to better visualize the variation present in a dataset with many variables.
#It is particularly helpful in the case of "wide" datasets, where you have many variables for each sample.
str(mtcars)
#We will use the mtcars dataset, which is built into R. This dataset consists of data on 32 models of car,
#taken from an American motoring magazine (1974 Motor Trend magazine). For each car, you have 11 features,
#expressed in varying units (US units), 
#Because PCA works best with numerical data, we'll exclude the two categorical variables (vs and am).
dat.select=data.frame(mtcars[,1:7],mtcars[,10:11])
mtcars.pca <- prcomp(dat.select,
                     center = TRUE,#logical value indicating whether the variables should be shifted to be zero centered.
                     scale. = TRUE)#logical value indicating whether the variables should be scaled to have unit variance before the analysis takes place.
summary(mtcars.pca)
#9 principal components, which we call PC1-9. Each of these explains a percentage of the total variation in the dataset.
#That is to say: PC1 explains ca 63% of the total variance, which means that nearly two-thirds of the information in the dataset
#(9 variables) can be encapsulated by just that one Principal Component. PC2 explains 23% of the variance.
#So, by knowing the position of a sample in relation to just PC1 and PC2, you can get a very accurate view on where it stands
#in relation to other samples, as just PC1 and PC2 can explain 86% of the variance.
screeplot(mtcars.pca)#this function allows visually exploring the relative contribution of each PCs
#Different ways to plot a PCA
#The basic way, by using the function biplot
biplot(mtcars.pca)
#Or we can use the the ggbiplot package, which offers a user-friendly and pretty function to plot biplots.
#Here we will introduce devtools. 
#The aim of devtools is to make package development easier by providing R functions that simplify and expedite common tasks.
# Install devtools from CRAN
install.packages("devtools")
library(devtools)

install_github("vqv/ggbiplot",type="source")#DEPRECATED_________________________
library(ggbiplot)#DEPRECATED____________________________________________________
ggbiplot(mtcars.pca)#DEPRECATED_________________________________________________
ggbiplot(mtcars.pca, labels=rownames(mtcars))#DEPRECATED________________________

#Interpreting the results
#Maybe if you look at the origin of each of the cars. You'll put them into one of three categories,
#one each for the US, Japanese and European cars. You make a list for this info, then pass it to the groups argument of ggbiplot.
#You'll also set the ellipse argument to be TRUE, which will draw an ellipse around each group.
mtcars.country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))
ggbiplot(mtcars.pca,ellipse=TRUE,  labels=rownames(mtcars), groups=mtcars.country)

library(factoextra)#NEW SOLUTION________________________________________________
factoextra::fviz_pca_ind(mtcars.pca,#NEW SOLUTION_______________________________
             habillage=mtcars.country,#NEW SOLUTION_____________________________
             addEllipses=TRUE)#NEW SOLUTION_____________________________________

#the American cars form a distinct cluster to the right. Looking at the axes, you see that the American cars are characterized by
#high values for cyl, disp, and wt. Japanese cars, on the other hand, are characterized by high mpg. European cars are somewhat in
#the middle and less tightly clustered than either group.

#Constrained ordination
#Constrained analysis is a form of direct gradient analysis, which attempts to explain variation in a data
#table directly through the variation in a set of explanatory variables  (e.g. environmental factors) stored
#in a corresponding table or tables.

#Canonical Correspondence Analysis (CCA)
#we will use the built-in datasets varespec and varechem in the vegan package
#varespec = estimated cover values of 44 vegetation species in lichen pastures
data(varespec)
#varechem = soil characteristics of the very same sites as in the varespec data frame.
data(varechem)

#Similarly to Principal Component Analysis (PCA), it is an multivariate ordination analysis which uses variables
#from a single data set to find new variables or axes (orthogonal components) that are most able to explain the data set.
#However, the PCA assumes that observations (for example, species distributions) are linearly (or monotonically)
#related to each other and to the gradients (for example, environmental variables). 
#On the other hand, the CA expects observations (for example, species distributions) to have unimodal distributions
#along the gradients (for example, environmental variables). Notably, this is usually the case for species distributions
#(see the figure below). A species usually shows highest abundance not in the lowest or highest value of an environmental
#gradient, but usually near an intermediate level. This is why the CA (and also CCA) are so used by ecologists

#To perform CCA, we need to specify that species distribution matrix is explained by environmental matrix.

ccamodel <- cca(varespec~., #by using the point after the tilde we will tell R to use ALL the variables in the environmental
                            #matrix
                data=varechem)
summary(ccamodel)
screeplot(ccamodel)#importance of components
plot(ccamodel)

## Note that "Total Inertia" is the total variance in species (observations matrix) distributions.
#"Constrained Inertia" is the variance explained by the environmental variables (gradients matrix).
#The "Proportion" values represent the percentages of variance of species distributions explained by Constrained (environmental)
#and Unconstrained variables.
#Eigenvalues of constrained and unconstrained axes represent the amount of variance explained by each CCA axis
#(graphs usually present the first two constrained axes, so take a look at their values).

#But...are we sure we need  ALL the env parameters to explain the observed variation in the community data matrix?
#In R, we can automatically select those env variables that best explain the species matrix.
#We can do that by using a stepwise model from "ordistep" function.
finalmodel<- ordistep(ccamodel, scope=formula(ccamodel))
#However, we have to deal with something unwanted in constrained ordination: collinearity.
#Collinearity (or Multicollinearity when referred to multiple variables) occurs when independent
#variables in a regression model are correlated. This correlation is a problem because independent
#variables should be independent. If the degree of correlation between variables is high enough, it
#can cause problems when you fit the model and interpret the results.
#To identify which variables in CCA is highly collinear, we can calculate Variance Inflation Factors (VIF)
#for each of the constraints (variables) from the 
#environmental matrix. If we find an environmental variable with VIF>10, 
#we'll know that this variable presents collinearity with another or other variables.
#In that case, we would have to delete the variable from our initial dataset and redo all the analysis.
vif.cca(finalmodel)#it will measure collinearity by using the  variance inflation factors 
#three variables, K, S and Al have high VIF
#Usually, we could check previously how much variables are correlated to each other.
#We can simply correlate variables and decide to exclude those having an r>0.7. 
#Notice that the absolute value of r is completely arbitrary. As a matter of facts, the higher the correlation,
#the higher the probability for a variable to be collinear.
cor(varechem)
pairs(varechem)
#Repeat the analysis by removing collinear variables.
new.env=varechem[ , -which(names(varechem) %in% c("K","S","Al"))]#with this function we will exclude specific columns from the data frame
ccamodel.new <- cca(varespec~.,  new.env)
finalmodel.new<- ordistep(ccamodel.new, scope=formula(ccamodel.new))
# Testing the significance of the CCA model:
anova.cca(finalmodel)

# Testing the significance of terms (environmental variables):
anova.cca(finalmodel, by="terms")
anova.cca(finalmodel.new, by="terms")
# Testing the significance of CCA axes (at least the first two or three should present a significant p value):
anova.cca(finalmodel, by="axis")
anova.cca(finalmodel.new, by="axis")

par(mfrow=c(1,2))
plot(finalmodel)
plot(finalmodel.new)

#Redundancy Analysis (RDA)
#Choosing between CCA and RDA to biodiversity studies should be based on the type of
#response you expect from the biodiversity matrix. If we expected linear responses,
#we should chose RDA. If we expect unimodal responses, we should chose CCA.
#Of course, we can transform a non-linear data and use the RDA,
#which usually has few problems than CCA.
