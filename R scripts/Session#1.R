#DATA ANALYSIS WITH R
#SESSION#1

#Install packages
#The vegan package provides tools for descriptive community ecology.
#It has most basic functions of diversity analysis, community ordination and
#dissimilarity analysis. Most of its multivariate tools can be used for other
#data types as well.

#1. Installing by console
install.packages("vegan")
#2. Installing via the "packages" tab in the "Utility panel"

#Once a specific package has been installed, we need to recall it
#we will use the function "library"
library(vegan)
#Let's have a look to the help
#we can use the function "help"
help("vegan")
#we can use the symbol "?"
?vegan
#..or, we can use the "help" Tab on the Utility panel and search for the
#package's help

#Load dataset
#The "iris" dataset.
#This famous (Fisher's or Anderson's) iris data set gives the measurements
#in centimeters of the variables sepal length and width and petal length and width,
#respectively, for 50 flowers from each of 3 species of iris.
#The species are Iris setosa, versicolor, and virginica.
#iris loads automatically in R, being one of the most popular dataset on which to
#learn the use of R for statistical analysis.

#Anyway, for the purpose of this tutorial, will load the iris dataset
#from a directory we previously set. This is because the script and the data MUST be
#on the same directory.

#We will use different data type and functions.
#1. loading a .txt file
iris.txt=read.table("iris.txt",sep="",dec=".")
#2. loading a .csv file
iris.csv=read.csv("iris.csv",sep=",",dec=".")
#3. loading an excel file (.xls or .xlsx)
#To be able to load an excel file we need to install and load a specific library
#called "readxl"
install.packages("readxl")
library(readxl)
iris.xls=read_xls("iris.xls")#this will load the entire file
#If we need to load a specific sheet, we should use the argument
#"sheet", specifying the number of the sheet (1,2,3,..) or, eventually, its name
iris.xls=read_xls("iris.xls",sheet=1)
iris.xls.2=read_xls("iris.xls",sheet=2)

#When loading an excel file with "read_xls", R automatically gives back a "tibble" object
#which is a nicest way to format a data frame
#To convert this object to a data frame we can use the function "as.data.frame"
as.data.frame(iris.xls)#!If we want to make as a permanent object, we must assign a name

#If needed, we can explore the data structure of the dataset, by using the function "str"
str(iris.txt)
str(iris.csv)
str(iris.xls)

#We can test the type of data structure by using the function "is.(matrix OR dataframe OR list)"
is.matrix(iris.txt)
is.matrix(iris.csv)
is.matrix(iris.xls)
is.data.frame(iris.txt)
is.data.frame(iris.csv)
is.data.frame(iris.xls)
#The function gives back a logic value TRUE/FALSE, depending on the nature of the dataset

#Depending on our needs, we can decide to split out our dataset to create a matrix
#where ALL the values MUST be of the same type (numbers, text, categories and so on)
#we can use the function "as.matrix()"
iris.txt.matrix=as.matrix(iris.txt[,1:3])
#In the above function we use the syntax "iris.txt[,1:3]" to select the range of columns
#from 1 to 3. In R a data frame or matrix object is always given in the form
#data[rows,columns]
#To select a specific range of rows we then can use data[rows-from:to,]
is.matrix(iris.txt.matrix)#with this we can check whether the new object called "iris.txt.matrix"
#IS or IS NOT a matrix

#Data types
#Lists
#R list is the object which contains elements of different types – like strings,
#numbers, vectors and another list inside it. R list can also contain a matrix
#or a function as its elements. The list is created using the list() function in R.
#In other words, a list is a generic vector containing other objects.
#Let's create a list composed by the first two  columns of the iris dataset
colnames(iris)#this function allows showing the names of the columns
pts <- list(x=iris$Sepal.Length, y = iris$Sepal.Width)
pts2 <- list(x=iris$Sepal.Length, y = iris$Sepal.Width,z=iris$Petal.Length)

#Arrays
#An array in R can have one, two or more dimensions.
#It is simply a vector which is stored with additional attributes
#giving the dimensions (attribute "dim") and optionally names for those
#dimensions (attribute "dimnames").
#Select two columns from the iris dataset
#Create two vectors by selecting two of the three columns
v1<-iris$Sepal.Length
v2<-iris$Sepal.Width
final = array (c (v1, v2),dim =c(4,4,3))#Take two vectors above as an input to an array
#where the dimension is considered as 4 * 4, and two matrices or dimensional data is created.

#Now that we learned how to import data, let's make some basic maths
#For this, we will use the iris datasets, as we would need number-only variables..

#Functions
#Functions are fundamental to any programming language,
#and R is no different. There are three main sources for functions.
#Defined in Base R
#There are many-many of these, and we will touch on some common ones throughout this class.
#Consider the code below:
sum(v1)#this function will perform the same over v1

#Defined in Packages
#When you install a package, what you’re really doing is adding more
#functions to the universe in which you are working

#Defined in Your Script
#Both to organize your code and, more importantly,
#to prevent repeating the same code within a single script,
#you can define functions within your script that you then call from elsewhere in the script.
# Define a function called 'cubeValue'
cubeValue <- function(x){
  cat("The cube of ",x," is ",x^3,".\n",sep="")
}
# Loop through the numbers 1 through 3 and print the cube of each.
for (i in 1:3){
  cubeValue(i)
}

#Maths on two vectors
v1+v2#sum
v1*v2#multiplication
v1/v2#division
v1-v2#subtraction
sqrt(v1)#square root
#Writing math functions in R follows the usual syntax, with operators following the order given by
#parentheses
(v1+v2)*v1
v1+v2*v1

#Type of variables
#Let's have a look to different type of variables in the dataset
class(iris)
class(iris$Sepal.Length)
class(iris$Species)

#Basic plots
#1.Strip chart
#A strip chart is the most basic type of plot available.
#It plots the data in order along a line with each data point represented as a box. 
stripchart(iris$Sepal.Length)
#2.Histograms
#A histogram is very common plot. It plots the frequencies that data appears within certain ranges. 
hist(iris$Sepal.Length)
#3.Boxplots
#A boxplot provides a graphical view of the median, quartiles, maximum, and minimum of a data set. 
boxplot(iris$Sepal.Length)
#4.Scatterplot
#A scatter plot provides a graphical view of the relationship between two sets of numbers. 
plot(iris$Sepal.Length,iris$Sepal.Width)#..or
plot(pts)
#5.Normal QQ plot
#This plot is used to determine if your data is close to being normally distributed.
#You cannot be sure that the data is normally distributed, but you can rule out
#if it is not normally distributed.
qqnorm(iris$Sepal.Length)

#Customizing plot
#When plotting, we can customize the color, the width of the plotting window and many other features#
#This will allow us to obtain a pretty nice figure, which we can use as it is for our purposes
#or to save in a vector graphic (e.g., pdf, eps or svg) and then manipulate for a better aspect
#in other vector-graphic softwares (e.g., Inkscape)
plot(iris$Sepal.Length,iris$Sepal.Width,pch=19)#pch determines the shape of the points
plot(iris$Sepal.Length,iris$Sepal.Width,pch=12)
plot(iris$Sepal.Length,iris$Sepal.Width,pch=2)
?pch
plot(iris$Sepal.Length,iris$Sepal.Width,pch=19,col="red")#col determines the color of the points
plot(iris$Sepal.Length,iris$Sepal.Width,pch=2,col="red")#col determines the color of the points
plot(iris$Sepal.Length,iris$Sepal.Width,pch=19,col="red",xlab="Hello!",ylab="Hi!",main="Test Plot")
#xlab, ylab and main set the axis labels and the main title of the plot (if needed)
plot(iris$Sepal.Length,iris$Sepal.Width,pch=19,col="red",xlab="Hello!",ylab="Hi!",main="Test Plot",
     las=1)#las sets the direction of the axis plot


#How to make composite pictures
#Sometimes could be useful to create composite figures, plotting for instance two scatterplots at once
par(mfrow=c(1,2))#mfrow=c(1,2) determines how many rows (1) and columns (2) the plot areas should be
#divided into
plot(iris$Sepal.Length,iris$Sepal.Width,pch=19,col="red",xlab="Hello!",ylab="Hi!",main="Test Plot")
plot(iris$Sepal.Length,iris$Sepal.Width,pch=4,col="red",xlab="Hello!",ylab="Hi!",main="Test Plot2")
#We can also decide to allow the plots to be perfectly squared
par(pty="s",mfrow=c(1,2))#pty is a character specifying the type of plot region to be used;
#"s" generates a square plotting region and "m" generates the maximal plotting region. 
plot(iris$Sepal.Length,iris$Sepal.Width,pch=19,col="red",xlab="Hello!",ylab="Hi!",main="Test Plot")
plot(iris$Sepal.Length,iris$Sepal.Width,pch=4,col="red",xlab="Hello!",ylab="Hi!",main="Test Plot2")

par(pty="m",mfrow=c(1,2))#pty is a character specifying the type of plot region to be used;
#"s" generates a square plotting region and "m" generates the maximal plotting region. 
plot(iris$Sepal.Length,iris$Sepal.Width,pch=19,col="red",xlab="Hello!",ylab="Hi!",main="Test Plot")
plot(iris$Sepal.Length,iris$Sepal.Width,pch=4,col="red",xlab="Hello!",ylab="Hi!",main="Test Plot2")

par(pty="m",mfrow=c(1,2),cex=2)#cex is a magnifier parameter, to allow the elements omn the plot to 
#scale in magnitude (bigger or lower)
plot(iris$Sepal.Length,iris$Sepal.Width,pch=19,col="red",xlab="Hello!",ylab="Hi!",main="Test Plot")
plot(iris$Sepal.Length,iris$Sepal.Width,pch=4,col="red",xlab="Hello!",ylab="Hi!",main="Test Plot2")


