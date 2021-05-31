#DATA ANALYSIS WITH R
#SESSION#6 - Multivariate analysis#1

#Dissimilarity/similarity matrix
#Broadly speaking, multivariate patterns amongst objects can either 
#be quantified on the basis of the associations (correlation or covariance)
#between variables (species) on the basis of similarities between objects.
#The former are known as R-mode analyses and the later Q-mode analyses.
#Consider the following fabricated data matrices.
#The Y matrix on the left consists of four species abundances from five sites.
#The X matrix on the right represents five environmental measurements
#(concentrations in mg/L) from five sites.
Y <- matrix(c(
  + 2,0,0,5,
  + 13,7,10,5,
  + 9,5,55,93,
  + 10,6,76,81,
  + 0,2,6,0)
,5,4,byrow=TRUE)
colnames(Y) <- paste("Sp",1:4,sep="")
rownames(Y) <- paste("Site",1:5,sep="")

E <- matrix(c(
  + 0.2,0.5,0.7,1.1,
  + 0.1,0.6,0.7,1.3,
  + 0.5,0.6,0.6,0.7,
  + 0.7,0.4,0.3,0.1,
  + 0.1,0.4,0.5,0.1
  ),5,4,byrow=TRUE)
colnames(E) <- paste("Conc",1:4,sep="")
rownames(E) <- paste("Site",1:5,sep="")

#Measures of association
#sums-of-squares-and-cross-products (SSCP) matrix is a symmetrical diagonal
#matrix with sums of squares of each variable on the diagonals and sums of
#cross products on the off-diagonals. Alternatively, the SSCP values can be
#calculated as the cross-products of centered variables.
crossprod(scale(Y,scale=FALSE))
#variance covariance matrix. The SSCP values can be converted to average differences
#through division by independent sample size (df).
#The variance-covariance matrix is a symmetrical diagonal matrix with variance of
#each variable on the diagonals and covariances on the off-diagonals.
#A variance covariance matrix is calculated by dividing the
#sums-of-squares-and-cross-products by the degrees of freedom
#(number of observations n minus 1).
var(Y)
#correlation matrix.
#The variance-covariance matrix can be standardized
#(values expressed on a scale independent of the scale of the original data)
#into a correlation matrix by dividing the matrix elements by the standard deviations
#of the constituting variables.
cor(Y)
#Measures of distance
#Measures of distance (or resemblance) between objects reflect the degree of
#similarity between pairs of objects. Intuitively, small values convey small
#degrees of difference between things. Hence distances are usually expressed 
#as dissimilarity rather than similarity. A small value of dissimilarity
#(large degree of similarity) indicates a high degree of resemblance between two objects.
#There are a wide range of distance measures, each of which is suited to
#different circumstances and data. Most of these dissimilarities are supported
#via the vegdist() function of the vegan package.
#In the following j and k are the two objects (rows) being compared and i refers
#to the variables (columns).

#Euclidean distance represents the geometric distance between two points in
#multidimensional space. Euclidean distance is bounded by zero when two objects
#have identical variable values. However, there is no upper bound and the magnitude
#of the values depends on the scale of the observations as well as the sample size.
#Euclidean distance is useful for representing differences of purely measured variables
#(of similar scale), for which the simple geometric distances do have real meaning.
#However it is not well suited to data such as species abundances
#(without prior standardizations) due to its lack of a maximum and its high 
#susceptibility to large differences (due to being based on squared differences).
if(!require(vegan)){install.packages("vegan")}
vegdist(Y,method="euclidean")
vegdist(E,method="euclidean")
#Note:
#counter intuitively, sites 1 and 5 of the species abundances are considered the most similar - not desirable as they have nothing in common
#sites 1 and 5 have low species counts and therefore low distances - not desirable for abundance data
#sites 1 and 2 in the environmental data are considered the most similar and are separated by 0.245 units (mg/L)

#χ2 distance is essentially the euclidean distances of relative abundances
#(frequencies rather than raw values) weighted (standardized) by the square root
#of the inverse of column sums and multiplied by the square root of the total abundances.
#Since χ2 distance works on frequencies, it is only relevant for abundance data
#for which it is arguably more appropriate than euclidean distances
#(due to the non-linearity of species abundances).
#As a result of working with relative abundances (frequencies), all sites and
#species are treated equally - that is, unlike the related euclidean distance,
#the distance values are not dependent on absolute magnitudes.
dist(decostand(Y,method="chi"))
#Note:
#sites 3 and 4 are considered the most similar and sites 1 and 5 the most dissimiliar
#(consistent with expectations).
#the units of the distances don't have any real interpretation

#Hellinger distance is essentially the euclidean distances of square root
#relative abundances (frequencies rather than raw values).
#Square rooting the frequencies reduces the impacts of relatively abundant species.
#Like χ2 distance, the Hellinger distance works on frequencies and therefore is only
#relevant for abundance data. A Hellinger transformation can be a useful preparation
#of species abundance data where the abundances are expected to by unimodal.
dist(decostand(Y,method="hellinger"))
#Note:
#sites 3 and 4 are considered the most similar and sites 1 and 5 the most dissimiliar
#(consistent with expectations).
#the units of the distances don't have any real interpretation

#Bray-Curtis dissimilarities are considered most appropriate for species abundance
#data as they:
#reach a maximum value of 1 when two objects have nothing in common
#ignores joint absences (0's)
#Nevertheless, it is predominantly influenced by large values, and therefore
#standardizations are recommended prior to generating a Bray-Curtis dissimilarity.
vegdist(Y,method="bray")
vegdist(E,method="bray")

#As a rule of thumb, when working with species presence/abyndance data, Euclidean distance
#should be avoided. Better using the bray curtis one.
#Conversely, Euclidean distance works well with environmental data.

#When species data are given as presence/absence (which is the norm in many studies)
#The use of Jaccard distance is reccomended
vegdist(Y,method="jaccard")

#The dist object in R is a particular type of data. It is not a matrix, nor a data frame
#object.
#Sometimes we need to convert the dist object in a matrix to use specific functions.

#Let's get back to the iris dataset
data(iris)#with this command we will recall the dataset from scratch.
iris
#Let's calculate the distance between individuals of different species based on the
#sepal and petal length
#We will use a the gowdis measures (Gower 1971) of dissimilarity for mixed variables,
#which is able to handle both quantitative and qualitative variables.
#This function is particularly efficient when working with species traits data
#The function gowdis is in the FD package
if(!require(FD)){install.packages("FD")}
?gowdis#take a look to the help
GD=gowdis(data.frame(iris$Sepal.Length,iris$Petal.Length))

#We can visualize our result by using the function heatmap(), which uses distance matrices
#to create a false color image with a dendrogram added to the left side and to the top
heatmap(GD)
heatmap(as.matrix(GD))

#This introduces the:
#Cluster analysis
#Clustering algorithms group a set of data points into subsets or clusters.
#The algorithms' goal is to create clusters that are coherent internally,
#but clearly different from each other externally. In other words, 
#entities within a cluster should be as similar as possible and entities in one cluster
#should be as dissimilar as possible from entities in another.

#Broadly speaking there are two ways of clustering data points based on
#the algorithmic structure and operation, namely agglomerative and divisive.
#Agglomerative : An agglomerative approach begins with each observation
#in a distinct (singleton) cluster, and successively merges clusters together
#until a stopping criterion is satisfied.
#Divisive : A divisive method begins with all patterns in a single cluster
#and performs splitting until a stopping criterion is met.

#Agglomerative or bottom-up approach starts with each data point as its own cluster
#and then combine clusters based on some similarity measure.
#The idea can be easily adapted for divisive methods as well.
#The similarity between the clusters is often calculated from the dissimilarity
#measures like the euclidean distance between two clusters.
#So the larger the distance between two clusters, the better it is.
#There are many distance metrics that you can consider to calculate the dissimilarity
#measure, and the choice depends on the type of data in the dataset.
#For example if you have continuous numerical values in your dataset you can use
#euclidean distance, if the data is binary you may consider the Jaccard distance
#(helpful when you are dealing with categorical data for clustering after you have
#applied one-hot encoding).

#Pre-processing operations for Clustering
#There are a couple of things you should take care of before starting.
#Scaling
#It is imperative that you normalize your scale of feature values in order
#to begin with the clustering process. This is because each observations'
#feature values are represented as coordinates in n-dimensional space
#(n is the number of features) and then the distances between these coordinates
#are calculated. If these coordinates are not normalized, then it may lead to false
#results.

#Dendrograms
#In hierarchical clustering, you categorize the objects into a hierarchy similar
#to a tree-like diagram which is called a dendrogram. The distance of split or merge
#(called height) is shown on the y-axis.
#One question that might have intrigued you by now is how do you decide when to stop
#merging the clusters? Well, that depends on the domain knowledge you have about the data.
#But sometimes you don't have that information too. In such cases, you can leverage
#the results from the dendrogram to approximate the number of clusters. You cut the
#dendrogram tree with a horizontal line at a height where the line can traverse the
#maximum distance up and down without intersecting the merging point.

#Measuring the goodness of Clusters
#Perhaps the most important part in any unsupervised learning task is the analysis
#of the results. After you have performed the clustering using any algorithm and any
#sets of parameters you need to make sure that you did it right. But how do you
#determine that?
#Well, there are many measures to do this, perhaps the most popular one is the 
#Dunn's Index. Dunn's index is the ratio between the minimum inter-cluster distances
#to the maximum intra-cluster diameter. The diameter of a cluster is the distance between
#its two furthermost points. In order to have well separated and compact clusters you
#should aim for a higher Dunn's index

#We will apply hierarchical clustering on the seeds dataset.
#This dataset consists of measurements of geometrical properties of
#kernels belonging to three different varieties of wheat: Kama, Rosa and Canadian.
#It has variables which describe the properties of seeds like area, perimeter,
#asymmetry coefficient etc. There are 70 observations for each variety of wheat.
set.seed(786)
file_loc <- 'seeds_dataset.txt'
seeds_df <- read.csv(file_loc,sep = '\t',header = FALSE)
#Since the dataset doesn't have any column names you will give columns name yourself from the data description.
feature_name <- c('area','perimeter','compactness','length.of.kernel','width.of.kernal','asymmetry.coefficient','length.of.kernel.groove','type.of.seed')
colnames(seeds_df) <- feature_name
#It's advisable to gather some basic useful information about the dataset like
#its dimensions, data types and distribution, number of NAs etc.
#You will do so by using the str(), summary() and is.na() functions in R.
str(seeds_df)
summary(seeds_df)
any(is.na(seeds_df))
#You will now store the labels in a separate variable and exclude the type.of.seed column
#from your dataset in order to do clustering.
#Later you will use the true labels to check how good your clustering turned out to be.
seeds_label <- seeds_df$type.of.seed
seeds_df$type.of.seed <- NULL
str(seeds_df)
#Now we will use R's scale() function to scale all your column values.
seeds_df_sc <- as.data.frame(scale(seeds_df))
summary(seeds_df_sc)
#Since all the values here are continuous numerical values, we will use the
#euclidean distance method.
dist_mat <- dist(seeds_df_sc, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)
#The problem here is that we have missin (Not Available, NA) values in our dataset.
#We therefore should substitute this values with something else, e.g., 0
dist_mat[is.na(dist_mat)]=0
hclust_avg <- hclust(dist_mat, 
                     method = 'average')#with the method parameters we can change
                                        #the type agglomeration method to be used
plot(hclust_avg)

hclust_sing <- hclust(dist_mat, 
                     method = 'single')#use of single agglomerative method
hclust_comp <- hclust(dist_mat, 
                      method = 'complete')#use of complete agglomerative method

#Let's plot all togheter to see (if any) the differences
par(mfrow=c(3,1))
plot(hclust_avg)
plot(hclust_sing)
plot(hclust_comp)

#Next, we can cut the dendrogram in order to create the desired number of clusters.
#Since in this case we already know that there could be only three types of wheat
#we will choose the number of clusters to be k = 3, or as we can see in the dendrogram
#h = 3 you get three clusters. We will use R's cutree() function to cut the tree with
#hclust_avg as one parameter and the other parameter as h = 3 or k = 3.
cut_avg <- cutree(hclust_avg, k = 3)
plot(hclust_avg)
rect.hclust(hclust_avg , k = 3, border = 2:6)
#Now we can see the three clusters enclosed in three different colored boxes.
#We can also use the color_branches() function from the dendextend library to visualize
#our tree with different colored branches.
if(!require(dendextend)){install.packages("dendextend")}
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)
#Now we will append the cluster results obtained back in the original dataframe
#under column name the cluster with mutate(), from the dplyr package and count how
#many observations were assigned to each cluster with the count() function.
if(!require(dplyr)){install.packages("dplyr")}
seeds_df_cl <- mutate(seeds_df, cluster = cut_avg)
count(seeds_df_cl,cluster)

#Non hierarchical clustering
#K-means
#K-means clustering is the most commonly used unsupervised machine learning
#algorithm for dividing a given dataset into k clusters. Here, k represents
#the number of clusters and must be provided by the user.
#Here, we will  need to load the following packages:
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(cluster)){install.packages("cluster")}
if(!require(factoextra)){install.packages("factoextra")}
#Here, we’ll use the built-in R data set USArrests, which contains statistics
#in arrests per 100,000 residents for assault, murder, and rape in each of the
#50 US states in 1973. It includes also the percent of the population living in urban areas
df=USArrests
#To remove any missing value that might be present in the data, type this:
df <- na.omit(df)
#As we don’t want the clustering algorithm to depend to an arbitrary variable unit,
#we start by scaling/standardizing the data using the R function scale:
df <- scale(df)
head(df)
#Clustering Distance Measures
#The classification of observations into groups requires some methods for computing
#the distance or the (dis)similarity between each pair of observations.
#The result of this computation is known as a dissimilarity or distance matrix. 
#The choice of distance measures is a critical step in clustering.
#It defines how the similarity of two elements (x, y) is calculated and it will
#influence the shape of the clusters.
#For most common clustering software, the default distancemeasure is the Euclidean
#distance. However, depending on the type of the data and the research questions,
#other dissimilarity measures might be preferred and you should be aware of the options.
#Within R it is simple to compute and visualize the distance matrix using the functions
#get_dist and fviz_dist from the factoextra R package. 
#This starts to illustrate which states have large dissimilarities (red) versus those 
#that appear to be fairly similar (teal).
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
#Computing k-means clustering in R
#We can compute k-means in R with the kmeans function. Here will group the data into
#two clusters (centers = 2). The kmeans function also has an nstart option that attempts 
#multiple initial configurations and reports on the best one. 
#For example, adding nstart = 25 will generate 25 initial configurations. 
#This approach is often recommended.
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
k2
#We can also view our results by using fviz_cluster. 
#This provides a nice illustration of the clusters. 
#If there are more than two dimensions (variables) fviz_cluster 
#will perform principal component analysis (PCA) and plot the data points 
#according to the first two principal components that explain the 
#majority of the variance.
fviz_cluster(k2, data = df)
#Because the number of clusters (k) must be set before we start the algorithm, 
#it is often advantageous to use several different values of k and examine the 
#differences in the results. We can execute the same process for 3, 4, and 5 clusters, 
#and the results are shown in the figure:

k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

if(!require(gridExtra)){install.packages("gridExtra")}

grid.arrange(p1, p2, p3, p4, nrow = 2)

#Although this visual assessment tells us where true dilineations occur 
#between clusters, it does not tell us what the optimal number of clusters is.
#Determining Optimal Clusters
#As you may recall the analyst specifies the number of clusters to use; preferably
#the analyst would like to use the optimal number of clusters. To aid the analyst,
#the following explains the three most popular methods for determining the optimal 
#clusters, which includes:
#1.Elbow method
#2.Silhouette method
#3.Gap statistic

#In short, the average silhouette approach measures the quality of a clustering.
#That is, it determines how well each object lies within its cluster. 
#A high average silhouette width indicates a good clustering. The average silhouette 
#method computes the average silhouette of observations for different values of k. 
#The optimal number of clusters k is the one that maximizes the average silhouette 
#over a range of possible values for k.

silh=fviz_nbclust(df, kmeans, method = "silhouette")
gap=fviz_nbclust(df, kmeans, method = "gap_stat")
grid.arrange(silh, gap,nrow = 1)

#With most of these approaches suggesting 2 as the number of optimal clusters, 
#we can perform the final analysis and extract the results using 2 clusters.
# Compute k-means clustering with k = 2
final <- kmeans(df, 2, nstart = 25)
print(final)
fviz_cluster(final, data = df)




