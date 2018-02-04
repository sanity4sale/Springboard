# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle.data","NbClust"))
library("cluster")
library("rattle.data")
library("NbClust")

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)
str(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

wine.Analysis <- wine[, 2:ncol(wine)]
wine.Analysis <- scale(wine.Analysis)
summary(wine.Analysis)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine.Analysis)

# Exercise 2:
#   * How many clusters does this method suggest?

# The graph bends between 2 to 4 clusters, indicating that 3 clusters could be a suitable choice. 

#   * Why does this method work? What's the intuition behind it?

# The SSE (Sum of Squared Errors of Prediction) is defined as the sum of the squared distance 
# between each member of a cluster and its cluster centroid.
# A higher SSE indicates lower accuracy and lower precision, while a lower SSE indicates higher
# accuracy and higher precision. 
# At the "elbow" of the graphed curve, additional clusters will only give marginal improvement
# in accuracy and precision, while increasing the risk of over-fitting.
# Therefore, choosing the number of clusters at the "elbow" will help the scientist balance the
# competing forces of accuracy, precision, and over-fitting. 

#   * Look at the code for wssplot() and figure out how it works

# wssplot() is a custom-built function that accepts a scaled dataset, variable "data". 
# Two variables are hard-coded: 
# 1) "nc" represents the number of total potential clusters.
# 2) "seed" represents the number at which to set R's random number generator.
# Inside the function itself, the following actions are taken;
# 1) Count the number of rows in dataset "data", and then subtract 1 from that number. 
# 2) Multiply the resulting number by the sum of the variance across columns.
# 3) For each column, 2 through 15, set the seed at 1234, and calculate the within-class sum of squares scatter matrix.
# 4) Plot the resulting data on a line chart, connected by distinct points. 

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

set.seed(1234)
nc <- NbClust(wine.Analysis, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Number of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

# This method also suggests 3 clusters, as shown by the generated bar graph. 

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

k = 3
set.seed(1)
fit.km <- kmeans(wine.Analysis, centers = k, iter.max = 1000)
str(fit.km)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

wine.Results <- table(fit.km$cluster, wine$Type)
View(wine.Results)

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library

clusplot(wine.Results, clus = wine.Results[2, ])

# * Would you consider this a good clustering?

# No. While the model accurately predicted Type 1 with no errors, it did not correctly predict Types 2 or 3.
# Where Type is 2 in the original dataset, the model predicted Type 3 in 91.5% of observations.
# Where Type is 3 in the original dataset, the model predicted Type 2 in 100% of observations.
