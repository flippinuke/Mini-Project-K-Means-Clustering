# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle","NbClust"))
library(cluster)
library(rattle)
library(NbClust)

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)
str(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

#BK: options 1 and 2
library(dplyr)
#BK: these both remove the first column of the data set
try1 <- select(wine, -Type)
try2 <- select(wine, -1)
#BK: then scale it
dfTry1 <- scale(try1)
dfTry2 <- scale(try2)

#BK: option 3: this does it in one step
dfTry3 <- scale(wine[-1])

#BK: check options 1, 2, and 3 against each other
head(dfTry1)
head(dfTry2)
head(dfTry3)

df <- scale(wine[-1])

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

wssplot(df)

# Exercise 2:
#   * How many clusters does this method suggest?
  #BK: if we determine it by bend location - bend seems to be at 3 - it suggests 3 clusters
#   * Why does this method work? What's the intuition behind it?
  #BK: The intuition is that the within groups sum of squares of certain clusters will be close to each
  #BK: other, but other clusters will be farther apart
  #BK: the point at which the bend occurs is likely where before/after there is a more distance in the
  #BK: sum of squares value
#   * Look at the code for wssplot() and figure out how it works
  #BK: it seems as though the code is defining the number of clusters "potentially" desired, from
  #BK: 1 to 15, and then creating a for loop that calculates the wiss for each of those clusters, then
  #BK: plots it out.

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

head(nc)
str(nc)


# Exercise 3: How many clusters does this method suggest?
  #BK: suggests 3 clusters


# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

# fit.km <- kmeans( ... )
fit.km <- kmeans(df, centers = 3)
fit.km <- kmeans(df, 3)
fit.km <- kmeans(df, centers = 3, iter.max = 1000)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
#BK: i think the above has a typo. Should be fit.km$cluster instead of fit.km$clusters
#BK: yes, based on the output, it seems like good clustering
table(fit.km$cluster, wine$Type)

# separate, then together again:
fit.km$cluster
wine$Type
table(fit.km$cluster, wine$Type)



# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

#clusplot( ... )
clusplot(df, fit.km$cluster, color = TRUE, main = "Wine Clusters")

#BK: again, the points seem to fall in pretty identifiable clusters, both by the above methods,
#BK: and by plotting and seeing the visually.

