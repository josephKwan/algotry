###############################################################################
# Script: iris_cluster.R
# Copyright (c) 2020 by Alan Montgomery. Distributed using license CC BY-NC 4.0
# To view this license see https://creativecommons.org/licenses/by-nc/4.0/
#
# Clustering example with Iris data.  There are four different measurements
# for a number of irises measured by a botanist on a particular day in Canada.
# This is very popular dataset and you will see it frequently.
# Before clustering we do several visualizations.
###############################################################################



##########################################################################################
# setup
##########################################################################################

# prepare data
if (!require(datasets)) {install.packages("datasets"); library(datasets)}
data("iris")    # load the data into memory

# summarize the dataset
summary(iris)



##########################################################################################
# understand the structure of the data [optional]
##########################################################################################

# first look at the data
head(iris)

# R makes extensive use of the select operator
# multiple ways to list the first 6 rows and first 2 columns
head(iris[,1:2])  # select the first two columns and then use head
iris[1:6,1:2]     # here we think of the data frame as a table and get the first 6 rows and 2 columns
iris[1:6,c(1,2)]  # notice that 1:2=seq(1,2)=c(1,2)
iris[1:6,c("Sepal.Length","Sepal.Width")]  # another way to do the same thing using the names of the columns
varnames=c("Sepal.Length","Sepal.Width")   # create a vector of the variable names
iris[1:6,varnames]  # another way using another object that has the variables
# some more variations
iris[c("1","2","3"),varnames]  # sometimes rows have names (see rownames) and we can use these names to select these rows
iris$Sepal.Length[1:6]   # yet another way if we just want to access a column directly
subset(iris,subset=(rownames(iris) %in% c("1","2","3")),select=varnames)  # and yet another, see also dplyr
# notice we use %in% which is another notation for match(rownames(iris),c("1","2","3"),nomatch=0)>0



##########################################################################################
# visualize the data
##########################################################################################

# plot iris data in boxplot
par(mfrow=c(4,1),mar=c(3,4,1,1))  # mfrow=c(4,1) tells R to plot 4 rows and 1 column, and mar is the margin
# boxplot (as well as many other functions in R) use what is known as formula syntax
# the basics of formula is left hand side is the dependent variance and the right side are
# the independent variables. So for instance Petal.Length~Species tells boxplot to 
# have Petal.Length in the y-axis against Species in the x-axis.  One nice thing is that
# when use formula syntax R knows that Species is a factor variable so it knows to 
# automatically create dummy variables for each of its levels "setosa", "versicolor" and
# "virginica".  Notice if you execute the command boxplot(Petal.Length) then we only
# get a single boxplot that has all species.
boxplot(Petal.Length~Species,data=iris,ylab="Petal Length")
boxplot(Petal.Width~Species,data=iris,ylab="Petal Width")
boxplot(Sepal.Length~Species,data=iris,ylab="Sepal Length")
boxplot(Sepal.Width~Species,data=iris,ylab="Sepal Width")
par(mfrow=c(1,1))  # reset to one graph in the panel

# one more visualization is a parallel lines plot
if (!require(lattice)) {install.packages("lattice"); library(lattice)}
# the ~ ... | ... in the following line is a formula in R, and says the input variables
# are the first four columns of iris and are conditional upon the category Species
parallelplot(~iris[1:4]|Species,data=iris)
# scatterplot matrix plots an array of scatterplots
splom(~iris[1:4], groups = Species, data = iris)




##########################################################################################
# cluster the data
##########################################################################################

# before performing k-means clusters with iris data
# remove the Species variable by setting it to NULL
newiris=iris
newiris$Species = NULL

# apply kmeans and save the clustering result to kc.
# the parantheses tell R to print/evaluate the object, alternatively we could enter
# kc=kmeans(newiris,3); print(kc)  but this gives us a simpler way to do both in one line
# there are two inputs to kmeans the dataset of newiris and setting K to the value 3
# to understand the inputs and outputs you can ask for help from R using help(kmeans) or ?kmeans
# however, the help is really meant to be syntax help not help in understanding the algorithm
set.seed(1248765792)   # set the seed so the random initialization of the clusters is the same
( kc = kmeans(newiris, 3) )

# notice that kc is a list.  Quite frequently R returns lists as the result, so that it can
# organize many different variables into a single group.  In this case kmeans returns:
#   kc$cluster:   a vector of integers (from 1:K) indicating the cluster to which each point is allocated
#   kc$centers:   a matrix of cluster centers (the rows correspond to the clusters, columns are variables used for kmeans)
#   kc$totss:     total sum of squares, which says how much variation that originally was in the dataset
#   kc$withinss:  within-cluster sum of squares, vector with K dimensions, each element corresponds to a cluster
#   kc$betweenss: the between-cluster sum of squares which equals is the difference between totss and betweenss
#   kc$size:      the number of pionts in each cluster
#   kc$iter:      the number of iterations
#   kc$ifault:    integer that indicates a possible algorithm problem
str(kc)

# plot the clusters and their centers. Note that there are four dimensions
# in the data and that only the first two dimensions are used to draw the plot
# below.  Some black points close to the green center (asterisk) are actually
# closer to the black center in the four dimensional space

# scatter plot of each cluster with a different color for each cluster
par(mar=c(5,5,5,5))
plot(newiris[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=2) # overlay the cluster centroids as asterisks
legend("topright",legend=as.character(1:3),col=1:3,pch=8,bty='n') # add legend to tell which colors correspond with which cluster

# look at the cluster solution
print(kc$centers)  # the centers or centroids are the averages of observation within a cluster
#mean(newiris$Sepal.Length[kc$cluster==1])   # example: compute average of variable for specific cluster

# we can use parallel plot to see the effects, the auto.key plots a legend above the parallellines
parallelplot(iris[1:4],groups=iris$Species,main="Species",auto.key=list(space="top",columns=3,lines=T))
parallelplot(iris[1:4],groups=kc$cluster,main="Clusters",auto.key=list(space="top",columns=3,lines=T))

# compare the Species label with the cluster result
table(iris$Species, kc$cluster)
#xtabs(~Species+kc$cluster,data=iris)  # another version of same table, xtabs using formula notation

# we can also compare the cluster centroids
parallelplot(kc$centers,main="Clusters",auto.key=list(text=c("1","2","3"),space="top",columns=3,lines=T))
# notice the scales are relative for each variable, try it again it the same scale
parallelplot(kc$centers,main="Clusters",common.scale=TRUE,auto.key=list(text=c("1","2","3"),space="top",columns=3,lines=T))



###############################################################################
### Build a scree plot to determine the number of clusters
###############################################################################

# set the random number seed so the samples will be the same if regenerated
set.seed(34612)

# compute multiple cluster solutions
grpA2=kmeans(newiris,centers=2)
grpA3=kmeans(newiris,centers=3)
grpA4=kmeans(newiris,centers=4)
grpA5=kmeans(newiris,centers=5)
grpA6=kmeans(newiris,centers=6)
grpA7=kmeans(newiris,centers=7)
grpA8=kmeans(newiris,centers=8)
grpA9=kmeans(newiris,centers=9)
grpA10=kmeans(newiris,centers=10)
grpA15=kmeans(newiris,centers=15)
grpA20=kmeans(newiris,centers=20)
grpA30=kmeans(newiris,centers=30)

# compute between and within SS
kclust=c(2:10,15,20,30)
bss=c(grpA2$betweenss,
      grpA3$betweenss,grpA4$betweenss,grpA5$betweenss,grpA6$betweenss,
      grpA7$betweenss,grpA8$betweenss,grpA9$betweenss,grpA10$betweenss,
      grpA15$betweenss,grpA20$betweenss,grpA30$betweenss)
wss=c(grpA2$tot.withinss,
      grpA3$tot.withinss,grpA4$tot.withinss,grpA5$tot.withinss,grpA6$tot.withinss,
      grpA7$tot.withinss,grpA8$tot.withinss,grpA9$tot.withinss,grpA10$tot.withinss,
      grpA15$tot.withinss,grpA20$tot.withinss,grpA30$tot.withinss)

# plot the results and look for the "Hockey-Stick" effect
par(mfrow=c(1,1))
plot(kclust,wss,type="l",main="Within SS for k-means")  # Within SS is variation of errors
points(kclust,wss)
plot(kclust,bss/(wss+bss),type="l",main="R-Squared for k-means")  # R-Squared is ratio of explained variation
points(kclust,bss/(wss+bss))



###############################################################################
### Build a scree plot to determine the number of clusters
### Alternative method using a for loop
###############################################################################

# compute multiple cluster solutions
kclust=2:30                        # create a vector of k values to try
nclust=length(kclust)              # number of kmeans solutions to compute
bss=wss=rep(0,nclust)              # initialize vectors bss and wss to zeroes
set.seed(34612)                    # set the seed so we can repeat the results
grpQ=as.list(rep(NULL,nclust))     # create empty list to save results
# compute SS for each cluster
for (i in 1:nclust) {
   grpQ[[i]]=kmeans(newiris,kclust[i])  # compute kmeans solution, !! try adding nstart=100 to try many random initial values !!
   wss[i]=grpQ[[i]]$tot.withinss        # save the within SS
   bss[i]=grpQ[[i]]$betweenss           # save the between SS
}

# plot the results and look for the "Hockey-Stick" effect
par(mfrow=c(1,1))
plot(kclust,wss,type="l",main="Within SS for k-means")  # Within SS is variation of errors
points(kclust,wss)
plot(kclust,bss/(wss+bss),type="l",main="R-Squared for k-means")  # R-Squared is ratio of explained variation
points(kclust,bss/(wss+bss))




###############################################################################
### Build a scree plot to determine the number of clusters
### Use the for loop, but instead of choosing one initial starting point for
### each value of k, let's try nstart=100.  The nstart option tells kmeans
### to randomly choose many randomly choosen values.  kmeans will only return
### the "best" solution (or the one that minimizes the SSE)
###############################################################################

# compute multiple cluster solutions
kclust=2:30                        # create a vector of k values to try
nclust=length(kclust)              # number of kmeans solutions to compute
bss.nstart=wss.nstart=rep(0,nclust) # initialize vectors bss and wss to zeroes
set.seed(34612)                    # set the seed so we can repeat the results
grpQ=as.list(rep(NULL,nclust))     # create empty list to save results
# compute SS for each cluster
for (i in 1:nclust) {
   grpQ[[i]]=kmeans(newiris,kclust[i],nstart=100)  # compute kmeans solution with nstart value, nstart=100 try many random initial values !!
   wss.nstart[i]=grpQ[[i]]$tot.withinss        # save the within SS
   bss.nstart[i]=grpQ[[i]]$betweenss           # save the between SS
}

# plot the results and look for the "Hockey-Stick" effect
par(mfrow=c(1,1))
# plot the old solution
plot(kclust,wss,type="l",main="Within SS for k-means")  # Within SS is variation of errors
points(kclust,wss)
# overlay our improved solutions
lines(kclust,wss.nstart,col="blue")
points(kclust,wss.nstart,col="blue",pch=16)
legend("topright",c("nstart=1","nstart=100"),lty=1,col=c("black","blue"),bty="n")
# plot the old solution for r-squared
plot(kclust,bss/(wss+bss),type="l",main="R-Squared for k-means")  # R-Squared is ratio of explained variation
points(kclust,bss/(wss+bss))
# overlay our improved solutions
lines(kclust,bss.nstart/(wss.nstart+bss.nstart),col="blue")
points(kclust,bss.nstart/(wss.nstart+bss.nstart),col="blue",pch=16)
legend("bottomright",c("nstart=1","nstart=100"),lty=1,col=c("black","blue"),bty="n")



###############################################################################
### (Optional) Create a hierarchical cluster analysis
### instead of grouping observations into K clusters, we build up the cluster by finding
### individual observations that are most like another observation or previous grouping
### you can turn a hierarchical cluster into K clusters by deciding what is the distance
### at which you want to define your clusters
###############################################################################

# try a hierarchical cluster on the flowers
hc=hclust(dist(newiris),method="complete")
plot(hc,cex=.7)
hc3id = cutree(hc,k=3)  # divide the tree into three clusters
print(hc3id)  # these are the clusters if with divide into three clusters
table(hc3id,kc$cluster)  # notice that the division is similar

# if we want to color the labels according to the species
if (!require(dendextend)) {install.packages("dendextend"); library(dendextend)}  # need new package
dend=as.dendrogram(hc)  # create a new object that stores the dendogram
colorCodes = c(setoas="red",versicolor="green",virginica="yellow")  # the elements of the list correspond to labels
labels_colors(dend) = colorCodes[iris$Species][order.dendrogram(dend)]  # notice we have to reorder the objects according to where they are
plot(dend)  # let's plot the denodogram again
# for an extended analysis see https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html#iris---edgar-andersons-iris-data

