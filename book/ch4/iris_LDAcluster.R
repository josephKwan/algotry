
# TopicModel Clustering example with Iris data.
# There are four different measurements for a number of irises measured
# by a botanistic on a particular day in Canada.
# This is very popular dataset and you will see it frequently.
# In this example we show how a LatentDirichletAllocation or LDA model
# can be applied to give a probabilistic topic solution to the Iris data.


# setup libraries to prepare your library
if (!require(NLP)) {install.packages("NLP"); library(NLP)}
if (!require(topicmodels)) {install.packages("topicmodels"); library(topicmodels)}
if (!require(tm)) {install.packages("NLP"); library(tm)}
if (!require(slam)) {install.packages("NLP"); library(slam)}
if (!require(lattice)) {install.packages("NLP"); library(lattice)}



### organize the data

# prepare data
data(iris)                 # load the data into memory
newiris=iris               # create a copy of the dataset
newiris$Species = NULL     # remove the species variable from newiris dataset
newiris=round(newiris*10)  # round the iris data so we can use it with topicmodels
leniris=apply(newiris,1,sum)   # get the total length the iris



### estimate the topic models

# estimate a series of LDA models (each run can take a few minutes depending upon your processor)
ldac = LDA(newiris,3,method="Gibbs")



### understanding the model

# show the measures and associated topics
ClustTopics = exp(ldac@beta)  # matrix with probabilities of each measure per topic
colnames(ClustTopics)=colnames(newiris)   # use the variable names from the iris dataset
parallelplot(ClustTopics,main="Topic associated with each measure")
head(round(ClustTopics*100))   # print out the % associated with the topics

# probability of topic assignments across flowers
ClustAssign = ldac@gamma   # this is a matrix with the row as the user and column as the topic
ClustBest = apply(ClustAssign,1,which.max)  # determine the best guess of a cluster, a vector with best guess
parallelplot(ClustAssign,groups=ClustBest,ylab="Topic",main="Topic Assignments for each Observation")
boxplot(ClustAssign,xlab="Topic",ylab="Probability of Topic")

# show some specific flowers
head(newiris)
head(ClustAssign)
tail(newiris)
tail(ClustAssign)

# show a flower
barplot(ClustAssign[150,],names.arg=1:ncol(ClustAssign),main="Topics Associated with a selected Flower")

# compare the species label with the best guess from
# the topic model, but remember that kmeans assignments
# are deterministic, while topic models are probabilistic
# so this crosstab only considers most likely topic
parallelplot(~ClustAssign|iris$Species,ylab="Topic",main="Topic Assignments by Species")
xtabs(~iris$Species+ClustBest)

# compare the Species label with the cluster result
( kc = kmeans(newiris, 3) )
xtabs(~kc$cluster+ClustBest)
xtabs(~iris$Species+kc$cluster)

# determine the best guess for each flower by multiplying 
# the cluster assignments by the cluster topics by the total length of the iris (which we assume known)
# for example for the first observation we have
ClustAssign[1,]
ClustTopics
leniris[1]
ClustAssign[1,]%*%ClustTopics
ClustAssign[1,]%*%ClustTopics*leniris[1]
# in matrix form we can compute all of the observations
ClustGuess=(ClustAssign%*%ClustTopics)*leniris
ClustErr=newiris-ClustGuess    # errors associated with best guess
( withinss=sum(ClustErr^2) )   # root of the mean-squared error associated with predictions
1-withinss/kc$totss            # or if we prefer we can compute the R-squared
sum(kc$withinss)               # we can compare this with the within sum-of-squares from the k-means

