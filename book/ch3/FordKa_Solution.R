###############################################################################
# Script: FordKa_Solution.R
#
# R script to create customer segments for ford ka using k-Means
# Requires the excel spreadsheet with the data (FordKaData.xlsx).
# This script creates clusters using both the psycographic and demographic
# datasets using k-means analysis with varying clusters.  You should consider
# different settings of k and may want to consider different sets of
# transformations of the input variables.
###############################################################################



###############################################################################
### setup the environment
###############################################################################

# load in additional packages to extend R functionality
if (!require(lattice)) {install.packages("lattice"); library(lattice)}
if (!require(gplots)) {install.packages("gplots"); library(gplots)}
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
if (!require(reshape2)) {install.packages("reshape2"); library(reshape2)}
if (!require(openxlsx)) {install.packages("openxlsx"); library(openxlsx)}
if (!require(gmodels)) {install.packages("gmodels"); library(gmodels)}
if (!require(likert)) {install.packages("likert"); library(likert)}
if (!require(fields)) {install.packages("fields"); library(fields)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); library(RColorBrewer)}

# set to your correct working directory
setwd("~/Documents/class/data science/hw/ford ka/data")



###############################################################################
### input data
###############################################################################

# read in Ford Ka datasets from the Excel file
forddemo=read.xlsx("FordKaData.xlsx",sheet="Demographic Data",startRow=7,colNames=T,rowNames=F,cols=2:10)  # read the demographic data
fordpsyc=read.xlsx("FordKaData.xlsx",sheet="Psychographic Data",startRow=7,colNames=T,rowNames=F,cols=2:63)  # read the psychographic data
fordquest=read.xlsx("FordKaData.xlsx",sheet="Psychographic questionnaire",startRow=7,colNames=T,rowNames=F,cols=2)  # read the question list
fordseg=read.xlsx("FordKaData.xlsx",sheet="Demographic Data",startRow=7,colNames=T,rowNames=F,cols=11:12)  # read the segments that ford created (do not use in cluster)

# transform the data to make it easier to use
fordquest=paste0(1:62,',',fordquest$Statement)  # convert the question list into a character string to make it easier to work with
afordquest=strtrim(fordquest,30)  # truncate the strings to the first 30 characters since some questions are quite long
fordseg$SegName=as.factor(fordseg$SegmentName)  # convert the segment names into a factor for easier use as a classification variable
fordseg$SegmentName=NULL  # remove this variable
ford=cbind(forddemo,fordpsyc)  # create a new dataframe with both demogrpahic and psychographic data

# create some lists of variables which we will use later in the script
qlist=paste0("Q",1:62)  # sequence of numbers from 1 to 62




###############################################################################
### suggested solution that involves two cluster analyses:
### step #1 cluster questions -- and then save this centers (reduce # of questions)
### step #2 cluster users -- using the centers of the questions (reduce # of users)
### the final result gives a table that summarizes the data
###############################################################################



###############################################################################
### step #1 cluster questions (reduce # of questions)
###############################################################################

# transpose the data (columns become rows and rows become columns)
set.seed(34612)
qford=t(ford[,qlist])

# compute multiple cluster solutions
maxk = 30  # maximum number of clusters
kclust = 2:maxk  # vector of k values to try
nclust = length(kclust)  # number of clusters solutions to try
# initialize storage
grpQv = vector(mode="list",length=nclust)  # create an empty list
bss = rep(0,nclust)
wss = rep(0,nclust)
# compute k-means solutions now
for (i in 1:nclust) {
  # get the k value associated with the ith value
  ki=kclust[i]
  # perform cluster analysis
  grpQv[[i]]=kmeans(qford,centers=ki,nstart=100)
  # save results
  bss[i]=grpQv[[i]]$betweenss
  wss[i]=grpQv[[i]]$tot.withinss
}

# plot the results and look for the "Hockey-Stick" effect
par(mfrow=c(1,1))
plot(kclust,bss/(wss+bss),type="l",main="R-Squared for k-means")
points(kclust,bss/(wss+bss))

# identify "best" solution
kbest = 6  # choose the best cluster solution
iclust = which(kclust==kbest)  # get the index of the desired solution
points(kbest,(bss/(wss+bss))[iclust],col="red")

# extract "best" solution
grpQ=grpQv[[iclust]]
# list the questions in each group (use a for loop to repeat for each cluster)
for (i in 1:kbest) {
  cat("\nCluster # ",i,"\n")
  print(fordquest[grpQ$cluster==i])
}
nshortqlist=c(30,57,53,1,4,12)  # short list of questions
shortqlist=paste0("Q",nshortqlist)
shortqname=strtrim(fordquest[nshortqlist],30)
qcnames=c("Utilitarian","Trendy","SmallCarLover","Masculine","Character","Practical")  # 53,12,1,30,4,12

# using the 6 cluster solution create a dataset with the "scores" of each of the 6 clusters
rford=t(grpQ$centers)
colnames(rford)=qcnames



###############################################################################
### step #2 cluster users -- using the output from part #1
###############################################################################

# initialize storage
grpRv = vector(mode="list",length=nclust)  # create an empty list
bss = rep(0,nclust)
wss = rep(0,nclust)
# compute k-means solutions now
for (i in 1:nclust) {
  # get the k value associated with the ith value
  ki=kclust[i]
  # perform cluster analysis
  grpRv[[i]]=kmeans(rford,centers=ki,nstart=100)
  # save results
  bss[i]=grpRv[[i]]$betweenss
  wss[i]=grpRv[[i]]$tot.withinss
}

# plot the results and look for the "Hockey-Stick" effect
par(mfrow=c(1,1))
plot(kclust,bss/(wss+bss),type="l",main="R-Squared for k-means")
points(kclust,bss/(wss+bss))

# identify "best" solution
kRbest = 3  # choose the best cluster solution
iclust = which(kclust==kRbest)  # get the index of the desired solution
points(kRbest,(bss/(wss+bss))[iclust],col="red")

# extract "best" solution
grpR=grpRv[[iclust]]

# summarize the centroids
grpRcenter=t(grpR$centers)
colnames(grpRcenter)=c("TrendSetters","PracticalOwners","Expressive")
print(round(grpRcenter,digits=1))
parallelplot(t(grpRcenter))

# compare the cluster solutions with the PreferenceGroup
xtabs(~ford$PreferenceGroup+grpR$cluster)
CrossTable(ford$PreferenceGroup,grpR$cluster)   # slightly nicer cross tabulation

# compare the cluster solutions with the Segmentation Scheme developed by Market Researchers
xtabs(~fordseg$SegName+grpR$cluster)



###############################################################################
### generate summaries of the output
###############################################################################

# plot one person
par(mar=c(5,12,4,1))
mdata = unlist(ford[1,qlist])  # extract data for one user 
barplot(mdata,names.arg=fordquest,horiz=TRUE,las=2,cex.names=0.3,col=palette[mdata])
title(main="Psychographic Data for Single User",xlab="Response")

# plot one question
par(mar=c(5,12,4,1))
iquest = 2  # question to use
mdata = unlist(ford[,paste0("Q",iquest)])  # extract data for selected question
userno = 1:nrow(ford)
barplot(mdata,names.arg=as.character(userno),horiz=TRUE,las=2,cex.names=0.3,col=palette[mdata],xlim=c(0,7))
title(main=fordquest[iquest],xlab="Response")

# plot all data (original order)
par(mar=c(5,4,4,1))
mford=t(as.matrix(ford[,qlist]))
palette = brewer.pal(n=7,name="RdBu")
image.plot(mford,xaxt='n',yaxt='n',lab.breaks=NULL,col=palette)
image(mford,axes=FALSE,add=TRUE,col=palette)
axis(3, at=seq(0,1,length=nrow(mford)), labels=rownames(mford), lwd=0, pos=-0.1, las=2, cex.axis=0.5)
axis(2, at=seq(0,1,length=ncol(mford)), labels=colnames(mford), lwd=0, pos=0, las=2, cex.axis=0.5)

# plot all data (group the questions)
par(mar=c(5,4,4,1))
qolist=names(grpQ$cluster[order(grpQ$cluster)])  # create an ordered list of questions by cluster
mford=t(as.matrix(ford[,qolist]))
palette = brewer.pal(n=7,name="RdBu")
image.plot(mford,xaxt='n',yaxt='n',lab.breaks=NULL,col=palette)
image(mford,axes=FALSE,add=TRUE,col=palette)
axis(3, at=seq(0,1,length=nrow(mford)), labels=rownames(mford), lwd=0, pos=-0.1, las=2, cex.axis=0.5)
axis(2, at=seq(0,1,length=ncol(mford)), labels=colnames(mford), lwd=0, pos=0, las=2, cex.axis=0.5)

# plot all data (group the questions and users)
par(mar=c(5,4,4,1))
uolist=names(grpR$cluster[order(grpR$cluster)]) # create an ordered list of users by cluster
mford=t(as.matrix(ford[uolist,qolist]))
palette = brewer.pal(n=7,name="RdBu")
image.plot(mford,xaxt='n',yaxt='n',lab.breaks=NULL,col=palette)
image(mford,axes=FALSE,add=TRUE,col=palette)
axis(3, at=seq(0,1,length=nrow(mford)), labels=rownames(mford), lwd=0, pos=-0.1, las=2, cex.axis=0.5)
axis(2, at=seq(0,1,length=ncol(mford)), labels=colnames(mford), lwd=0, pos=0, las=2, cex.axis=0.5)

# draw with hierarchical clustering
heatmap(mford,Rowv=NA,Colv=NA)
heatmap(mford)
heatmap.2(mford)

# print table of values
t(grpRcenter)

