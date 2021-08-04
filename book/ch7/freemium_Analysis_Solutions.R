###############################################################################
###
### This is suggested solution for the highnote freemium exercise which asks you
### to create a predictive model of adoption for the highnote freemium dataset.
### This script does the following:
###  0) sets up the environment
###  1) imports the freemium dataset from a text file
###  2) creates another version of the data with all missing values recoded to their mean
###  3) computes descriptive statistics and plots
###  4) estimates a tree model and logistic regression
###     for each of these models it computes predictions, a confusion matrix, and lift
###     of those observations in the top decline.
###  5) compare the models
###
###  notice the !! denote areas to change the code !!
###############################################################################


###############################################################################
### setup the environment
###############################################################################

# setup environment, for plots
if (!require(reshape2)) {install.packages("reshape2"); library(reshape2)}
if (!require(gplots)) {install.packages("gplots"); library(gplots)}
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
# setup environment, make sure this library has been installed
if (!require(rstudioapi)) {install.packages("rstudioapi"); library(rstudioapi)}
# setup environment, make sure this library has been installed
if (!require(tree)) {install.packages("tree"); library(tree)}
# setup environment (if you want to use fancy tree plots)
if (!require(rpart)) {install.packages("rpart"); library(rpart)}
if (!require(rattle)) {install.packages("rattle"); library(rattle)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); library(rpart.plot)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); library(RColorBrewer)}
if (!require(party)) {install.packages("party"); library(party)}
if (!require(partykit)) {install.packages("partykit"); library(partykit)}
# a better scatterplot matrix routine
if (!require(car)) {install.packages("car"); library(car)}
# better summary tables
if (!require(psych)) {install.packages("psych"); library(psych)}
# for visualizing regressions
if (!require(visreg)) {install.packages("visreg"); library(visreg)}
# tools for logistic regression
if (!require(mefa)) {install.packages("mefa"); library(mefa)}  # modified rep function
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}  # ROC curve for tree or logistic
if (!require(plotmo)) {install.packages("plotmo"); library(plotmo)}  # show model response
# Install a package for plotting correlations and include it
if (!require(corrplot)) {install.packages("corrplot"); library(corrplot)}
# data manipulation
if (!require(plyr)) {install.packages("plyr"); library(plyr)}
# for parallelplot
if (!require(lattice)) {install.packages("lattice"); library(lattice)}

# define a function to summary a classification matrix we will use later
confmatrix.summary <- function(predprob,predclass,trueclass) {
  # compute confusion matrix (columns have truth, rows have predictions)
  results = xtabs(~predclass+trueclass)
  if (nrow(results)!=2 & ncol(results)!=2) {
    stop("Error: results is not a 2x2 matrix, cannot compute confusion matrix")  }
  # compute usual metrics from the confusion matrix
  accuracy = (results[1,1]+results[2,2])/sum(results)   # how many correct guesses along the diagonal
  truepos = results[2,2]/(results[1,2]+results[2,2])  # how many correct "default" guesses
  precision = results[2,2]/(results[2,1]+results[2,2]) # proportion of correct positive guesses 
  trueneg = results[1,1]/(results[2,1]+results[1,1])  # how many correct "non-default" guesses 
  # compute the lift using the predictions for the 10% of most likely
  topdefault = as.vector( predprob >= as.numeric(quantile(predprob,probs=.9)))  # which customers are most likely to default
  ( baseconv=sum(trueclass==1)/length(trueclass) )  # what proportion would we have expected purely due to chance
  ( actconv=sum(trueclass[topdefault])/sum(topdefault))  # what proportion did we actually predict
  ( lift=actconv/baseconv )  # what is the ratio of how many we got to what we expected
  return(list(confmatrix=results,accuracy=accuracy,truepos=truepos,precision=precision,trueneg=trueneg,lift=lift))
}



###############################################################################
### input the data and prepare the dataset for analysis
###############################################################################

# set to working directory of script (assumes data in same directory as script)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # only works in Rstudio scripts
# alternatively set the working directory manually
setwd("~/Documents/class/data science/hw/freemium/data")

# Read data from the CSV file
freemium=read.csv("High Note data.csv")

# compute the number of observations in the freemium dataset
nobs=nrow(freemium)

# set the random number seed so the samples will be the same if regenerated
set.seed(1248765792)

# prepare new values using a uniform random number, each record in freemium has 
# a corresponding uniform random value which will be used to decide if the observation
# is assigned to the training, validation or prediction sample
randvalue=runif(nobs)
trainsample=randvalue<.6
validsample=(randvalue>=.6 & randvalue<.9)
predsample=(randvalue>=.9)
plotsample=sample(1:nrow(freemium),300)

# copy the dataset to one that has recoded values for all missing values
# this adds the columns age_Missing, male_Missing, good_country_Missing, shouts_Missing
# if the corresponding column of age, male, good_country, or shouts is NA
# if the values is missing then the Missing variable is set to 1 and 0 otherwise
# and the value in the original column is replaced with the average value
rfreemium=freemium

# create columns to code if the variable is missing
rfreemium$age_Missing=as.numeric(is.na(freemium$age))
rfreemium$age[rfreemium$age_Missing==1]=mean(freemium$age,na.rm=T)
rfreemium$male_Missing=as.numeric(is.na(freemium$male))
rfreemium$male[rfreemium$male_Missing==1]=mean(freemium$male,na.rm=T)
rfreemium$good_country_Missing=as.numeric(is.na(freemium$good_country))
rfreemium$good_country[rfreemium$good_country_Missing==1]=mean(freemium$good_country,na.rm=T)
rfreemium$shouts_Missing=as.numeric(is.na(freemium$shouts))
rfreemium$shouts[rfreemium$shouts_Missing==1]=mean(freemium$shouts,na.rm=T)
rfreemium$avg_friend_age_Missing=as.numeric(is.na(freemium$avg_friend_age))
rfreemium$avg_friend_age[rfreemium$avg_friend_age_Missing==1]=mean(freemium$avg_friend_age,na.rm=T)
rfreemium$avg_friend_male_Missing=as.numeric(is.na(freemium$avg_friend_male))
rfreemium$avg_friend_male[rfreemium$avg_friend_male_Missing==1]=mean(freemium$avg_friend_male,na.rm=T)
# since there are not too many missing observations for friend_cnt, subscriber_friend_cnt,
# friend_country_cnt, and tenure these then the missing values for these are simply set to the mean
rfreemium$friend_cnt[is.na(rfreemium$friend_cnt)]=mean(freemium$friend_cnt,na.rm=T)
rfreemium$subscriber_friend_cnt[is.na(rfreemium$subscriber_friend_cnt)]=mean(freemium$subscriber_friend_cnt,na.rm=T)
rfreemium$friend_country_cnt[is.na(rfreemium$friend_country_cnt)]=mean(freemium$friend_country_cnt,na.rm=T)
rfreemium$tenure[is.na(rfreemium$tenure)]=mean(freemium$tenure,na.rm=T)

# create a list with the variables that will be used in the analysis
varlist=c("age","male","friend_cnt","subscriber_friend_cnt","avg_friend_age","avg_friend_male","friend_country_cnt",
          "songsListened","playlists","posts","shouts","lovedTracks","tenure","good_country")
# also create a list for the recoded values
rvarlist=c("age","age_Missing","male","male_Missing","friend_cnt","subscriber_friend_cnt","avg_friend_age","avg_friend_age_Missing",
           "avg_friend_male","avg_friend_male_Missing","friend_country_cnt","songsListened","playlists","posts",
           "shouts","shouts_Missing","lovedTracks","tenure","good_country","good_country_Missing")
crvarlist=c("adopter",rvarlist)

##
# create a dummy variable for subscriber friend count change
##
# Note that the delta1_subscriber_friend_cnt variable represents changes in the number of
# subscriber friends in both directions.  If a user's subscribe friend count increases, the
# delta1_subscriber_friend_cnt is positive.  If a user's subscriber friend count decreases, the
# delta1_subscriber_friend_cnt is negative.  E.g., delta1_subscriber_friend_cnt=1 implies that the 
# user obtained one more friend who is a subscriber.  It could be that this user made a new friend
# who is a subscriber, or that an existing friend switched from being a non-subscriber to a
# subscriber.
#
# Similarly, delta1_subscriber_friend_cnt=-1 implies that the user lost a friend who is a subscriber,
# or that one of the user's friend swtiched from being a subscriber to a non-subscriber. For
# delta1_subscriber_friend_cnt=1, we generate dum_delta1_subsfrcnt=1; for
# delta1_subscriber_friend_cnt=-1, we generate dum_delta1_subscfrcnt=0.
#
# The impact of increasing subscriber friend and decreasing subscriber friend is asymmmetric. An
# additional subscriber friend might have a positive influence on the user.  However, the attribution of 
# a subscriber firned usually does not have a negative impact on the user's adoption decision. We
# hence transform the delta1_subscriber_friend_cnt variable to be a dummary variable,
# dum_delta1_subscfrnt.  If the delta1_subscriber_friend_cnt is positive, dum_delta1_usbsfrcnt
# equals to 1. Ohterwise, dum_delta1_subsfrcnt equals to 0. And we use the dum_delta1_usbsfrcnt
# as our explanatory variable instead of delta1_subscriber_friend_cnt, because
# dum_delta1_subsfrcnt is a more accurate variable that captures the actual influence.
rfreemium$dum_delta1_subsfrcnt=(rfreemium$delta1_subscriber_friend_cnt>0)+1



###############################################################################
### understanding the data with descriptive statistics and graphics
###############################################################################

# number of observations
sum(trainsample)
sum(validsample)
sum(predsample)

# let's take a look at just one observation
print(freemium[1,])
# same observation but just a few values
print(freemium[1,varlist])  

# use the describe function in the psych package to generate nicer tables
describe(freemium[trainsample,varlist],fast=TRUE)
# describe the freemium data for adopters and non-adopters, ?? do you see differences between groups ??
describeBy(freemium[trainsample,varlist],group=freemium$adopter[trainsample],fast=TRUE)

# do the same thing with the recoded data (but just for the training data)
describe(rfreemium[trainsample,rvarlist],fast=TRUE)
describeBy(rfreemium[trainsample,rvarlist],group=rfreemium$adopter[trainsample],fast=TRUE)

# boxplots  ?? can you see differences ??
par(mfrow=c(3,4),mar=c(5,5,1,1))
boxplot(age~adopter,data=freemium[plotsample,],xlab="adopter",ylab="age")
boxplot(friend_cnt~adopter,data=freemium[plotsample,],xlab="adopter",ylab="friend_cnt")
boxplot(subscriber_friend_cnt~adopter,data=freemium[plotsample,],xlab="adopter",ylab="subscriber_friend_cnt")
boxplot(avg_friend_age~adopter,data=freemium[plotsample,],xlab="adopter",ylab="avg_friend_age")
boxplot(avg_friend_male~adopter,data=freemium[plotsample,],xlab="adopter",ylab="avg_friend_male")
boxplot(friend_country_cnt~adopter,data=freemium[plotsample,],xlab="adopter",ylab="friend_country_cnt")
boxplot(songsListened~adopter,data=freemium[plotsample,],xlab="adopter",ylab="songsListened")
boxplot(playlists~adopter,data=freemium[plotsample,],xlab="adopter",ylab="playlists")
boxplot(posts~adopter,data=freemium[plotsample,],xlab="adopter",ylab="posts")
boxplot(shouts~adopter,data=freemium[plotsample,],xlab="adopter",ylab="shouts")
boxplot(lovedTracks~adopter,data=freemium[plotsample,],xlab="adopter",ylab="lovedTracks")
boxplot(tenure~adopter,data=freemium[plotsample,],xlab="adopter",ylab="tenure")

# cross tabs to understand relationships across discrete values
xtabs(~male+adopter,data=freemium)
xtabs(~good_country+adopter,data=freemium)

# compute correlation matrix (using only complete sets of observations)
round(cor(freemium[,varlist],use="pairwise.complete.obs"),digits=2)

# pairs
par(mfrow=c(1,1),mar=c(5,4,4,1))
pairs(freemium[plotsample,varlist])

# nicer scatterplot matrix (the diagonals give the histogram, the colors plot those that convert and those that do not)
par(mfrow=c(1,1),mar=c(5,4,4,1))
scatterplotMatrix(~age+tenure+friend_cnt|adopter,data=freemium[plotsample,])  # small plot to see easier, the "|" is used to print separate plots by adopter
scatterplotMatrix(~age+friend_cnt+subscriber_friend_cnt+avg_friend_age+avg_friend_male+friend_country_cnt|adopter,data=freemium[plotsample,])
scatterplotMatrix(~songsListened+playlists+posts+shouts+lovedTracks+tenure|adopter,data=freemium[plotsample,])



###############################################################################
### estimate a tree model with all variables
###############################################################################

# estimate a decision tree model using rpart
ctree.full = rpart(adopter~., data=rfreemium[trainsample,crvarlist], control=rpart.control(cp=0.0005), model=TRUE)
summary(ctree.full)
# uncomment the line below to view the full tree -- clearly needs pruning -- which is what the commands below do
#prp(ctree.full)  # make sure your plot window is large or this command can cause problems

# these lines are helpful to find the "best" value of cp
# A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line.
printcp(ctree.full)               # display table of optimal prunings based on complexity parameter
plotcp(ctree.full)                # visualize cross-validation results

# prune the tree back !! choose on of the lines below for treeA or treeB, and leave the other commented out !!
ctree=prune(ctree.full,cp=0.001)  # prune tree using chosen complexity parameter !! try choosing other values of cp like .001, .005, ... !!
ctree1=prune(ctree.full,cp=0.005)

# visualize the trees 
par(mfrow=c(1,1))         # reset one graphic per panel
plot(ctree); text(ctree)  # simple graph
prp(ctree,extra=101,nn=TRUE)  # add the size and proportion of data in the node
#fancyRpartPlot(ctree)     # fancy graphic  !! uncomment if you load library(rattle) !!

# give a summary of the model's trained parameters (+++ see #@plotlr for more plots +++)
summary(ctree)
plotmo(ctree)             # evaluates selected input but holds other values at median
#plotmo(ctree,pmethod="partdep")   # evaluates selected input and averages other values  !! pmethod="apartdep" is faster but approximate !!

# compute predictions for the entire sample -- but model was only trained on trainsample
padopter.tree = predict(ctree,newdata=rfreemium,type='vector')
cadopter.tree = (padopter.tree>0.16)+0  # !! change 0.16 cutoff as appropriate !!
trueadopter = rfreemium$adopter

# compute confusion matrix and some usual statistics (uncomment train and prediction samples if you want to see these statistics)
#confmatrix.summary(padopter.tree[trainsample],cadopter.tree[trainsample],trueadopter[trainsample])  # summary for training sample (look to see if train is substantially better than valid)
confmatrix.summary(padopter.tree[validsample],cadopter.tree[validsample],trueadopter[validsample])  # summary for validation sample
#confmatrix.summary(padopter.tree[predsample],cadopter.tree[predsample],trueadopter[predsample]) # summary for prediction sample (use this when you want to know how good your "final" model is)

# compute ROC and AUC
rocpred.tree = prediction(padopter.tree[validsample],trueadopter[validsample])  # compute predictions using "prediction"
rocperf.tree = performance(rocpred.tree, measure = "tpr", x.measure = "fpr")
plot(rocperf.tree, col=rainbow(10)); abline(a=0, b= 1)
auc.tmp = performance(rocpred.tree,"auc")  # compute area under curve
(auc.tree = as.numeric(auc.tmp@y.values))

# compute ROC and AUC (do for ctree1 as well)
padopter.tree1 = predict(ctree1,newdata=rfreemium,type='vector')
rocpred.tree1 = prediction(padopter.tree[validsample],trueadopter[validsample])  # compute predictions using "prediction"
rocperf.tree1 = performance(rocpred.tree1, measure = "tpr", x.measure = "fpr")
plot(rocperf.tree1, col=rainbow(10)); abline(a=0, b= 1)
auc.tmp = performance(rocpred.tree1,"auc")  # compute area under curve
(auc.tree1 = as.numeric(auc.tmp@y.values))

# compute the predictions for each decline of most likely adopterers (for validation sample)
vprob=seq(.9,.1,-.1)  # define 90th to 10th percentiles
vlift1=rep(0,length(vprob))  # save results to vector
for (i in 1:length(vprob)) {
  topadopter = as.vector(padopter.tree>=as.numeric(quantile(padopter.tree,probs=vprob[i])))  # compute indices of topadopters
  ( baseconv=sum(trueadopter==1)/length(trueadopter) )  # what proportion would we have expected purely due to chance
  ( actconv=sum(trueadopter[topadopter])/sum(topadopter))  # what proportion did we actually predict
  ( vlift1[i]=actconv/baseconv )  # what is the ratio of how many we got to what we expected
}
plot(vlift1,axes=F,xlab="Percentile",ylab="Lift")   # plot the lift
lines(vlift1)
axis(2)  # overlay y axis
axis(1,at=1:length(vprob),labels=vprob)  # overlay x axis, but use vprob as labels




###############################################################################
### additional exploratory analysis
###############################################################################

# determine fraction of users who shout, lovetracks, playlist, post
perc.users=colSums(rfreemium[trainsample,c("shouts","lovedTracks","playlists","posts")]!=0)/sum(trainsample)
barplot(perc.users,xlab="Activity",ylab="Percent Engaged",main="User initiated actions",col="lightblue")

# check by adopters and non-adopters
perc.adopt=colSums(rfreemium[trainsample & rfreemium$adopter==1,c("shouts","lovedTracks","playlists","posts")]!=0)/sum(rfreemium$adopter[trainsample]==1)
perc.free=colSums(rfreemium[trainsample & rfreemium$adopter==0,c("shouts","lovedTracks","playlists","posts")]!=0)/sum(rfreemium$adopter[trainsample]==0)
barplot(rbind(perc.free,perc.adopt),beside=TRUE,col=c("blue","red"),xlab="Activity",ylab="Percent Engaged",main="User initiated actions")
legend("topright",c("Free users","Adopters"),pch=15,col=c("blue","red"),bty="n")

# count number that shout, love and create playlist (overall and for adopters)
perc.heavyuser=sum(
  rfreemium$shouts[trainsample]>0 &
    rfreemium$lovedTracks[trainsample]>0 &
    rfreemium$playlists[trainsample]>0)/sum(trainsample)
perc.adoptuser=sum(
  rfreemium$shouts[trainsample & rfreemium$adopter==1]>0 &
    rfreemium$lovedTracks[trainsample & rfreemium$adopter==1]>0 &
    rfreemium$playlists[trainsample & rfreemium$adopter==1]>0)/sum(rfreemium$adopter[trainsample]==1)
perc.heavyfree=sum(
  rfreemium$shouts[trainsample & rfreemium$adopter==0]>0 &
    rfreemium$lovedTracks[trainsample & rfreemium$adopter==0]>0 &
    rfreemium$playlists[trainsample & rfreemium$adopter==0]>0)/sum(rfreemium$adopter[trainsample]==0)
cbind(perc.heavyuser,perc.heavyfree,perc.adoptuser)  # print totals



###############################################################################
### @logistic regression model
### using stepwise regression model with all the variables and their interactions
###############################################################################

## !! if you want to build your own model run this portion of the code, otherwise skip to the next block of code !!
# uncomment following lines with ## in front of them)
# first estimate the null model (this just has an intercept)
null=glm(adopter~1,data=rfreemium[trainsample,crvarlist],family='binomial')
# second estimate a complete model (with all variables that you are interested in)
#full=glm(adopter~.,data=rfreemium[trainsample,crvarlist],family='binomial')  # can be slow to estimate
# if you have time uncomment the following line and include all squared terms (e.g., nonlinear effects)
full=glm(adopter~.^2,data=rfreemium[trainsample,crvarlist],family='binomial')  # takes a very long time -- but since we just want the formula for stepwise can just use plotsample instead of trainsample
# finally estimate the step wise regression starting with the null model
lrstep=step(null, scope=formula(full),steps=15,dir="forward")  # !! can increase beyond 10 steps, just takes more time
lrmdl=lrstep  # overwrite lrmdl with the new stepwise regression

# give a summary of the model's trained parameters 
summary(lrmdl)
plotmo(lrmdl)             # evaluates selected input but holds other values at median
#plotmo(lrmdl,pmethod="partdep")   # evaluates selected input and averages other values  !! pmethod="apartdep" is faster but approximate !!

# visualize the effects of the model
# plot the log of the odds ratio as function of playlists
par(mfrow=c(2,1))
visreg(lrmdl,"playlists",ylab="Log(OddsRatio of Adopt)",xlim=c(0,100))
visreg(lrmdl,"playlists",scale="response",ylab="Pr(Adopt)",xlim=c(0,100))
# plot the log of the odds ratio as function of lovedtracks
par(mfrow=c(2,1))
visreg(lrmdl,"lovedTracks",ylab="Log(OddsRatio of Adopt)",xlim=c(0,100))
visreg(lrmdl,"lovedTracks",scale="response",ylab="Pr(Adopt)",xlim=c(0,100))
# plot the log of the odds ratio as function of subscriber_friend_cnt
par(mfrow=c(2,1))
visreg(lrmdl,"subscriber_friend_cnt",ylab="Log(OddsRatio of Adopt)",xlim=c(0,100))
visreg(lrmdl,"subscriber_friend_cnt",scale="response",ylab="Pr(Adopt)",xlim=c(0,100))
# create a contour plot to visualize two effects at the same time
par(mfrow=c(1,1))
visreg2d(lrmdl,"playlists","subscriber_friend_cnt",plot.type="image",main="Log(OddsRatio of Adopt)",xlim=c(0,100),ylim=c(0,100))
visreg2d(lrmdl,"playlists","subscriber_friend_cnt",scale="response",plot.type="image",main="Pr(Adopt)",xlim=c(0,100),ylim=c(0,100))

# compute predictions for the entire sample -- but model was only trained on trainsample
padopter.lr = predict(lrmdl,newdata=rfreemium,type='response')
cadopter.lr = (padopter.lr>0.16)+0  # !! change 0.16 cutoff as appropriate !!
trueadopter = rfreemium$adopter

# compute confusion matrix and some usual statistics (uncomment train and prediction samples if you want to see these statistics)
#confmatrix.summary(padopter.lr[trainsample],cadopter.lr[trainsample],trueadopter[trainsample])  # summary for training sample (look to see if train is substantially better than valid)
confmatrix.summary(padopter.lr[validsample],cadopter.lr[validsample],trueadopter[validsample])  # summary for validation sample
#confmatrix.summary(padopter.lr[predsample],cadopter.lr[predsample],trueadopter[predsample]) # summary for prediction sample (use this when you want to know how good your "final" model is)

# compute ROC and AUC
rocpred.lr = prediction(padopter.lr[validsample],trueadopter[validsample])  # compute predictions using "prediction"
rocperf.lr = performance(rocpred.lr, measure = "tpr", x.measure = "fpr")
plot(rocperf.lr, col=rainbow(10)); abline(a=0, b= 1)
auc.tmp = performance(rocpred.lr,"auc")  # compute area under curve
(auc.lr = as.numeric(auc.tmp@y.values))

# compute ROC and AUC (do for full model as well)
padopter.lr1 = predict(full,newdata=rfreemium,type='response')
rocpred.lr1 = prediction(padopter.lr1[validsample],trueadopter[validsample])  # compute predictions using "prediction"
rocperf.lr1 = performance(rocpred.lr1, measure = "tpr", x.measure = "fpr")
plot(rocperf.lr1, col=rainbow(10)); abline(a=0, b= 1)
auc.tmp = performance(rocpred.lr1,"auc")  # compute area under curve
(auc.lr = as.numeric(auc.tmp@y.values))

# compute the predictions for each decline of most likely adopterers (for validation sample)
vprob=seq(.9,.1,-.1)  # define 90th to 10th percentiles
vlift=rep(0,length(vprob))  # save results to vector
for (i in 1:length(vprob)) {
  topadopter = as.vector(padopter.lr>=as.numeric(quantile(padopter.lr,probs=vprob[i])))  # compute indices of topadopters
  ( baseconv=sum(trueadopter==1)/length(trueadopter) )  # what proportion would we have expected purely due to chance
  ( actconv=sum(trueadopter[topadopter])/sum(topadopter))  # what proportion did we actually predict
  ( vlift[i]=actconv/baseconv )  # what is the ratio of how many we got to what we expected
}
plot(vlift,axes=F,xlab="Percentile",ylab="Lift")   # plot the lift
lines(vlift)
axis(2)  # overlay y axis
axis(1,at=1:length(vprob),labels=vprob)  # overlay x axis, but use vprob as labels



###############################################################################
### @compare models using ROC plot
###############################################################################

# plot all ROC curves together
plot(rocperf.lr,col="red"); abline(a=0,b=1)
plot(rocperf.lr1,add=TRUE,col="blue")
plot(rocperf.tree,add=TRUE,col="green")
plot(rocperf.tree1,add=TRUE,col="darkgreen")
legend("bottomright",c("LogRegr","FullRegr","Simple Tree","Complex Tree"),pch=15,col=c("red","blue","green","darkgreen"),bty="n")

# plot lift
plot(vlift1,axes=F,xlab="Percentile",ylab="Lift",col="blue")   # plot the lift
lines(vlift1,col="blue")
axis(2)  # overlay y axis
axis(1,at=1:length(vprob),labels=vprob)  # overlay x axis, but use vprob as labels
points(vlift,col="red"); lines(vlift,col="red")
legend("topright",c("LogRegr","Tree"),pch=15,col=c("red","blue"),bty="n")



###############################################################################
### (OPTIONAL) summarize the logistic regression by saving the coefficients to CSV
###############################################################################

# create list of customer indices to extract for our analysis
userlist=c(1:4)  # just select the first 4 users

# create vector of variables used in model called mvarlist, and add other variables that we want to write out
# these lines require the lrmdl and ctree to be created appropriately above
mvarlist=names(coefficients(lrmdl))[-1]   # get the variables used in your logistic regression moodel, except the intercept which is in first position
# check if there are interactions
isinteractions=grepl(":",paste(mvarlist,collapse=" "))
if (isinteractions) {mvarlist=unique(unlist(strsplit(mvarlist,":"))) }
evarlist=c()    # vector of extra variables to save -- regardless of whether they are in the model
varlist=c(mvarlist,evarlist)         # vector of variable names that we will use (all model variables plus ID and revenue)
print(varlist)  # vector of variables to save

# retrieve coefficients from your model
coeflist=summary(lrmdl)$coefficients  # extract coefficients estimates and std errors and z values
coefdata=data.frame(rn=rownames(coeflist),coeflist,row.names=NULL)  # change to dataframe
colnames(coefdata)=c("rn",colnames(coeflist))
print(coefdata)   # print out the coefficients

# retrieve data about the users (assumes that pchurn.lr and pchurn.tree have been computed in earlier part of script)
userpred=cbind(padopter.lr[userlist])  # create matrix of predictions from our model for selected users
colnames(userpred)=c("padopter.lr")  # label our columns appropriately
modeldata=model.matrix(lrmdl,data=rfreemium[userlist,])  # construct the data used in the model
userdata=rfreemium[userlist,evarlist]  # get additional variables
userdata=t(cbind(modeldata,userdata,userpred))  # get relevant data for a set of customers
userdata=data.frame(rn=rownames(userdata),userdata,row.names=NULL)  # change to dataframe
print(userdata)   # print out user data

# retrieve averages and std dev across all users
modelall=model.matrix(lrmdl,data=rfreemium[trainsample,])  # get a matrix of all data used in the model (just training sample)
meandata=apply(modelall,2,mean) # compute the average for the selected variables (the "2" means compute by column)
sddata=apply(modelall,2,sd)  # compute the standard deviation for selected variables
descdata=data.frame(rn=names(meandata),meandata,sddata,row.names=NULL)  # merge the vectors with the mean and stddev into a single dataframe
print(descdata)   # print out the descriptive values

# combine the data together to make it easier to dump out to a single spreadsheet
mdata=join(coefdata,descdata,type='full',by='rn')  # merge the coefficients and descriptive stats
mdata=join(mdata,userdata,type='full',by='rn')  # create a final single dataframe
print(mdata)    # print out the combined data

# write the data to a spreadsheet
write.csv(mdata,file="freemium_lrmodeldata.csv")   # if you want you can import this file into excel for easier processing



###############################################################################
### cluster consumers based upon the data weighted by the logistic regression coefficients
### the purpose of this analysis is to identify a smaller set of 'prototypical' customers
###############################################################################

# create matrix of data used in the model
modeldata=model.matrix(lrmdl,data=rfreemium)  # construct the data used in the model
xmodeldata=modeldata  # copy (modeldata has ": and xmodeldata has "X")
mvarcoef=colnames(modeldata)  # list of variables (with potential interactions as ":")
if (isinteractions) {
  mvarlisti.idx=grepl(":",mvarcoef)    # get indices of interaction variables in modeldata
  colnames(xmodeldata)=gsub(":","X",colnames(xmodeldata))  # replace : with X for any interactions
}
xmvarcoef=colnames(xmodeldata)
head(xmodeldata)

# create predictions from the logistic regression model
userpred=predict(lrmdl,newdata=rfreemium[,crvarlist],type='response')  # predict prob for all users
head(userpred)

# take a look at first users
head(round(cbind(userpred,xmodeldata),2))

# "weight" the data using the logistic regression coefficients.  this allows the cluster to look at
# the variables based upon their contribution to their log-odds ratio
parm=coefdata[,2]  # just extract the parameter estimates in the 1st column
names(parm)=coefdata$rn
wuserdata=sweep(modeldata,MARGIN=2,parm,"*")  # multiply each row in userdata by parm vector

# compare the data (show for just first 4 columns since there are many)
xmodeldata[1,1:4]
parm[1:4]
wuserdata[1,1:4]

# compare original data
head(round(cbind(userpred[1:4],xmodeldata[,1:4]),2))
head(round(cbind(userpred[1:4],wuserdata[,1:4]),2))

# compute multiple cluster solutions
kclust=2:15                        # create a vector of k values to try
nclust=length(kclust)              # number of kmeans solutions to compute
bss=wss=rep(0,nclust)              # initialize vectors bss and wss to zeroes
set.seed(34612)                    # set the seed so we can repeat the results
grpQ=as.list(rep(NULL,nclust))     # create empty list to save results
# compute SS for each cluster
for (i in 1:nclust) {
  grpQ[[i]]=kmeans(wuserdata,kclust[i],nstart=10)  # compute kmeans solution, !! try adding nstart=30 to try many random initial values !!
  wss[i]=grpQ[[i]]$tot.withinss        # save the within SS
  bss[i]=grpQ[[i]]$betweenss           # save the between SS
}

# plot the results and look for the "Hockey-Stick" effect
par(mfrow=c(1,1))
plot(kclust,wss,type="l",main="Within SS for k-means")  # Within SS is variation of errors
points(kclust,wss)
plot(kclust,bss/(wss+bss),type="l",main="R-Squared for k-means")  # R-Squared is ratio of explained variation
points(kclust,bss/(wss+bss))

# cluster the users into groups
set.seed(612490)   # make sure we get the same solution
ncluster=8    # number of clusters
grpA=kmeans(wuserdata,ncluster,nstart=20)  # add nstart=50 to choose 50 different random seeds

# look at the centers
round(grpA$centers[,1:6],2)

# look at the size of the clusters
table(grpA$cluster)

# create boxplot of userpred using the clusters
boxplot(userpred~grpA$cluster,xlab="Cluster",ylab="Pr(Churn)")
boxplot(rfreemium$lovedTracks~grpA$cluster,xlab="Cluster",ylab="LovedTracks")  # to limit yscale use ylim=c(0,500)

# create a parallel plot to visualize the centroid values
parallelplot(grpA$centers,auto.key=list(text=as.character(1:nrow(grpA$centers)),space="top",columns=5,lines=T))
parallelplot(grpA$centers,auto.key=list(text=as.character(1:nrow(grpA$centers)),space="top",columns=5,lines=T),common.scale=TRUE)  # choose min and max across variables to give an absolute comparison





###############################################################################
### compare logistic regression models with the change instead of level
###############################################################################

### estimate logistic regression with behaviors (usual way)
lrmdl=glm(adopter~good_country
          +friend_cnt
          +subscriber_friend_cnt
          +songsListened
          +lovedTracks
          +posts
          +playlists
          +shouts,
          data=rfreemium,family='binomial')
summary(lrmdl)

### estimate logistic regression with change in behaviors (using historical period as controls)
lrmdl=glm(adopter~delta1_good_country
          +delta1_friend_cnt
          +dum_delta1_subsfrcnt
          +delta1_songsListened
          +delta1_lovedTracks
          +delta1_posts
          +delta1_playlists
          +delta1_shouts,
          data=rfreemium,family='binomial')
summary(lrmdl)

# check the standard deviation (use sapply to call the standard deviation function with the argument na.rm=T)
# alternatively we could just enter sd(rfreemium$delta1_good_country,na.rm=T) for each variable
results=sapply(rfreemium[,c("delta1_good_country","delta1_friend_cnt","delta1_songsListened","delta1_lovedTracks","delta1_posts",
                            "delta1_playlists","delta1_shouts")],sd,na.rm=T)
as.matrix(results)  # print out the results
results=sapply(rfreemium[,c("good_country","friend_cnt","songsListened","lovedTracks","posts",
                            "playlists","shouts")],sd,na.rm=T)
format(as.matrix(results),digits=3,scientific=F)  # print the table in a nicer format





###############################################################################
### estimate a tree model for sample of variables (use as a benchmark to compare with next model)
###############################################################################

# estimate a baseline model with a sample of variables
svarlist=c("adopter","good_country","friend_cnt","subscriber_friend_cnt","songsListened","lovedTracks","posts","playlists","shouts")
ctree3 = rpart(adopter~., data=rfreemium[trainsample,svarlist], control=rpart.control(cp=0.001))
fancyRpartPlot(ctree3)   # prettier plot

# predict probability (for validation sample)
padopter = predict(ctree3,newdata=rfreemium[validsample,svarlist],type='vector')
cadopter = (padopter>.16)+0    # notice that we use a cutoff of 25% because it is harder to predict adopters
trueadopter = freemium$adopter[validsample]
(results = xtabs(~cadopter+trueadopter) )  # confusion matrix (columns have truth, rows have predictions)
(accuracy = (results[1,1]+results[2,2])/sum(results) )  # how many correct guesses along the diagonal
(truepos = results[2,2]/(results[1,2]+results[2,2]))  # how many correct "adopter" guesses
(precision = results[2,2]/(results[2,1]+results[2,2])) # proportion of correct positive guesses 
(trueneg = results[1,1]/(results[2,1]+results[1,1]))  # how many correct "non-adopter" guesses

# compute the predictions for the 10% of most likely adopterers (for validation sample)
topadopter = as.vector(padopter>=as.numeric(quantile(padopter,probs=.9)))
( baseconv=sum(trueadopter==1)/length(trueadopter) )  # what proportion would we have expected purely due to chance
( actconv=sum(trueadopter[topadopter])/sum(topadopter))  # what proportion did we actually predict
( lift=actconv/baseconv )  # what is the ratio of how many we got to what we expected




###############################################################################
### estimate a tree model with a sample of variables (consider the change from the previous period)
###############################################################################

# estimate a baseline model with a sample of variables
svarlist2=c("adopter","delta1_good_country","delta1_friend_cnt","dum_delta1_subsfrcnt","delta1_songsListened",
            "delta1_lovedTracks","delta1_posts","delta1_playlists","delta1_shouts")
ctree4 = rpart(adopter~., data=rfreemium[trainsample,svarlist2], control=rpart.control(cp=0.001))
fancyRpartPlot(ctree4)   # prettier plot

# predict probability (for validation sample)
padopter = predict(ctree4,newdata=rfreemium[validsample,svarlist2],type='vector')
cadopter = (padopter>.25)+0    # notice that we use a cutoff of 25% because it is harder to predict adopters
trueadopter = freemium$adopter[validsample]
(results = xtabs(~cadopter+trueadopter) )  # confusion matrix (columns have truth, rows have predictions)
(accuracy = (results[1,1]+results[2,2])/sum(results) )  # how many correct guesses along the diagonal
(truepos = results[2,2]/(results[1,2]+results[2,2]))  # how many correct "adopter" guesses
(precision = results[2,2]/(results[2,1]+results[2,2])) # proportion of correct positive guesses 
(trueneg = results[1,1]/(results[2,1]+results[1,1]))  # how many correct "non-adopter" guesses

# compute the predictions for the 10% of most likely adopterers (for validation sample)
topadopter = as.vector(padopter>=as.numeric(quantile(padopter,probs=.9)))
( baseconv=sum(trueadopter==1)/length(trueadopter) )  # what proportion would we have expected purely due to chance
( actconv=sum(trueadopter[topadopter])/sum(topadopter))  # what proportion did we actually predict
( lift=actconv/baseconv )  # what is the ratio of how many we got to what we expected


