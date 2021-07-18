###############################################################################
###
### predictive model for cell2cell
### use for Part1 of the Exercise to estimate two different models:
### decision tree and a logistic regression
###
###############################################################################


###############################################################################
### setup
###############################################################################

# setup environment, for plots
if (!require(plyr)) {install.packages("plyr"); library(plyr)}
if (!require(reshape2)) {install.packages("reshape2"); library(reshape2)}
if (!require(gplots)) {install.packages("gplots"); library(gplots)}
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
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
# tools for logistic regression
if (!require(plotmo)) {install.packages("plotmo"); library(plotmo)}  # visualize regression
if (!require(visreg)) {install.packages("visreg"); library(visreg)}  # visualize regression
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}  # ROC curve for tree or logistic
# tools for random forest
if (!require(randomForest)) {install.packages("randomForest"); library(randomForest)} 
# tools for gradient boosted tree
if (!require(gbm)) {install.packages("gbm"); library(gbm)} 

# define a function to summary a classification matrix we will use later
# this is a function which has three arguments:
#    predprob  = vector of prediction probabilities
#    predclass = vector of predicted labels
#    trueclass = vector of true class labels
confmatrix.summary <- function(predprob,predclass,trueclass) {
  # compute confusion matrix (columns have truth, rows have predictions)
  results = xtabs(~predclass+trueclass)  
  # compute usual metrics from the confusion matrix
  accuracy = (results[1,1]+results[2,2])/sum(results)   # how many correct guesses along the diagonal
  truepos = results[2,2]/(results[1,2]+results[2,2])  # how many correct "churn" guesses
  precision = results[2,2]/(results[2,1]+results[2,2]) # proportion of correct positive guesses 
  trueneg = results[1,1]/(results[2,1]+results[1,1])  # how many correct "non-churn" guesses 
  # compute the lift using the predictions for the 10% of most likely
  topchurn = as.vector( predprob >= as.numeric(quantile(predprob,probs=.9)))  # which customers are most likely to churn
  ( baseconv=sum(trueclass==1)/length(trueclass) )  # what proportion would we have expected purely due to chance
  ( actconv=sum(trueclass[topchurn])/sum(topchurn))  # what proportion did we actually predict
  ( lift=actconv/baseconv )  # what is the ratio of how many we got to what we expected
  return(list(confmatrix=results,accuracy=accuracy,truepos=truepos,precision=precision,trueneg=trueneg,lift=lift))
}

# useful function for generating plot data
# assumes that cell2cell is public data and pchurn is vector of same length
genplotData = function(myvarlist,mychurn) {
  plotData <- lapply(names(cell2cell[,myvarlist]), function(x){
    out <- data.frame(
      var = x,
      type = c(rep('Actual',nrow(cell2cell)),rep('Predicted',nrow(cell2cell))),
      value = c(cell2cell[,x],cell2cell[,x]),
      churn = c(cell2cell$Churn,mychurn)
    )
    out$value <- out$value-min(out$value) #Normalize to [0,1]
    out$value <- out$value/max(out$value)
    out
  })
  plotData <- do.call(rbind,plotData)
  return(plotData)
}



###############################################################################
### prepare the dataset for analysis
###############################################################################

# import dataset from file (change the directory to where your data is stored)
setwd("~/Documents/class/data science/hw/cell2cell/data")  # !! change this to your directory !!

# import dataset from file (change the directory to where your data is stored)
cell2cell=read.csv("cell2cell_data.csv")
cell2celldoc=read.csv("cell2cell_doc.csv",as.is=TRUE)  # just read in as strings not factors
cell2cellinfo=cell2celldoc$description  # create a separate vector with just the variable description
rownames(cell2celldoc)=cell2celldoc$variable   # add names so we can reference like cell2celldoc["Revenue",]
names(cell2cellinfo)=cell2celldoc$variable  # add names so we can reference like cell2cellinfo["Revenue]

# set the random number seed so the samples will be the same if regenerated
set.seed(1248765792)

# prepare new values
trainsample=(cell2cell$Sample==1)
validsample=(cell2cell$Sample==2)
predsample=(cell2cell$Sample==3)
plotsample=sample(1:nrow(cell2cell),200)

# remove sample from the cell2cell set, since we have the sample variables
cell2cell$Sample=NULL

# recode the location so that we only keep the first 3 characters of the region
# and only remember the areas with more than 800 individuals, otherwise set region to OTH for other
newcsa=strtrim(cell2cell$Csa,3)  # get the MSA which is the first 3 characters
csasize=table(newcsa)  # count number of times MSA occurs
csasizeorig=rownames(csasize)  # save the city names which are in rownames of our table
csasizename=csasizeorig  # create a copy
csasizename[csasize<=800]="OTH"  # replace the city code to other for those with fewer than 800 customers
# overwrite the original Sca variable with the newly recoded variable using mapvalues
cell2cell$Csa=factor(mapvalues(newcsa,csasizeorig,csasizename))  # overwrites original Csa

# create a missing variable for age1 and age2
cell2cell$Age1[cell2cell$Age1==0]=NA  # replace zero ages with missing value
cell2cell$Age2[cell2cell$Age2==0]=NA  # replace zero ages with missing value
cell2cell$Age1miss=ifelse(is.na(cell2cell$Age1),1,0)  # create indicator for missing ages
cell2cell$Age2miss=ifelse(is.na(cell2cell$Age2),1,0)  # create indicator for missing ages

# replace missing values with means
nvarlist = sapply(cell2cell,is.numeric)  # get a list of numeric variables
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))  # define a function to replace NA with means
cell2cell[nvarlist] = lapply(cell2cell[nvarlist], NA2mean)  # lapply performs NA2mean for each columns



###############################################################################
### estimate a tree model with all variables
###############################################################################

# estimate a model with all the variables that is very deep
ctree.full = rpart(Churn~., data=cell2cell[trainsample,], control=rpart.control(cp=0.0005))
summary(ctree.full)
plot(ctree.full)
# uncomment the line below to view the full tree -- clearly needs pruning -- which is what the commands below do
#prp(ctree.full)  # make sure your plot window is large or this command can cause problems

# these lines are helpful to find the "best" value of cp
# A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line.
printcp(ctree.full)               # display table of optimal prunings based on complexity parameter
plotcp(ctree.full)                # visualize cross-validation results

# prune the tree back !! choose on of the lines below for treeA or treeB, and leave the other commented out !!
#ctree=prune(ctree.full,cp=0.005)  # prune tree using chosen complexity parameter !! simple tree (treeA) !!
ctree=prune(ctree.full,cp=0.00090890)  # prune tree using chosen complexity parameter !! better tree (treeB) !!

# visualize the trees 
par(mfrow=c(1,1))         # reset one graphic per panel
#plot(ctree); text(ctree)  # simple graph
prp(ctree,extra=101,nn=TRUE)  # add the size and proportion of data in the node
#fancyRpartPlot(ctree)     # fancy graphic  !! uncomment if you load library(rattle) !!
plotmo(ctree)             # evaluates selected input but holds other values at median
#plotmo(ctree,pmethod="apartdep")   # evaluates selected input and averages other values  !! pmethod="partdep" is better but slower !!

# compute predictions for the entire sample -- notice we only train on trainsample
pchurn.tree = predict(ctree,newdata=cell2cell,type='vector')
cchurn.tree = (pchurn.tree>.5)+0   # make our predictions using a 50% cutoff, !! this can threshold can be changed !!
truechurn = cell2cell$Churn

# compute confusion matrix and some usual statistics (uncomment train and prediction samples if you want to see these statistics)
#confmatrix.summary(pchurn.tree[trainsample],cchurn.tree[trainsample],truechurn[trainsample])  # summary for training sample (look to see if train is substantially better than valid)
confmatrix.summary(pchurn.tree[validsample],cchurn.tree[validsample],truechurn[validsample])  # summary for validation sample
#confmatrix.summary(pchurn.tree[predsample],cchurn.tree[predsample],truechurn[predsample]) # summary for prediction sample (use this when you want to know how good your "final" model is)

# compute ROC and AUC
rocpred.tree = prediction(pchurn.tree[validsample],cell2cell$Churn[validsample])  # compute predictions using "prediction"
rocperf.tree = performance(rocpred.tree, measure = "tpr", x.measure = "fpr")
plot(rocperf.tree, col=rainbow(10)); abline(a=0, b= 1)
auc.tmp = performance(rocpred.tree,"auc")  # compute area under curve
(auc.tree = as.numeric(auc.tmp@y.values))

# we can compare the predicted responses vs actual response for each variable as well
# note: we can do this for any model
svarlist=c("Eqpdays","Months","Revenue","Mou")  # pick out the top four important variables
plotData=genplotData(svarlist,pchurn.tree)
qplot(value, churn, data=plotData, facets = type ~ var, geom='smooth', span = 0.1)



###############################################################################
### estimate a logistic regression
###############################################################################

# !! either use the first block of code to create your own regression,  !!
# !! or uncomment the lrmdl in the second block of code to use a prespecified model !!

# run a step-wise regression
# first estimate the null model (this just has an intercept)
#null = glm(Churn~1,data=cell2cell[trainsample,],family='binomial')
# second estimate a complete model (with all variables that you are interested in)
#full = glm(Churn~.,data=cell2cell[trainsample,],family='binomial')  # takes a long time
# if you have time uncomment the following line and include all squared terms (e.g., nonlinear effects)
#full=glm(Churn~.^2,data=cell2cell[plotsample,],family='binomial')  # takes a very long time -- but since we just want the formula for stepwise can just use plotsample instead of trainsample
# finally estimate the step wise regression starting with the null model  # takes a long time (~5 min)
#lrmdl = step(null, scope=formula(full),steps=20,dir="forward")  # the final model is what is saved

# this logistic regression uses some important variables and is a gives a good model
# if you uncomment the stepwise regression then commnet out the following line
#lrmdl = glm(Churn~Eqpdays+Retcall+Months+Refurb+Uniqsubs+Mailres+Overage+Mou+Creditde+Actvsubs,data=cell2cell[trainsample,],family='binomial')
# two predefined logistic regression models (ranging from simple to complex)
# (lrA) final stepwise model (without interactions): model with . and steps=15
#lrmdl=glm(Churn~Eqpdays+Retcall+Months+Refurb+Uniqsubs+Mailres+Overage+Mou+Creditde+Actvsubs+Roam+Setprcm+Changem+Changer+Marryno,data=cell2cell[trainsample,],family='binomial')
# (lrB) model with ^2 and steps=20  (!! this is a better model, but more complex due to interactions!!)
lrmdl=glm(Churn~Eqpdays+Retcall+Months+Refurb+Uniqsubs+Mailres+Overage+Mou+Setprcm+Creditde+Actvsubs+Roam+Changem+Changer+Marryno+Age1+Eqpdays:Months+Months:Mou+Creditde:Changem+Overage:Age1,data=cell2cell[trainsample,],family='binomial')

# give a summary of the model's trained parameters
summary(lrmdl)
plotmo(lrmdl)             # evaluates selected input but holds other values at median
#plotmo(lrmdl,pmethod="partdep")   # evaluates selected input and averages other values  !! pmethod="apartdep" is faster but approximate !!

# compute predictions for the entire sample -- but model was only trained on trainsample
pchurn.lr = predict(lrmdl,newdata=cell2cell,type='response')
cchurn.lr = (pchurn.lr>.5)+0
truechurn = cell2cell$Churn

# compute confusion matrix and some usual statistics (uncomment train and prediction samples if you want to see these statistics)
#confmatrix.summary(pchurn.lr[trainsample],cchurn.lr[trainsample],truechurn[trainsample])  # summary for training sample (look to see if train is substantially better than valid)
confmatrix.summary(pchurn.lr[validsample],cchurn.lr[validsample],truechurn[validsample])  # summary for validation sample
#confmatrix.summary(pchurn.lr[predsample],cchurn.lr[predsample],truechurn[predsample]) # summary for prediction sample (use this when you want to know how good your "final" model is)

# compute ROC and AUC
rocpred.lr = prediction(pchurn.lr[validsample],cell2cell$Churn[validsample])  # compute predictions using "prediction"
rocperf.lr = performance(rocpred.lr, measure = "tpr", x.measure = "fpr")
plot(rocperf.lr, col=rainbow(10)); abline(a=0, b= 1)
auc.tmp = performance(rocpred.lr,"auc")  # compute area under curve
(auc.lr = as.numeric(auc.tmp@y.values))

# we can compare the predicted responses vs actual response for each variable as well
# note: we can do this for any model
svarlist=c("Eqpdays","Months","Revenue","Mou")  # pick out the top four important variables
plotData=genplotData(svarlist,pchurn.lr)
qplot(value, churn, data=plotData, facets = type ~ var, geom='smooth', span = 0.1)




#@randomforest
###############################################################################
### estimate a random forest
###
### uses randomForest package, but can also use cForest in party
### an advantage of cForest is that you can plot individual trees using prp
###############################################################################

# estimate random forest
# !! change ntree=20 to larger values, but can take lots of time !!
# !! nodesize=30 limits branches to at least 30 observations !!
# caution: can take ~5 minutes to train using the following settings  (if time permits increase ntree)
rfmdl = randomForest(Churn~.,data=cell2cell[trainsample,],ntree=20,proximity=TRUE,importance=TRUE,nodesize=30)

# summary of forest
summary(rfmdl)
plot(rfmdl)        # plots the error rates per # of trees
varImpPlot(rfmdl)  # dot plot of the importance of the variables

# we can pull out specific trees from the forest
getTree(rfmdl,k=1,labelVar=TRUE)  # k can be any number between 1 and ntree

# compute predictions for the entire sample -- notice we only train on trainsample
pchurn.rf = predict(rfmdl,newdata=cell2cell,type='response')
cchurn.rf = (pchurn.rf>.5)+0   # make our predictions using a 50% cutoff, this can threshold can be changed
truechurn = cell2cell$Churn

# compute confusion matrix and some usual statistics (uncomment train and prediction samples if you want to see these statistics)
#confmatrix.summary(pchurn.rf[trainsample],cchurn.rf[trainsample],truechurn[trainsample])  # summary for training sample (look to see if train is substantially better than valid)
confmatrix.summary(pchurn.rf[validsample],cchurn.rf[validsample],truechurn[validsample])  # summary for validation sample
#confmatrix.summary(pchurn.rf[predsample],cchurn.rf[predsample],truechurn[predsample]) # summary for prediction sample (use this when you want to know how good your "final" model is)

# compute ROC and AUC
rocpred.rf = prediction(pchurn.rf[validsample],cell2cell$Churn[validsample])  # compute predictions using "prediction"
rocperf.rf = performance(rocpred.rf, measure = "tpr", x.measure = "fpr")
plot(rocperf.rf, col=rainbow(10)); abline(a=0, b= 1)
auc.tmp = performance(rocpred.rf,"auc")  # compute area under curve
(auc.rf = as.numeric(auc.tmp@y.values))

# plot response model
plotmo(rfmdl)             # evaluates selected input but holds other values at median
#plotmo(rfmdl,pmethod="apartdep")   # evaluates selected input and averages other values  !! pmethod="partdep" is better but slower !!

# understand relationship with inputs
( rfmdl.imp=importance(rfmdl) ) # larger values correspond with more important variables
impvar = rownames(rfmdl.imp)[order(rfmdl.imp[, 1], decreasing=TRUE)]
par(mfrow=c(2,3))  # put 2x3=6 plots in a panel
# !! change impvar[1:3] to impvar to see all variables
# !! change plotsample to trainsample to see all fitted data
for (i in seq_along(impvar[1:6])) {
  partialPlot(rfmdl, cell2cell[plotsample,], impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]),ylim=c(0,1))
}

# we can compare the predicted responses vs actual response for each variable as well
# note: we can do this for any model
svarlist=c("Eqpdays","Months","Revenue","Mou")  # pick out the top four important variables
plotData=genplotData(svarlist,pchurn.rf)
qplot(value, churn, data=plotData, facets = type ~ var, geom='smooth', span = 0.1)



#@boostedtree
###############################################################################
### estimate a boosted tree with gbm
###
### some other alternatives are xgboost and LightGBM
### we use gbm since it has a nicer interface, but the others are newer and have
### computational advantages as well as other options
###############################################################################

# estimate gradient boosted tree
# !! add cv.folds=5 for cross validation to find best # of trees, and n.cores=2 if you have multiple CPUs
# !! warning: may take >15 mins for n.trees=10000 (change value as time permits)
gbmdl=gbm(formula=Churn~.,data=cell2cell[trainsample,],
          distribution="bernoulli",n.trees=1000,shrinkage=.01,n.minobsinnode=20,cv.folds=5)

# if you have cv.folds>1 above then black line is training bernoulli deviance,
# green line is the testing bernoulli deviance, and best tree indicated by vertical blue line since
# it minimizes the testing error on the cross-validation folds
par(mfrow=c(1,1))
gbmdl.best=gbm.perf(gbmdl)
#gbmdl=gbmdl.best  # use this as your best gbm tree

# summary of forest
summary(gbmdl)

# visualize a tree (this is only one of many trees that make up gbmdl)
pretty.gbm.tree(gbmdl, i.tree=1)

# compute predictions for the entire sample -- notice we only train on trainsample
pchurn.gb = predict(gbmdl,newdata=cell2cell,type='response',n.trees=20000)
cchurn.gb = (pchurn.gb>.5)+0   # make our predictions using a 50% cutoff, this can threshold can be changed
truechurn = cell2cell$Churn

# compute confusion matrix and some usual statistics (uncomment train and prediction samples if you want to see these statistics)
#confmatrix.summary(pchurn.gb[trainsample],cchurn.gb[trainsample],truechurn[trainsample])  # summary for training sample (look to see if train is substantially better than valid)
confmatrix.summary(pchurn.gb[validsample],cchurn.gb[validsample],truechurn[validsample])  # summary for validation sample
#confmatrix.summary(pchurn.gb[predsample],cchurn.gb[predsample],truechurn[predsample]) # summary for prediction sample (use this when you want to know how good your "final" model is)

# compute ROC and AUC
rocpred.gb = prediction(pchurn.gb[validsample],cell2cell$Churn[validsample])  # compute predictions using "prediction"
rocperf.gb = performance(rocpred.gb, measure = "tpr", x.measure = "fpr")
plot(rocperf.gb, col=rainbow(10)); abline(a=0, b= 1)
auc.tmp = performance(rocpred.gb,"auc")  # compute area under curve
(auc.gb = as.numeric(auc.tmp@y.values))

# plot response model
plotmo(gbmdl)             # evaluates selected input but holds other values at median
#plotmo(gbmdl,pmethod="apartdep")   # evaluates selected input and averages other values  !! pmethod="partdep" is better but slower !!

# understand relationship with inputs
( gbmdl.imp=summary(gbmdl) ) # larger values correspond with more important variables
impvar=rownames(gbmdl.imp)[order(gbmdl.imp[, 2], decreasing=TRUE)]
impvar=impvar[1:6]  # just six most important

# we can compare the predicted responses vs actual response for each variable as well
# note: we can do this for any model
svarlist=c("Eqpdays","Months","Revenue","Mou")  # pick out the top four important variables
plotData=genplotData(svarlist,pchurn.gb)
qplot(value, churn, data=plotData, facets = type ~ var, geom='smooth', span = 0.1)



#@compare
###############################################################################
### compare models using ROC plot
###############################################################################

# plot all ROC curves together
par(mfrow=c(1,1))
plot(rocperf.lr,col="red"); abline(a=0,b=1)
plot(rocperf.tree,add=TRUE,col="blue")
plot(rocperf.rf,add=TRUE,col="green")
plot(rocperf.gb,add=TRUE,col="violet")
legend("bottomright",c("LogRegr","Tree","Forest","GBM"),pch=15,col=c("red","blue","green","violet"),bty="n")

save.image(file="cell2cell_Part1_RandomForest.RData")
