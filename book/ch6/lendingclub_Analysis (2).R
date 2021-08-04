###############################################################################
# Script: lendingclub_Analysis.R
# Copyright (c) 2020 by R. Ravi (ravi@cmu.edu).  Distributed under license CC BY-NC 4.0
# Original example created by Ravi, this version by Alan Montgomery.
# To view this license see https://creativecommons.org/licenses/by-nc/4.0/
#
# predictive model for lendingclub that estimates both
# a logistic regression model and a classification and regression tree (CART)
###############################################################################



###############################################################################
### @setup the environment
###############################################################################

# setup environment, for plots
if (!require(gplots)) {install.packages("gplots"); library(gplots)}
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
# data manipulation
if (!require(plyr)) {install.packages("plyr"); library(plyr)}
# setup environment, make sure this library has been installed
if (!require(rstudioapi)) {install.packages("rstudioapi"); library(rstudioapi)}
# setup environment (if you want to use fancy tree plots)
if (!require(rpart)) {install.packages("rpart"); library(rpart)}
if (!require(rattle)) {install.packages("rattle"); library(rattle)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); library(rpart.plot)}
# tools for logistic regression
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}  # ROC curve for tree or logistic
if (!require(plotmo)) {install.packages("plotmo"); library(plotmo)}  # show model response
#Install a package for plotting correlations and include it
if (!require(corrplot)) {install.packages("corrplot"); library(corrplot)}

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
### @input the data and prepare the dataset for analysis
###############################################################################

# set to working directory of script (assumes data in same directory as script)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # only works in Rstudio scripts
# alternatively set the working directory manually
setwd("~/Documents/class/data science/examples/lendingclub") #!! set to your directory

# Read the loans data set saved in comma separated value format
loans = read.csv("loans-default.csv")

# split into training and validation samples  !! important to execute the set.seed before this group !!
set.seed(1248765792)        # set the random number seed so the samples will be the same if regenerated
randid=runif(nrow(loans))   # generate a random number between 0 and 1
trainsample=(randid<=0.6)   # use this sample for training
validsample=(randid>0.6)    # use this sample for validation (e.g. comparing models)
plotsample=sample(1:nrow(loans),200)  # use this small sample for plotting values (otherwise plots are too dense)



###############################################################################
### @exploratory analysis to understand the data
###############################################################################

# let's take a look at just one observation
print(loans[1,])

# Display the structure of the object read in, and summarize its fields
str(loans)
summary(loans)

# This is a good place to make sure you understand what the different fields mean
# E.g., the fraction of defaulters in the data set can be computed by taking the mean
mean(loans$default)

#Cross tabulate how the target variable and other categorical variables are related
xtabs(~credit.policy+default,data=loans)
xtabs(~purpose+default,data=loans)

# Simple histograms of the default variable versus categorical variables
# barplot is called on the table command which automatically bins the variable given to it 
# This first one shows the histograms of the defaulters
barplot(table(loans$default),
        main="Paid verus not paid loans",
        xlab="Status",
        border="black", 
        col=c("green","red"))

#... and how the defaulters varies with the second coordinate, the credit policy
barplot(table(loans$default, loans$credit.policy),
        main="Default Distribution by Credit Policy",
        xlab="Cust meets underwriting criteria",
        col=c("green","red"))
# You can adapt the above command to visually represent the variation of defaulters with other categorical variables
#   like delinq.2yrs, inq.last.6mnths etc. that have few values represented

# Box-and-whisker plots to see dispersion of continuous vars across default and not default
# boxplot uses the formula syntax, with LHS being the y-var (vertical) and RHS being the x-variables (horizontal)
boxplot(loans$int.rate ~ loans$default,main="Interest Rates across Default")
boxplot(loans$installment ~ loans$default,main="Installments across Default")
boxplot(loans$fico ~ loans$default, main = "FICO across Default")

#compute correlation matrix among numerical attributes in one large matrix
# cor is the command for computing correlations, c(1,3:14) indicates the column numbers 1 and 3 to 14 that have numerical 
# values and can be used to compute correlations
correlations = cor(loans[,c(1,3:14)])
round(correlations,2)  # the matrix is large, round the entries to 2 digits

#Generate a heat map of correlated predictors
#  (the hclust parameter orders the rows and columns according to a hierarchical clustering method)
corrplot(correlations, order="hclust")



###############################################################################
### @logistic regression model
### using stepwise regression model with all the variables and their interactions
###############################################################################

# specify a simple logistic regression model
lrmdl=glm(default~fico+installment,data=loans[trainsample,],family='binomial')

## !! if you want to build your own model run this portion of the code, otherwise skip to the next block of code !!
# uncomment following lines with ## in front of them)
# first estimate the null model (this just has an intercept)
null=glm(default~1,data=loans[trainsample,],family='binomial')
# second estimate a complete model (with all variables that you are interested in)
full=glm(default~.,data=loans[trainsample,],family='binomial')  # can be slow to estimate
# if you have time uncomment the following line and include all squared terms (e.g., nonlinear effects)
#full=glm(default~.^2,data=loans[plotsample,],family='binomial')  # takes a very long time -- but since we just want the formula for stepwise can just use plotsample instead of trainsample
# finally estimate the step wise regression starting with the null model
lrstep=step(null, scope=formula(full),steps=10,dir="forward")  # !! can increase beyond 10 steps, just takes more time
lrmdl=lrstep  # overwrite lrmdl with the new stepwise regression

# give a summary of the model's trained parameters
summary(lrmdl)
plotmo(lrmdl)             # evaluates selected input but holds other values at median
#plotmo(lrmdl,pmethod="partdep")   # evaluates selected input and averages across other values  !! pmethod="apartdep" is faster but approximate !!

# compute predictions for the entire sample -- but model was only trained on trainsample
pdefault.lr = predict(lrmdl,newdata=loans,type='response')
cdefault.lr = (pdefault.lr>0.16)+0  # !! change 0.16 cutoff as appropriate !!
truedefault = loans$default

# compute confusion matrix and some usual statistics (uncomment train and prediction samples if you want to see these statistics)
#confmatrix.summary(pdefault.lr[trainsample],cdefault.lr[trainsample],truedefault[trainsample])  # summary for training sample (look to see if train is substantially better than valid)
confmatrix.summary(pdefault.lr[validsample],cdefault.lr[validsample],truedefault[validsample])  # summary for validation sample
#confmatrix.summary(pdefault.lr[predsample],cdefault.lr[predsample],truedefault[predsample]) # summary for prediction sample (use this when you want to know how good your "final" model is)

# compute ROC and AUC
rocpred.lr = prediction(pdefault.lr[validsample],truedefault[validsample])  # compute predictions using "prediction"
rocperf.lr = performance(rocpred.lr, measure = "tpr", x.measure = "fpr")
plot(rocperf.lr, col=rainbow(10)); abline(a=0, b= 1)
auc.tmp = performance(rocpred.lr,"auc")  # compute area under curve
(auc.lr = as.numeric(auc.tmp@y.values))



###############################################################################
### @summary_logistic 
### summarize the logistic regression by saving the coefficients to CSV
###############################################################################

# create list of customer indices to extract for our analysis
userlist=c(1:4)  # just select the first 4 users

# create vector of variables used in model called mvarlist, and add other variables that we want to write out
# these lines require the lrmdl and ctree to be created appropriately above
mvarlist=names(coefficients(lrmdl))[-1]   # get the variables used in your logistic regression moodel, except the intercept which is in first position
mvarlist=unlist(unique(strsplit(mvarlist,":")))   # if you have interactions then you need to uncomment this line
#mvarlist.tree=names(ctree$variable.importance)   # if we want the list of variables in the tree then uncomment this line
#mvarlist=unique(c(mvarlist,mvarlist.tree))  # add the variables in the tree that are not in the lr model
evarlist=c()    # vector of extra variables to save -- regardless of whether they are in the model
varlist=c(mvarlist,evarlist)         # vector of variable names that we will use (all model variables plus ID and revenue)
print(varlist)  # vector of variables to save

# retrieve coefficients from your model
coeflist=summary(lrmdl)$coefficients  # extract coefficients estimates and std errors and z values
coefdata=data.frame(rn=rownames(coeflist),coeflist,row.names=NULL)  # change to dataframe
colnames(coefdata)=c("rn",colnames(coeflist))
print(coefdata)   # print out the coefficients

# retrieve data about the users (assumes that pchurn.lr and pchurn.tree have been computed in earlier part of script)
userpred=cbind(pdefault.lr[userlist])  # create matrix of predictions from our model for selected users
colnames(userpred)=c("pdefault.lr")  # label our columns appropriately
modeldata=model.matrix(lrmdl,data=loans[userlist,])  # construct the data used in the model
userdata=loans[userlist,evarlist]  # get additional variables
userdata=t(cbind(modeldata,userdata,userpred))  # get relevant data for a set of customers
userdata=data.frame(rn=rownames(userdata),userdata,row.names=NULL)  # change to dataframe
print(userdata)   # print out user data

# retrieve averages and std dev across all users
modelall=model.matrix(lrmdl,data=loans[trainsample,])  # get a matrix of all data used in the model (just training sample)
meandata=apply(modelall,2,mean) # compute the average for the selected variables (the "2" means compute by column)
sddata=apply(modelall,2,sd)  # compute the standard deviation for selected variables
descdata=data.frame(rn=names(meandata),meandata,sddata,row.names=NULL)  # merge the vectors with the mean and stddev into a single dataframe
print(descdata)   # print out the descriptive values

# combine the data together to make it easier to dump out to a single spreadsheet
mdata=join(coefdata,descdata,type='full',by='rn')  # merge the coefficients and descriptive stats
mdata=join(mdata,userdata,type='full',by='rn')  # create a final single dataframe
print(mdata)    # print out the combined data

# write the data to a spreadsheet
write.csv(mdata,file="lendingclub_lrmodeldata.csv")   # if you want you can import this file into excel for easier processing



###############################################################################
### @tree model
###############################################################################

# estimate a model with all the variables
# !! note: you can vary the depth of tree by changing cp to 0.0005 below (or other small values) !!
# rpart doesn't use cv.tree like in the previous case, instead you can use printcp and plotcp to correct complexity parameter
# A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line.
ctree.full = rpart(default~., data=loans[trainsample,], control=rpart.control(cp=0.001), model=TRUE)
summary(ctree.full)
plot(ctree.full)  #; text(ctree.full)  # view the full tree
#prp(ctree.full)                    # view the full tree -- but clearly it needs to be pruned -- which is what the next set of commands do

# these lines are helpful to find the "best" value of cp, !! uncomment to determine best cp -- but this is already done for treeB below !!
# A good choice of cp for pruning is often the leftmost value for which we are confident the mean lies below the horizontal line.
printcp(ctree.full)               # display table of optimal prunings based on complexity parameter
plotcp(ctree.full)                # visualize cross-validation results
# Relative-error is the ratio of the misclassificaton error to the root node (or first tree), which is why it is 1.
# X-error is the cross validated error using a 10-fold split of the training data.
# xstd is a measure of the uncertainty associated with the estimate.
# The plot shows the X-error in the y-axis and the x-axis gives different values of cp.
# The dotted line is the minimum of the curve plus 1 standard error.
# So by choosing the first vertical line that falls under the dashed line -- we are picking the smallest tree size 
# that is within 1 standard error of the minimum.

# prune the tree back !! change cp !!
ctree=prune(ctree.full,cp=0.005)  # prune tree using chosen complexity parameter, try cp=0.0048831 or cp=0.0032407
#ctree=ctree.full                 # uncomment this line to use the full tree, with cp=0.001

# visualize the trees 
par(mfrow=c(1,1))         # reset one graphic per panel
#plot(ctree); text(ctree)  # simple graph
#prp(ctree)                # tree graph
prp(ctree,extra=101,nn=TRUE)  # add the size and proportion of data in the node
fancyRpartPlot(ctree)     # fancy graphic  !! uncomment if you load library(rattle) !!
plotmo(ctree)             # evaluates selected input but holds other values at median
#plotmo(ctree,pmethod="apartdep")   # evaluates selected input and averages other values  !! pmethod="partdep" is better but slower !!

# compute predictions for the entire sample -- notice we only train on trainsample
pdefault.tree = predict(ctree,newdata=loans,type='vector')
cdefault.tree = (pdefault.tree>0.16)+0   # make our predictions using a 16% cutoff, !! this can threshold can be changed !!
truedefault = loans$default

# compute confusion matrix and some usual statistics (uncomment train and prediction samples if you want to see these statistics)
#confmatrix.summary(pdefault.tree[trainsample],cdefault.tree[trainsample],truedefault[trainsample])  # summary for training sample (look to see if train is substantially better than valid)
confmatrix.summary(pdefault.tree[validsample],cdefault.tree[validsample],truedefault[validsample])  # summary for validation sample
#confmatrix.summary(pdefault.tree[predsample],cdefault.tree[predsample],truedefault[predsample]) # summary for prediction sample (use this when you want to know how good your "final" model is)

# compute ROC and AUC
rocpred.tree = prediction(pdefault.tree[validsample],truedefault[validsample])  # compute predictions using "prediction"
rocperf.tree = performance(rocpred.tree, measure = "tpr", x.measure = "fpr")
plot(rocperf.tree, col=rainbow(10)); abline(a=0, b= 1)
auc.tmp = performance(rocpred.tree,"auc")  # compute area under curve
(auc.tree = as.numeric(auc.tmp@y.values))



###############################################################################
### @compare models using ROC plot
###############################################################################

# plot all ROC curves together
plot(rocperf.lr,col="red"); abline(a=0,b=1)
plot(rocperf.tree,add=TRUE,col="blue")
legend("bottomright",c("LogRegr","Tree"),pch=15,col=c("red","blue"),bty="n")



###############################################################
## @evaluate business metric counterfactuals for the whole data set
## Using your model simulate what would have happened if lendingclub
## follows your recommendations. This is known as a "counterfactual"
## or a "what if..." simulation.
#############################################################

# Calculate the estimated set of 36 monthly payments for all loans
Payments = loans$installment*36
# Calculate the principal using Total.Payment = 36*Installment = Principal * exp (3*Int.rate)
Principal = loans$installment*36/exp(3*loans$int.rate)
# This simpler formula is less accurate:
#   PrincipalFull <- loans$installment*36/(1+3*loans$int.rate)
# Compute Profits
Profit = Payments * (1-loans$default) - Principal

# Compute total profit on the full loan set
(TotalProfit = sum(Profit))
# Compute total principal processed on the full loan set
(TotalPrincipal = sum(Principal))
# Compute return on invested capital on the full loan set - 
#    annual hence divide by 3, the term of loan
(ROIC = TotalProfit/(3*TotalPrincipal))

# compute predictions for the entire dataset
pdefault.model = pdefault.lr  # choose your model pdefault.tree
cdefault.model = (pdefault.model>0.16)+0  # !! change 0.16 cutoff as appropriate !!
truedefault = loans$default
# compare predictions again
confmatrix.summary(pdefault.model[validsample],cdefault.model[validsample],truedefault[validsample])  # summary for validation sample

# now evaluate profit and performance metrics
( sum(cdefault.model==0) )  # how many loans funded
( sum(cdefault.model==0)/length(cdefault.model) )  # percentage of loans funded
# assumes that you will not earn anything from those that you classify as default
myProfit=Profit; myProfit[cdefault.model==1]=0
myPayments=Payments; myPayments[cdefault.model==1]=0
myPrincipal=Principal; myPrincipal[cdefault.model==1]=0

# Compute total profit on the full loan set
(myTotalProfit = sum(myProfit))
# Compute total principal processed on the full loan set
(myTotalPrincipal = sum(myPrincipal))
# Compute return on invested capital on the full loan set
(myROIC = myTotalProfit/(3*myTotalPrincipal))




###############################################################
## @solution of what is the "best" cutoff
## let's try many values
#############################################################

# try lot's of values
vcutoff=sort(unique(pdefault.model))
vLoans=rep(0,length(vcutoff))
vTotalProfit=rep(0,length(vcutoff))
vTotalPrincipal=rep(0,length(vcutoff))
vTotalPayments=rep(0,length(vcutoff))
vROIC=rep(0,length(vcutoff))
for (i in 1:length(vcutoff)) {
  # classify using cutoff
  cdefault.model = (pdefault.model>vcutoff[i])+0
  # now evaluate profit and performance metrics
  vLoans[i]=sum(cdefault.model==0)  # how many loans funded
  # assumes that you will not earn anything from those that you classify as default
  myProfit=Profit; myProfit[cdefault.model==1]=0
  myPayments=Payments; myPayments[cdefault.model==1]=0
  myPrincipal=Principal; myPrincipal[cdefault.model==1]=0
  # Compute total profit on the full loan set
  vTotalProfit[i] = sum(myProfit)
  # Compute total principal processed on the full loan set
  vTotalPrincipal[i] = sum(myPrincipal)
  # Compute return on invested capital on the full loan set
  vROIC[i] = vTotalProfit[i]/(3*vTotalPrincipal[i])
}

# plot results
plot(vcutoff,vLoans,type="l",xlab="Cutoff",ylab="Loans Given")
plot(vcutoff,vTotalProfit,type="l",xlab="Cutoff",ylab="Profits")
plot(vcutoff,vTotalPrincipal,type="l",xlab="Cutoff",ylab="Principal Loaned")
plot(vcutoff,vROIC,type="l",xlab="Cutoff",ylab="ROIC")

