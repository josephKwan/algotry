

#
# oversampling_logistic.R
#
# Often we encounter situations when 



######################################################################
# simulate a dataset
######################################################################

# first let's simulate a dataset with 100,000 observations
set.seed(6512098)      # set the seed so we all generate same random numbers
nobs=100000            # total number of observations
x1 = rnorm(nobs)       # simulate our first covariate
x2 = rnorm(nobs)       # simulate out second covariate
z = -3 + 2*x1 + 3*x2   # linear score for logistic model with "true" coefficients
pr = 1/(1+exp(-z))     # compute the probability using the inverse-logit function
churn = rbinom(nobs,1,pr)   # simulate churn using our model (where churn is our dependent variable)
proborig = mean(churn) # probability in the original dataset

# second let's estimate using all our data
alldata = data.frame(churn=churn,x1=x1,x2=x2)

# third let's subsample our data with just 10,000 true and 10,000 false
smallobs=10000          # small sample size
proboversample=0.5      # what is the conditional probability in our oversample 
trueidx=sample(which(alldata$churn==1),smallobs*proboversample)
falseidx=sample(which(alldata$churn==0),smallobs*proboversample)
smallidx=c(trueidx,falseidx)

# now let's compare our two datasets
summary(alldata)
summary(alldata[smallidx,])



######################################################################
# estimate our model
######################################################################

# let's estimate using all our data
mdlall = glm( churn ~ x1+x2, data=alldata, family='binomial')

# let's estimate using all a subset of the data
mdlsmall = glm( churn ~ x1+x2, data=alldata[smallidx,], family='binomial')

# notice that the coefficients of x1 and x2 are close, but 
summary(mdlall)
summary(mdlsmall)

# let's take the predictions from the models
prall = predict(mdlall,newdata=alldata,type='response')
prsmall = predict(mdlsmall,newdata=alldata,type='response')
summary(prall)
summary(prsmall)
summary(prall[smallidx])
summary(prsmall[smallidx])

# let's make an adjustment of the probability with the small index
prnewsmall = 1/(1+(1/proborig-1)/(1/proboversample-1)*(1/prsmall-1))
mean(prnewsmall)
