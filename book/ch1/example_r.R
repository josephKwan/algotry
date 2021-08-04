
#
# Program..: example_r.R
# Notes....: Understanding R
#


### Expressions ###

# Comments use an '#', so anything after a '#' is ignored.
# R evaluates expressions at the command prompt and prints the results by default
3+3
sin(pi)
sqrt(100)

# if your expression is incomplete then R prompts with a +
# if you get stuck then press escape.
sqrt(
  100
)
# uncomment following line to illustrate error
#sqrt(
#))


### Basic Data Types in R ###

# there are five basic data types: integer, numeric, logical, character, complex

# integer, either use L after number or as.integer function
3L   # the integer 3
as.integer(3)  # the integer 3

# numeric, which are real, decimal or floating point values
3
3.0
pi   # R also has some constants predefined

# logical values, which are either TRUE or FALSE
TRUE
FALSE

# character values or strings
'able is five'

# complex values
1+0i
2.3+4.1i

# also there is a special value for nothing
NULL

# and a special value for a missing value
NA


### Assignments ###

# assign variables using the assignment operator '=' or '<-'
weight=190
weight <- 190 # this is the same thing, but equal is usually easier to read
# notice that after an assignment nothing is printed
# entering an object by itself will result in the object being printed
weight
# or we could use the print command
print(weight)
# or a final technique is enclose the expression in paranthesis so that
# the left hand side of the assignment is printed
( weight=190 )

# variables can be numeric as above or character values which are in quotes
person="Jim"
person


### Vectors and Functions ###

# we can create vectors of values using the collect operator
weights=c(190,100,120)
weights

# often we use functions to manipulate or create data
# to simulate 10 observations from a standard normal distribution
x=rnorm(10)
x

# another function allows us to compute the average of the values
mean(x)
# we could also save the output to a new object
xmean=mean(x)

# we can create vectors of strings as well
people=c("Jim","Sue","Dave")
people

# there is a special sequence operator ":"
5:10
# which is the same as the function seq
seq(5,10)


### Defining your own function ###

# let's define a function that adds 1 to each element
myfunction = function(x) {
  return(x+1)
}
myfunction( 5:10 )

# we can create arguments as well
myfunction = function(x,increment=1) {
  return(x+increment)
}
# notice that we have two arguments, and the second by default is 1
myfunction( 5:10 )  # same as before
# we can override the default value by given the value
myfunction( 5:10, 2 )  # add 2 to everything

# functions can have loops and logic
# this function adds 1 if the value is less than 7 and 2 otherwise
myfunction = function(x) {
  y=x  # create new value
  for (i in 1:length(x)) {
    if (x[i]<7) {
      y[i]=x[i]+1
    } else {
      y[i]=x[i]+2
    }
  }
  return(y)
}
myfunction( 5:10 )


### Objects ###

# notice that we have been creating variables or objects
# object names may contain letters, numbers, and dots
#    height, weight, x.var, .yvar, x.y.var, or x110 are all valid
# but object names cannot use a hyphen, begin with a number or use a reserved symbol (like TRUE, T, NA, $)

# to list the workspace we can use ls or objects
ls()
objects()

# if we wish to remove an object from the workspace use the remove funciton
rm(x)
ls()

# all functions are objects, and if we type the object name by itself it prints it out
myfunction
myfunction( 5:10 )  # this evalutes the function


### Objects as Variables ###

# we can easily create new objects from other ones or functions
x=1:10
y=c(x,10)
length(y)

# basic mathematical operations are available and scalar functions work on an elementwise basis
x=1:5
x^2
2*x
2*x+sqrt(x)


### Types of Data ###

# there three main types of data is numeric, character and logical
# here is an example of a logical vector
x=rnorm(5)
x
x<0

# R also has a special way of representing missing values with NA
x=c(1,NA,3)
x
# since this value is missing R will keep it as missing after operations
x+1
# unfortunately, sometimes this means the output will be missing
sum(x)
# we can locate missing values using the is.na function
is.na(x)


### Vector Indexing ###

# another powerful feature of R is the indexing operator using brackets
x=c(2,4,6,8,10)
x
x[1]
x[3:5]
x[c(1,3,5)]

# if we want to omit specific values use negative indices
x[-(1:3)]


### Logical Indices ###

# logical indices allows use to select specific values
x=rnorm(5)
x
x[x<=0]
# remember that the logarithmic function is not defined for negative values
log.x=log(x)
x[is.na(log.x)]
# to only select the non missing values use the not missing expression
log.x[!is.na(log.x)]

# we can also replace values using the indexing operator on the left hand side
x=sample(1:8)
x
# replace the 6th element with a missing value
x[6]=NA
x
# use the is.na operator to replace missing values with zero
x[is.na(x)]=0
x


### Functions ###

# functions are called using the function name followed by parantheses
# without the parantheses you are referring to the object
seq(1,5)
# arguments can have names like length=6
seq(10,20,length=6)
# we can pass arguments by position or by name
seq(to=100,by=15,length=7)
# we can often omit many arguments
seq(length=10)

# rep is a repeat function
rep(1:3,2)
rep(1:3,length=8)
rep(1:3,c(3,2,1))
rep()
rep(1:3,c(3,2,1),5)


### Visualizing Data ###

# one of the best features of R is it ability to create graphics
x=rnorm(100)
graphic=hist(x)
graphic


### Matrices ###

# another type of data is a matrix
x=matrix(1:10,nrow=2)
x
dim(x)
x.matrix=matrix(c(20,10,3,1,7,4),ncol=2)
x.matrix

# matrices can have names associated with the columsn
x=matrix(c(1:20),ncol=4,byrow=T,dimnames=list(NULL,c("col1","col2","col3","col4")))
x

# specifying byrow=T forces R to read the data in row by row
x.matrix=matrix(c(20,10,3,1,7,4),ncol=2,byrow=T)
x.matrix

# to extract a value from a matrix, use two elements in the subscript
x=matrix(1:15,nrow=3,byrow=T)
x
# this will select just a single element
x[2,3]
# this will select a submatrix
x[2:3,3:5]
# to select a specific column leave the row location blank
x[,1]
# to select a specific row leave the column location blank
x[1,]


### Constructing Matrices from Vectors ###

# matrices can be constructed from row vectors using cbind
x=c(3,4,5)
y=c(6,7,8)
x.y=cbind(x,y)
x.y

# or combining rows
x=c(3,4,5)
y=c(6,7,8)
x.y=rbind(x,y)
x.y


### Data Coercion ###

# if we try to mix data types, then R coerces to characters
x=c(3,4,5)
y=c("Three","Four","Five")
x.y=rbind(x,y)
x.y


### Lists ###

# if we do not want to coerce these objects to common formats use a list (or data frame)
x=c(3,4,5)
y=c("Three","Four","Five")
x.y=data.frame(x,y)
x.y

# lists are collections of different types of data
x=list("alan",2)
x[[1]]  # to get the first element of list
x[[2]]  # to get the second element of list
x=list(name=c("alan","peter"),value=c(2,3,4,5))
x$name  # we can access the first element using the $name
x$value
x$name = c("alan","peter","henry")


### Data frames ###

# data frames are special types of lists where each element of the list has the same length
x=data.frame(name=c("alan","alan","john","john"),treatment=c(1,2,1,2),value=c(10,20,5,15),stringsAsFactors=FALSE)

# a useful function to understand the structure is str
str(x)

# data frames can be subsetted just like matrices
x[2,3]  # to get the 3 columns from the second row

# but it is easier to refer to it by name
x[2,"value"]
x$value[2]  # we can use list form to refer to a variable and then select the value

# we can also extract an observation or column
x[2,]  # this returns a data.frame
x[,3]  # this returns a vector


### Factors ###

# factors are a convenient way R to store categorical data (strings or values that are repeated)
x.names = as.factor(x$name)  # factors have levels and values
print(x.names)  # this looks similar to x$name
str(x.names)    # but it is stored as a factor

# factors have two parts levels (the string) and an integer (the index)
levels(x.names)  # notice this gives a vector of string
as.integer(x.names)  # the each values are stored as integers that reference the levels

# factors make it easy to rename the levels
levels(x.names)[1]="Alan"  # by separating it makes it easier to manipulate the levels separately
x.names  # notice that "alan" has become "Alan"

# we can also convert a factor to a string
as.character(x.names)  # we can coerce the factor to strings

