###########################
######## Workspace ########
###########################

## Objects: numbers, vectors, matrices, 
## functions, dataframes, lists

# R is case sensitive 
x <- 1
X
x

## Objects' names: use letters and numbers
## symbols and space are not allowed

x.1 <- 1:10 # from : to
#1x error

x <- 2 # or
y = 3
3 -> z

x; y

# list the workspace content
ls()

# remove a specific object
rm(x)
ls()

# clean the workspace
rm(list=ls())
ls()

# save the workspace
save.image("myfile.RData")

# load the workspace
load("myfile.RData")

## Working directory
getwd()
setwd(path) # or Session --> Set working directory

## Help 
?function.name # or help(function.name)
??function.name # or help.search(function.name)

## Quit R
q()


###########################
##### Basic commands ######
###########################

## logical values: TRUE or FALSE
12>10

x <- y <- 1:10
x; y
x>=y

z <- 4
x = z 
x == y

w <- y>3
w

sum(y>3); sum(w) # logical coerced to integer
sum(y!=3) # not equal

# %in% in the set
x %in% y
y %in% x


## arithmetic operators
-x
# + addition
x + z
x + y

# * multiplication
x * z
x * y

# ^ exponentiation
x ^ z
x ^ y

# use brackets to modify the precedence 
# of the arithmetic operators
x+y/z
v <- (x+y)/z
v

?Arithmetic


###########################
##### Data structure ######
###########################

## vector: collection of elements of the same type
p <- "Hello world!"
p <- c(p,"Hello world!")

#data type
class(p) # character
class(w) # logical
class(y) # integer
class(v) # numeric

# alternatives to create vector: c(), rep(), seq() [numeric]
seq(from=1, to=10, by=.5)
seq(from=1, to=10, length.out=5)

# elements' selection

x <- c(10, 5, 6.4, 3, 8, 2) 
y <- c("A", "B", "C")
# second element 
y <- x[2]        # overwrite y
y
# second, third and fourth elements
z <- x[2:4]      # z is a new object
z
# seconf and fifth elements
w <- x[c(2, 5)]    
w

x*w # evaluated element by element

x <- c(1, 2, 4, 8, 16, 32)
x
x[-4]

x <- 1:8
x[(x>7) | (x<5)]

# Basic functions
x <- 3:22
length(x) 
sort(x)
max(x)
min(x)
range(x)
sum(x)
prod(x)
mean(x)
round(x/2.2,1)
which(x%%2 == 0) # which are even?
which.max(x)
which.min(x)

# factor
f <- factor(rep(c("M","F"), times=5)) # or
f2 <- factor(rep(c("M","F"), each=5))

f
f2
levels(f2)
f2 <- relevel(f2, ref="M")
f2

table(f2)

?cut
ff <- rep(1:4, each=2)
ff

cut(ff, breaks = c(0,1,2,3,4)) #unique cut points
cut(ff, breaks = 2) #number of intervals into which x is to be cut

# ordered factor
f3 <- ordered(rep(c("1-2","3-4"), each=5))
f3

## matrix 
#create
m <- matrix(c(1:12), nrow=3, ncol=4, byrow=FALSE)
m
m <- matrix(v, nrow=5, byrow=TRUE)
m
cbind(x,y,w) #numeric
rbind(x,y,p) #coerced to character

# elements' selection
m
dim(m)
m[1,2] #select an element
m[2,2]

m[2,] #select a row
m[,2] #select a column

m[c(1,4),]
m[2:4,]


## list
l <- list(x,y,z,w,f,p,m)
l
length(l)


# elements' selection
l[[1]]
l[[5]]

## data frame: matrix-like structure whose columns 
## may be of differing types

# create
df <- data.frame(x,y,w,p,f,v)
df
str(df) #character coerced to be factors


cbind.data.frame(x,y,w,p,f,v,m)
is.data.frame(df)
m
as.data.frame(m) 

# info and dimensions
str(df)
dim(df)
ncol(df) # number of columns
nrow(df) # number of rows
head(df) # first few rows
tail(df) # first few columns

# add a new column
df$age <- c(5:24) 
str(df)

# select elements
df[2,]
df$p
df$age[2:7]

df$age[df$f=="F"] # or
df[df$f=="F","age"]
df[df$f=="F",c("age","w")]
df[df$f=="F",c(7,3)]

df[df$f=="F" & df$age>=15,]
df[df$f=="F" | df$age>=15,]

# subset()
subset(df, f=="F" & age>15, select=c("age"))

# colnames
colnames(df)
colnames(df)[5] <- "gender" 
str(df)

# functions
mean(df$age)
sort(df$w)

# attach()
age
attach(df)
age
age <- age+1
age
detach(df)
df

# clean the workspace

## Packages
install.packages("MASS")
#update.packages()
library(MASS)


##############
## Exercise ##
##############

data(Insurance)
?Insurance
str(Insurance)

#1. Select the units with Age<29, with Group greater than 2 litre 
#   or with more than 20 Holders
#2. Select the variables District and Claims for units with Age 
#   lower than 25
#3. Select the subset of units belonging to District 4 or more 
#   than 300 Holders, excluding the variable Age from the selection


#################################
#### Data frame manipulation ####
#################################

# split(x,f)
# divides the data in the vector x into the groups defined by f. 
split(Insurance$District, Insurance$Age)

# by(data, INDICES, FUN, ..., simplify = TRUE)
by(Insurance$Claims, Insurance$Group, sum)

# apply(X, MARGIN, FUN, ...)
?apply
apply(Insurance[,c(4,5)], 1, sum)
apply(Insurance[,c(4,5)], 2, sum)


################################
######## Read and write ########
################################

?read.csv
?write.csv

#write.table(object, file=path,...)

################################
######## Missing values ########
################################

# coded as NA in R
# when importing data from an external file, it is very likely that 
# the missing data is labelled with something different (*, "",999,...)
# You have two options:
# remove missing values when you import the dataset specifying 
# the argument na.strings in the read.format() function
# or
# read the data and then remove missing values

# <dataframe>[<dataframe> == 999] = NA

# to verify if there are missings
?is.na #... and also to subset your data
x1 <- c(rep(1,5), rep(NA,2), seq(2,5,length.out=8))
x1

x1[is.na(x1)]
x1 <- x1[!is.na(x1)]
x1

# alternatetively use na.omit
x2 <- c(rep(c("F","M"),each=5), rep(NA,2))
x2
na.omit(x2)

# or again..
?complete.cases #return the dataset without missing values

# to ignore missings in computing descriptive stats
# use the argument na.rm in the functions
x1 <- c(rep(1,5), rep(NA,2), seq(2,5,length.out=8))

sum(x1)
sum(x1, na.rm = TRUE)
mean(x1, na.rm = TRUE)

 
##############
## Exercise ##
##############
# The feeling dataset concers the attitude of American voters towards 
# different categories of individuals (data collected in 2016 available 
# at www.voterstudygroup.org). Each variable expresses a score from 0 
# to 100, indicating an unfavorable, indifferent or favorable attitude 
# towards the groups of people object of the question

load("feeling.Rdata")

#1. Define a categorical variable from the variable ft_immig_2016 with 
#   the following classes: strongly unfavorable (0-25), unfavorable or 
#   indifferent (26-50), lightly favorable (51-75), favorable (76-100). 
#   Add this new variable to the data.
#2. Remove all NA values, and save the data in the .csv format.

# what command do you use to reload the data set: load or read....?

################################
###### Programming with R ######
################################

# You can write your own functions (and even packages!)
# pay attention to the names of the arguments
# objects defined in the function are not saved in your
# workspace

# function
#  myfunction <- function(arg1,arg2,...){
#    statements
#    return(output)
#  }

# a very simple example
mysum <- function(a=1,b=2){
    output <- a+b
  return(output)
} 
# now it is in the workspace

# invoking the function
mysum()
mysum(3,5)

# If-else statements
#  if(cond) expr1 else expr2 
#  ifelse(cond, if.true, if.false)

a <-5; b <-3
ifelse(a<b, a-b, a+b)

#for loops
#  for(name in expr1) expr2
s <- 0
for(i in 1:5){ 
  s <- s + i #when you have several expressions use brackets
}
  s

#while loops
#  while(condition) expr
s <- 0
while(s < 15) s <- s + i

##############
## Exercise ##
##############

#1. Given a dataset, write a function that computes and returns the minimun, 
#   and the maximum of each numerical variable 
#2. Test your function on the Insurance dataset
