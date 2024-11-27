#Problem 1
#1(a) 
#Creating a vector
x <- c(3, 12, 6, -5, 0, 8, 15, 1, -10, 7)
x

#1(b) 
#Creating a new vector y with 10 elements ranging from the minimum value of x to the maximum value of x
y <- seq(min(x),max(x),length.out=10)
y

#1(c)
#Sum of elements of x
sum(x)
#Mean of elements in x
mean(x)
#Standard Deviation of x 
sd(x)
#Variance of x
var(x)
#Mean Absolute Deviation od x
mad(x)
#Quartiles for x
quantile(x,c(0.25, 0.5, 0.75,1))
#Quantiles for x
quantile(x,c(.20,.40,.60,.80,1))

#Sum of elements of y
sum(y)
#Mean of elements in y
mean(y)
#Standard Deviation of y 
sd(y)
#Variance of y
var(y)
#Mean Absolute Deviation of y
mad(y)
#Quartiles for x
quantile(y,c(0.25, 0.5, 0.75,1))
#Quantiles for x
quantile(y,c(.20,.40,.60,.80,1))

#1(d) 
#Creating a new 7 element vector z to randomly sample from x with replacement.
sample(x,7,replace=TRUE)

#1(e)
#Computing a statistical test for differences in means between the vectors x and y
t.test(x,y)

#1(f)
#Sort the vector x and re-run the t-test as a paired t-test.
x
x[order(x)]
t.test(x,y,paired=TRUE)

#1(g)
#Creating a logical vector that identifies which numbers in x are negative
z <- x<0
z

#1(h)
#Removing all entries with negative numbers from x
x[x>=0]




#Problem 2
#2(a)
#Creating the dataframe X
col1 <- c(1,2,3,NA,5)
col2 <- c(4,5,6,89,101)
col3 <- c(45,NA,66,121,201)
col4 <- c(14,NA,13,NA,27)

X <- data.frame(rbind (col1,col2,col3,col4))
str(X)
#displaying all rows in X with missing values
X[!complete.cases(X),]

#2(b)
y <- c(3,12,99,99,7,99,21)
#2(b)(i)
#Replacing any 99’s in the vector y with ‘NA’.
y[y==99]<-NA
y
#2(b)(ii)
#count the number of missing values in y
sum(is.na(y))

#Problem 3

install.packages("tidyverse")
library(tidyverse)

#3(a)
#Reading the data into a data frame in R
college <- read.csv("college.csv")
#Calling the data frame college
college

#3(b)
rownames (college) <- college [,1]
View (college )
#Eliminate the first column in the data where the names are stored
college <- college [,-1]
college

#3(c)(i)
#Producing a numerical summary of the variables in the data set
summary(college)

#3(c)(ii)
#Access help for the pairs function and then use pairs to produce a scatterplot matrix of the first ten columns
help("pairs")
college[,1] = as.numeric(factor(college[,1]))
pairs(college[,1:10])

#3(c)(iii)
college$Private<-as.factor(college$Private)
#Using the plot() function to produce side-by-side boxplots of Outstate versus Private
plot(college$Private,college$Outstate,main= "boxplots of Outstate versus Private",xlab='Private',ylab='Outstate')

#3(c)(iv)
#Making all the values to "No" using rep function
Elite <- rep ("No", nrow(college ))
#If the Top10perc variable is greater than 50 then we make it Yes
Elite [college$Top10perc >50] <- "Yes"
#Used to change the character to factor
Elite <- as.factor (Elite)
#Joining the elite column to college
college <- data.frame(college ,Elite)
#3(c)(v)
#Using the summary() function to see how many elite universities there are
summary(college$Elite[college$Elite=="Yes"])
#3(c)(vi)
#Producing side-by-side boxplots of Outstate versus Elite
college$Elite<-as.factor(college$Elite)
plot(college$Elite,college$Outstate,main="boxplots of Outstate versus Private",xlab='Elite',ylab='Outstate')
#3(c)(vii)
#divide the print window into four regions
par(mfrow=c(2,2)) 
#Using the hist() function to produce histograms
hist(college$Top10perc,breaks = 10) 
hist(college$Top25perc, breaks = 7) 
hist(college$Accept,breaks = 5) 
hist(college$PhD,breaks = 3)

#Problem 4

#4(a)
#Loading the data frame baseball in the plyr package
install.packages("plyr")
library(plyr)
#Getting information about the data set and definitions for the variables
?baseball
baseball
#4(b)
#Setting sf to 0 for players before 1954
baseball$sf[baseball$year<1954] <- 0
#Setting missing values in hbp to 0
baseball$hbp[is.na(baseball$hbp) ] <- 0
#Exclude all player records with fewer than 50 ab
baseball <- subset(baseball,subset=ab>=50)
baseball

#4(c)
#Making all the values to 0 using rep function and Computing on base percentage in the variable obp
obp<- rep(0, nrow(baseball))
obp<-(baseball$h + baseball$bb + baseball$hbp)/ (baseball$ab + baseball$bb + baseball$hbp +baseball$sf)
#Adding the column to dataframe
baseball<-data.frame(baseball,obp)
baseball
#4(d)
#Sort the data based on the computed obp and print the year, player name, and on base percentage for the top five records based on this value.

b_obp<-baseball[order(-baseball$obp),]
b_obp
b_obp[1:5,c("year","id","obp")]

#Problem 5
#5(a)
#Load the quakes data from the datasets package
install.packages("datasets")
library(datasets)
quakes

#5(b)
#Plotting the recorded earthquake magnitude against the earthquake depth
plot(quakes$depth,quakes$mag)

#5(c)
#Using aggregate to compute the average earthquake depth for each magnitude level
#Storing these results in a new data frame named quakeAvgDepth
quakeAvgDepth <- data.frame(aggregate(quakes$depth,by = list(quakes$mag),FUN= mean)) 
quakeAvgDepth

#5(d)
#Changing column name in quakeAvgDepth 
names(quakeAvgDepth)[names(quakeAvgDepth) == 'Group.1'] <- 'AgMag'
names(quakeAvgDepth)[names(quakeAvgDepth) == 'x'] <- 'AgDep'
quakeAvgDepth

#5(e)
#Plotting between average depth to magnitude 
plot(quakeAvgDepth$AgDep,quakeAvgDepth$AgMag)
