############################################################
##  R/DATA ANALYTICS TASTER SESSION: R SCRIPT FILE
############################################################

##############################
# REQUIRED LIBRARIES
##############################

# Before running this code in RStudio you will need to install a number of libraries.

# Run the following code only once:
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("caret")
# install.packages("gridExtra")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("randomForest")

# Run this code everytime:
library("lubridate")
library("ggplot2")
library("caret")
library("gridExtra")
library("rpart")
library("rpart.plot")
library("randomForest")


##############################
# Calculations 

# You can perform basic operations in R including: Addition (+), Subtraction (-), Multiplication(*), Exponents (^), Modulus (%%).     

2 + 2

##############################
# Objects 

# You can create objects and assign outputs of functions to them, using the assignment operator "<-"     

d1 <- 2
d1


d2 <- 2 + 2
d2

##############################
# Objects 

# You can create objects of different types, the three key ones are:     

obj1 <- 2
obj2 <- TRUE
obj3 <- "hello"

class(obj1)

class(obj2)

class(obj3)


##############################
# Vectors 

# These objects that we just created are actually stored in R as vectors of length one. To create a longer vector we can use the concatenation function, "c":     

g <- c(2 + 2, 7, 1234, 34, 78000000)
g

class(g)

# We can see the class of this vector is numeric, as all of its elements are numeric.     
##############################
# Vectors 

# Vectors can contain logical, character and numeric elements, but will store them as all one type, the "highest" that will store all elements     

h <- c(1 + 1 == 3, "should be false", 2.22, 5)
h

class(h)

# You can play around with the combinations of these to see that the order is: logical < numeric < character.     

##############################
# Vector Subsetting 

# You can access an element of a vector using the subset "[]" function.     

i <- c(1,5,8,11,12)
i[1]

i[c(1,4)]

i[2:4]


##############################
# Vector Operations 

# You can perform operations on a vector - either on all elements of the vector, or as an aggregation     

i <- c(1,5,8,11,12)
j <- c(2,6,9,12,13)
i+j

i^2

##############################
# Vector Operations 

# You can perform operations on a vector - either on all elements of the vector, or as an aggregation     

i <- c(1,5,8,11,12)
mean(i)

sum(i)

##############################
# Data Frames 

# If you're using R for analysing data, you are likely to be making, loading, and manipulating data frames a lot. Data frames are objects that include columns (vectors) that can have different types, e.g.:     
  
df <- data.frame(colour=c("blue","red","yellow","purple"),
                 number=c(1,2,3,4),
                 iseven=((1:4)%%2 == 0))
df


##############################
# Data Frames 

# You can access an element of a data frame either in the same way that you would a matrix:     
  
df[2,3]

# or using the column names     

df[3,"number"]


##############################
# Functions 

# You can create your own functions in R     

exponentplusone <- function(a, b){
                            c <- a^(b+1);
                            return(c) }

exponentplusone(2,3)


##############################
# Conditionals 

# The "if" conditional can be used to check >, <, !=, == and other boolean operators (e.g. isnull())     

x <- 23
if (x%%2 == 0) {print("even")} else {print("odd")}


# If statements can also be nested:     
  
x <- "Does this string contain the word apple?"

if (grepl("apple", x))
  {print("Yes")} else
  if (grepl("orange", x))
    {print("No, but it contains the word orange")} else
    {print("No")}


##############################
# Loops 

# Both for loops and while loops are supported in R, with additional "looping" functions over vectors and data frames, (e.g. apply, sapply)     

for (i in 1:5) {print(i^2)}


##############################
# Loops 

# Both for loops and while loops are supported in R, with additional "looping" functions over vectors and data frames, (e.g. apply, sapply)     

x <- 0

while(x < 5)
  {x <- x+1;
  print(x^2)}


sapply(1:5, function(x) x^2)


##############################
# Kaggle 
##############################

##############################
# Problem - Bike Sharing Demand
# The competition we will focus on can be found here: 
# https://www.kaggle.com/c/bike-sharing-demand   
# This is an old (finished) competition, and is to predict the number of bicycles that will be rented per hour, based on information about the season, month, day, hour and weather.     

##############################
# Accessing Data 
  
# Step one is to retrieve the data. We download the training dataset from here: 
# https://www.kaggle.com/c/bike-sharing-demand/data, 
# and make sure that it is in our working directory.     

# The "train" dataset is what we will use to explore the data and build our models. The "test" dataset does not have the outcome variables, and is used for us to predict the outcome and then submit to kaggle.     

##############################
# Loading Data 

# To load a csv, R has an aptly named function read.csv.
# The following code allows you to directly load this file from my GitHub repository.
  
bikes <- read.csv("https://raw.githubusercontent.com/awhiter/TeachingRepos/master/RTasterSession/train.csv")

# Alternatively, download the train.csv file from https://www.kaggle.com/c/bike-sharing-demand/data, then use the following code, changing the path accordingly:
# bikes <- read.csv("/Users/andrewwhiter/Downloads/train.csv")

    
##############################
# Exploring Data 

# One of the first things to do when given a dataset is to understand what data is inside, this can be done by looking at the top few rows:     
  
head(bikes,4)


# Or by looking at the structure     

str(bikes)


##############################
# Exploring Data 

# To drill into a specific variable, we can also look at the summary:     
  
summary(bikes$temp)


##############################
# Dates? Factors? 

# We can see that our datetime variable has been loaded in as a "factor" variable. A "factor" is a categorical variable, where the order and value is not important (an example could be colour). 
# The order clearly does matter for dates so we will have to cast this into the right format.     

bikes$date <- as.Date(bikes$datetime)


library('lubridate')  
# lubricate provides a range of date functions - if needed: install.packages('lubridate')
bikes$hour <- hour(ymd_hms(bikes$datetime))

    
##############################
# Check 

head(data.frame(datetime=bikes$datetime, date=bikes$date, time=bikes$hour))


##############################
# Cleaning Data - Seasons 

# Lets have a look at the "season" variable by checking its unique values.     

unique(bikes$season)

# Looks like this is a categorical variable for the four seasons. 
     
##############################
# Cleaning Data - Season 

# Lets see if we can figure out which number corresponds to which season by doing a plot.
# The regular way of plotting in R is plot()     

plot(bikes$date, bikes$season)


##############################
# Cleaning Data - Season 

# To double check, lets figure out the possible seasons where the month is January     

unique(bikes$season[format.Date(bikes$date, "%m")=="01"])


##############################
# Cleaning Data - Season 

# create a new variable for season as a factor, and give it its meaningful name.     

bikes$seasonfactor <- as.factor(bikes$season)

levels(bikes$seasonfactor) <- c("Spring", "Summer", "Autumn", "Winter")


##############################
# Cleaning Data - Weather 

bikes$weatherfactor <- as.factor(bikes$weather)

# Note: while this example code has chosen to convert the weather number to a factor there is a counter argument to retain this as a number, as these weather numbers are ordered, with higher numbers representing progressively worse weather


##############################
# Cleaning Data 

# Lets look at what we now have in our "bikes" data frame object, after converting to factors and Date data types, and adding a new "hour" column

str(bikes)


##############################
# Plotting & ggplot 

# Many R users use the package "ggplot", you can get this by installing the package and loading it:     

# install.packages("ggplot2")    # this is only required to be done once for your laptop
library(ggplot2)               # this is required every time to load this into memory


##############################
# Exploring Data - Count 

# We are interested in how many people rent bikes in a certain hour, let's first look at the distribution of # of bikes rented per hour, as a histogram.     

ggplot(data=bikes, aes(x=count)) + geom_histogram()

# You will get a warning message while running this saying that the number of "bins" is being chosen automatically by R.
   

############################## 
# Exploring Data - Count 
  
# Because the histogram default y axis is count, the previous graph is slightly confusing, lets make it prettier.     

ggplot(data=bikes, aes(x=count)) +
  geom_histogram(binwidth = 20, fill="darkblue") +
  xlab("number of bikes rented per hour")
 

##############################
# Exploring Data - Hour 

# Lets look at the interaction between hour and number of bikes rented, hopefully we will be able to see a daily pattern.     

ggplot(data=bikes, aes(x=hour, y=count)) + geom_point()

##############################
# Exploring Data - Mean Count By Hour 

# The aggregate function is useful for claculating summarising statistics such as means  

meandata <- aggregate(bikes$count, list(bikes$hour, bikes$workingday),mean)
names(meandata)<-c("hour","workingday","mean.count")

ggplot(data=meandata, aes(x=hour, y=mean.count)) +
  geom_line(color='blue') + facet_grid(. ~ workingday) + ggtitle("Mean Count: Weekends(0) and Weekdays(1)")

##############################
# Exploring Data - Day of Week 
  
# Somewhat, but it might be better for us to also look at day of week:     
  
bikes$weekday <- wday(as.Date(bikes$date), label=TRUE)  

ggplot(data=bikes, aes(x=hour, y=count, color=weekday)) +
  geom_point(alpha=0.05, color="darkgray")+ 
  geom_smooth(fill=NA) +
  theme_light()
  
##############################
# Exploring Data - Day of Week 
  
# Lets take look also at the overall counts by number of days per week:     

ggplot(data=bikes, 
       aes(x="", y=count, color=weekday)) + 
        geom_boxplot(position=position_dodge(1))


##############################
# Exploring Data - Seasons 

# Lets check whether the seasons make a difference to this:     
  
ggplot(data=bikes, aes(x=hour, y=count, color=seasonfactor)) +
  geom_point(alpha=0.05, color="darkgray") +
  geom_smooth(fill=NA) +
  facet_grid(. ~ workingday) + theme_light()

  
############################## 
# Exploring Data - Weather 
  
# Lets check whether the weather makes a difference to this (note weather = 4 has been auto-removed as there is only 1 point):     

ggplot(data=bikes, aes(x=hour, y=count,color=weatherfactor)) +
  geom_point(alpha=0.05, color="darkgray") +
  geom_smooth(fill=NA) +
  facet_grid(. ~ workingday) + theme_light()


##############################
# Exploring Data - Temperature </hgroup>

# R can also include a colour scale for continuous variables:     

ggplot(data=bikes, aes(x=hour, y=count, color=temp)) +
  geom_point(alpha=0.6) +
  facet_grid( ~ workingday) + theme_light()


##############################
# Exploring Data - Temperature 

# A couple of good ideas are to include a temperature colour scale, and add some "jitter" to the x axis.     

ggplot(data=bikes, aes(x=hour, y=count, color=temp)) +
  geom_point(alpha=0.6, size=0.75, position=position_jitter(w=1,h=0)) +
  scale_colour_gradientn("Temp (°F)", colours=c("#5e4fa2", "#3288bd","#66c2a5", "#abdda4","#e6f598", "#fee08b","#fdae61", "#f46d43","#d53e4f", "#9e0142")) +
  facet_wrap( ~ workingday, ncol=1) + theme_light()


##############################
# Exploring Data - Temperature 

# Lets visually look at a basic assumption that higher temperature causes higher numbers of bikes to be rented:     
  
ggplot(data=bikes, aes(x=temp, y=count, color=as.factor(workingday))) +
  geom_point(alpha=0.6, color="gray") +
  geom_smooth() +
  guides(color=guide_legend(title = "workingday"))

  
############################## 
# Exploring Data - Year 
  
# So far we have been looking at the data over both years (2011 and 2012), but there may have been a different level for each of those years, let's have a look:     

bikes$year <- as.factor(year(ymd_hms(bikes$datetime)))

ggplot(data=bikes, aes(x=hour, y=count, color=year)) + geom_point(alpha=0.05, color="gray") + geom_smooth(fill=NA) + facet_grid(. ~ workingday) + theme_light()



##############################
# Modeling and prediction 
##############################

##############################
# Test and training sets 

# Seeing as kaggle does not provide us with the target variables of the test set, we will only be able to know the predictive accuracy once we submit our results to kaggle. So it may be a useful idea for us to create our own test set within our data, so that we can internally test using that.     

# install.packages("caret")
library(caret)

set.seed(1)
intrain <- createDataPartition(y = bikes$count, p = 0.9, list = FALSE)

bikestrain <- bikes[intrain, ]
bikestest <- bikes[-intrain, ]


##############################
# Simple Linear Modelling 

# To start simply, let's build a model that tries to explain the number of people renting a bike based only on temperature.     

linearmodel.temperatureonly <- lm(count ~ temp, bikestrain)

summary(linearmodel.temperatureonly)


##############################
# Visually 
  
ggplot(data=bikestrain, aes(x=temp, y=count)) +
  geom_point(alpha=0.6, color="gray") + theme_light() +
  geom_line(data=data.frame(temp=bikestrain$temp,
                            count=predict(linearmodel.temperatureonly,bikestrain)))


############################## 
# Goodness of fit / Accuracy 

# The way that kaggle, for this competition, assessed accuracy is by using the **Root Mean Squared Logarithmic Error** of the predictions. This penalises predictions that are too low more heavily than those that are too high.    

rmsle <- function(testvalues,predvalues)
  {sqrt(1/length(testvalues)*
        sum((log(predvalues +1)-log(testvalues +1))^2))}

# Lets calculate the RMSLE of our model on our internal test set:     
  
testvalues <- bikestest$count

predvalues <- predict(linearmodel.temperatureonly,bikestest)

rmsle(testvalues,predvalues)


##############################
# More complex linear modelling 

linearmodel.full <- lm(count ~ temp + I(temp^2) +
                         workingday + holiday +
                         windspeed + weatherfactor +
                         seasonfactor + hour:temp +
                         workingday:temp + hour:year
                       + temp:year + year:workingday
                       + weather:hour + hour:workingday,
                       bikestrain)

summary(linearmodel.full)    


##############################
# Accuracy on our test set 

# Lets calculate the RMSLE of our model on our internal test set:     

testvalues <- bikestest$count

predvalues <- predict(linearmodel.full,bikestest)

predvalues[predvalues<=0] <- 0

rmsle(testvalues,predvalues)

##############################
# Classification Trees 

# Lets look at a simple classification tree for our target outcome:     
  
library(rpart)
library(rpart.plot)

treemodel.basic = rpart(count ~ temp + hour, method="anova", data=bikestrain)

prp(treemodel.basic)


##############################
# Classification Trees 
  
# We can make classification trees more complicated by adding in further factors:     
  
treemodel.full = rpart(count ~ temp + I(temp^2) +
                         hour + year +
                         workingday + holiday +
                         windspeed + weatherfactor +
                         seasonfactor, method="anova",
                       data=bikestrain)

prp(treemodel.full)

  
##############################
# Classification Trees 
  
# The rpart package calculates splits that reduce the variance within groups, but sometimes do not include all of the variables, e.g.:     
  
printcp(treemodel.full)


##############################
# Accuracy on our test set 

# Let's calculate the RMSLE of our model on our internal test set:     

testvalues <- bikestest$count
predvalues <- predict(treemodel.full,bikestest)
predvalues[predvalues<=0] <- 0

rmsle(testvalues,predvalues)

##############################
# Random Forest 

# Classification trees can be useful on their own, usually for categorical variables, but often they are ensembled into "random forests" to improve performance.     

# install.packages("randomForest")
library(randomForest)

randomforestmodel <- randomForest(count ~ ., bikestrain[,c(2:9,12:14,18)], ntree=100)

# Removed casual and registered (columns 10 and 11), and also removed the factor variables. Some caution on the choice of ntree, larger ntree are likely to increase accuracy, but at the cost of algorithm running time.     


##############################
# Accuracy on our test set 

# Lets calculate the RMSLE of our model on our internal test set:     
  
testvalues <- bikestest$count

predvalues <- predict(randomforestmodel,bikestest)

predvalues[predvalues<=0] <- 0

rmsle(testvalues,predvalues)


     
  



