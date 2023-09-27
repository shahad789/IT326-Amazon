#The getwd() function tells you what the current working directory is
getwd()

#setwd function to change the directory you work on 
setwd("/Users/sumayah")

################## Importing the dataset ##################
#read file named "Data.csv" and save it in dataframe named "dataset"
dataset = read.csv('Data.csv')

View(dataset)
str(dataset)

################# Dealing with the missing values - Outliers ##################
#Checking NULL, FALSE means no null, TRUE cells means the value of the cell is null
is.na(dataset)

# to find the total null values in the dataset
sum(is.na(dataset))

#Remove rows with missing values
#dataset = na.omit(dataset)
#View(dataset)

#Replace missing Data with average value
dataset$Age = ifelse(is.na(dataset$Age), ave(dataset$Age, FUN =function(x) mean(x,na.rm=TRUE)), dataset$Age)
dataset$Salary = ifelse(is.na(dataset$Salary), ave(dataset$Salary, FUN=function(x) mean(x,na.rm= TRUE)), dataset$Salary)
#Usage:
#ifelse(test, yes, no)
#dataset$column_header: Selects the column in the dataset specified after $ (age and salary).
#is.na(dataset$column_header): This method returns true for all the cells in the specified column with no values.
#ave(dataset$column_header, FUN = function(x) mean(x, na.rm = ‘TRUE’)): Ths method calculates the average of the column passed as argument.

print(dataset)

###Detect Outlier
#Outlier: Find value with largest difference from the mean
#logical if set to TRUE, gives vector of logical values, and possible outlier position is marked by TRUE

#install.packages("outliers")
library(outliers)

OutAge = outlier(dataset$Age, logical =TRUE)
sum(OutAge)
Find_outlier = which(OutAge ==TRUE, arr.ind = TRUE)
OutAge
Find_outlier

#Remove outlier
dataset= dataset[-Find_outlier,]
#Second row is removed now
print(dataset)

################# Data Conversion ##################
###Encoding categorical data
dataset$Country = factor(dataset$Country,levels = c("France","Spain", "Germany"), labels = c(1, 2, 3))
dataset$Purchased = factor(dataset$Purchased, levels = c("No", "Yes"), labels = c(0, 1))
#Usage:
#factor(dataset$olumn_header, levels = c(), labels = c()) : the factor method converts the categorical features in the specified column to factors or numerical codes.
#levels: the categories in the column passed as a vector. Example c(‘India’,’Germany’,’Russia’)
#labels: The numerical codes for the specified categories in the same order. Example c(1,2,3))
print(dataset)

dataWithoutNormalization <- dataset
print(dataWithoutNormalization)


################ Feature Scaling / Normalization ##############
#we can apply the feature scaling to only non-categorical data in R
#scale, with default settings, will calculate the mean and standard deviation of the entire vector, then "scale" each element by those values by subtracting the mean and dividing by the sd
dataset [, 2:3] = scale(dataset [, 2:3])
print(dataset)

#Define function normalize().
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
#Define function Z_normalize().
Z_normalize <- function(x) {return ((x - mean(x)) / sd(x))}

#Call normalize funcrtion 
dataset$Age<-normalize(dataWithoutNormalization$Age)
print(dataset)

#Call Z_normalize funcrtion 
dataset$Age<-Z_normalize(dataWithoutNormalization$Age)
print(dataset)



