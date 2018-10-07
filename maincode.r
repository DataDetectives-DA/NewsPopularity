# Reading data to a dataframe :

News <- read.csv('A:/Da_proj/OnlineNewsPopularity/OnlineNewsPopularity.csv', header = TRUE)

# Summary and Structure of data before preprocessing:
#------------------------------PREPROCESSING-----------------------------------------------------------------
summary(News)
str(News)
# Exploratory Data Analysis and Cleaning Data:

#if the data has any missing values:

sum(is.na(News))
#if 0 no missing values  ,this dataset has no missing values as NA are zero 

#Checking for outliers if any...

# after looking at the summary statistics of the data  and by plotting boxplot we observed that the columns "n_unique_tokens", "n_non_stop_words", and "n_non_stop_unique_tokens" 
#has one very big outlier
boxplot(News$n_unique_tokens, xlab = "number of unique tokens",ylab="number") 
# These values seem unusual from the rest values
News=News[!News$n_unique_tokens==701,] #this removes the outiers as the outlier is in the same row
boxplot(News$n_unique_tokens, xlab = "number of unique tokens",ylab="number") .#this boxplot doesnt have those unusual datapoints.

# columns like url, timedelta - being non-predictive variables,removing them.
# also column  "is_weekend" seems to be repetitive with other weekday data, so removing it as it is redundant.

News <- subset( News, select = -c(url, timedelta, is_weekend ) )

#News is  a datset with pre-proceesed datapoints.
#---------------------------------------------------------------------------------------------------------
#selecting the  column which tells which day is the article been published .these columns ae one hot encoded 
data<-News[,c(30,31,32,33,34,35,36)]
whichday<-toupper(names(data)[max.col(data)])
#selecting the cloumns of which channel like enternaiment or sports or so on columns
data<-News[,c(12,13,14,15,16,17)]
whichchannel<-toupper(names(data)[max.col(data)])
no_shares<-News[,c(58)]

