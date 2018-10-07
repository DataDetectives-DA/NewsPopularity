# Reading data to a dataframe :

News <- read.csv('A:/Da_proj/OnlineNewsPopularity/OnlineNewsPopularity.csv', header = TRUE)

# Summary and Structure of data before preprocessing:

summary(News)
str(News)

# Exploratory Data Analysis and Cleaning Data:

#if the data has any missing values:

sum(is.na(News))

#outliers in var:"n_unique_tokens", "n_non_stop_words", and "n_non_stop_unique_tokens"
# These values seem unusual from the rest values, hence removing them

News=News[!News$n_unique_tokens==701,]

# url, timedelta - being non-predictive variables,removing them
# also is_weekend seems to be repetitive with other weekday data, so removing it.

News <- subset( News, select = -c(url, timedelta, is_weekend ) )

data<-News[,c(30,31,32,33,34,35,36)]
News$weekday<-toupper(names(data)[max.col(data)])
