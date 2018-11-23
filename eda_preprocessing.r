# Reading data to a dataframe :

News <- read.csv('OnlineNewsPopularity.csv', header = TRUE)

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
#plotting histograms to get an idea of distributions

par(mfrow=c(3,4))
for(i in 1:12){hist(News[,i], xlab=names(News)[i],main="Histogram")}

par(mfrow=c(3,4))
for(i in 13:24){hist(News[,i], xlab=names(News)[i],main="Histogram")}

par(mfrow=c(3,4)) 
for(i in 25:36){hist(News[,i], xlab=names(News)[i],main="Histogram")}

par(mfrow=c(3,4))
for(i in 37:48){hist(News[,i], xlab=names(News)[i],main="Histogram")}

par(mfrow=c(3,4))
for(i in 49:58){hist(News[,i], xlab=names(News)[i],main="Histogram")}

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
data$whichday<-as.factor(toupper(names(data)[max.col(data)]))
counts <- table(data$whichday)

#plotting bar graph
barplot(counts, main="comparison of article been published in which day of the week",xlab="day",col=rgb(0.3,0.1,0.4,0.6),las=2)

#From the above bargraph we can come to a conclusion that a lot of articles are been published during weekdays and not during weekends. 
#selecting the cloumns of which channel like enternaiment or sports or so on columns

data1<-News[,c(12,13,14,15,16,17)]
data1$whichchannel<-as.factor(toupper(names(data)[max.col(data)]))
counts1<-table(data1$whichchannel)

#plotting bar graph
barplot(counts1, main="comparison of article been published for various category of channels",xlab="day",col=rgb(0.3,0.1,0.4,0.6),las=2)

#when we observe the above graph a lot of articles are published under the category of channel being world,technology,and entertainment

#We are performing log of shares because log is a better transformation to conclude popularity as the data is very widespread so to concentrate it we take log

#checking relationship b/w log of shares and weekday on which it was published
news_day <- rep("Sunday", nrow(News))
news_day[News$weekday_is_monday==1] <- "Monday"
news_day[News$weekday_is_tuesday==1] <- "Tuesday"
news_day[News$weekday_is_wednesday==1] <- "Wednesday"
news_day[News$weekday_is_thursday==1] <- "Thursday"
news_day[News$weekday_is_friday==1] <- "Friday"
news_day[News$weekday_is_saturday==1] <- "Saturday"
#Check 
p1 <- ggplot(data=News, aes(as.factor(news_day), log(News$shares)))
p1 + geom_boxplot()

# no such relationship was found

#checking relationship b/w log of shares and data channel on which it was published
channel <- rep("Lifestyle", nrow(News))
channel[News$data_channel_is_entertainment==1] <- "Entertainment"
channel[News$data_channel_is_bus==1] <- "Bussiness"
channel[News$data_channel_is_socmed==1] <- "Social Media"
channel[News$data_channel_is_tech==1] <- "Technology"
channel[News$data_channel_is_world==1] <- "World"
#Check 
p1 <- ggplot(data=News, aes(as.factor(channel), log(News$shares)))
p1 + geom_boxplot()
#no such relationship was found though it was found that there is some difference when considering weekend or not.

# The following variables are categorical with 2 values : 0 and 1 but are numeric; hence, converted all such variables to factor variables with 2 levels.
News$weekday_is_monday <- factor(News$weekday_is_monday) 
News$weekday_is_wednesday <- factor(News$weekday_is_wednesday) 
News$weekday_is_thursday <- factor(News$weekday_is_thursday) 
News$weekday_is_friday <- factor(News$weekday_is_friday) 
News$weekday_is_tuesday <- factor(News$weekday_is_tuesday) 
News$weekday_is_saturday <- factor(News$weekday_is_saturday) 
News$weekday_is_sunday <- factor(News$weekday_is_sunday) 

News$data_channel_is_lifestyle <- factor(News$data_channel_is_lifestyle) 
News$data_channel_is_entertainment <- factor(News$data_channel_is_entertainment) 
News$data_channel_is_bus <- factor(News$data_channel_is_bus) 
News$data_channel_is_socmed <- factor(News$data_channel_is_socmed) 
News$data_channel_is_tech <- factor(News$data_channel_is_tech) 
News$data_channel_is_world <- factor(News$data_channel_is_world)
--------------------------------------------------------------------------------------------------------------------------------------
#correlation matrix
News$shares=log(News$shares)
library(corrplot)
cor_mat<-cor(News)
corrplot(cor_mat, type = "upper", order = "hclust",tl.srt = 45,tl.col = "black")
#no such correlation with log shares was found

#PCA ANANLYSIS
library(factoextra)

#performing PCA Analysis on News dataframe except the target variable
PCA_Analysis <- prcomp(News[-58],scale=TRUE,center=TRUE)

#getting all the standard deviation which is in descending order
PCA_stdev<-PCA_Analysis$sdev

#getting the respective variances
PCA_var<-PCA_stdev^2

#proportion of variance explained for each component
prop_var <- PCA_var/sum(PCA_var)
sum<-0

#percentage of variance for 42 principal components which accounts for 98% of variance
for(i in 1:42){
  sum<-sum+prop_var[i]
}

#scree plot to access components or factors which explains the most of variability in the data
#this shows that 42 principal components seem to account for 98% of variance so we can conclude that we have reduced 57 datapoints to 42 datapoints 
plot(prop_var, xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b")

#cumulative scree plot
plot(cumsum(prop_var), xlab = "Principal Component",ylab = "Cumulative Proportion of Variance Explained",type = "b")
