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
data$whichday<-as.factor(toupper(names(data)[max.col(data)]))
counts <- table(data$whichday)
#plotting bar graph
barplot(counts, main="comparison of article been published in which day of the week",xlab="day",col=rgb(0.3,0.1,0.4,0.6),las=2)
#From the above bargraph we can come to a conclusion that a lot of articles are been published during weekdays and not during weekends. 
#selecting the cloumns of which channel like enternaiment or sports or so on columns
data1<-News[,c(12,13,14,15,16,17)]
data1$whichchannel<-as.factor(toupper(names(data)[max.col(data)]))
counts1<-table(data1$whichchannel)
barplot(counts1, main="comparison of article been published for various category of channels",xlab="day",col=rgb(0.3,0.1,0.4,0.6),las=2)
#when we observe the above graph a lot of articles are published under the category of channel being world,technology,and entertainment
no_shares<-News[,c(58)]


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

#-----------------------------------------MODELS -----------------------------------------------
set.seed(20)
train_data_index<-sample(1:nrow(News),0.8*(nrow(News)),replace=FALSE);
test_data_index<-setdiff(1:nrow(News),train_data_index);
train_data<-News[train_data_index,]
test_data<-News[test_data_index,]
model1<- lm(formula=shares~.,data=train_data)
predmodel1<-predict(model1,test_data)
errormodel1<-test_data$shares - predmodel1 
rmse_usingmodel1<-sqrt(mean((errormodel1)^2)) 
