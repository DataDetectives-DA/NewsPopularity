#importing required libraries
library(party)
require(randomForest)

# Reading data to a dataframe :

News <- read.csv('A:/Da_proj/OnlineNewsPopularity/OnlineNewsPopularity.csv', header = TRUE)

#removing outlier
News=News[!News$n_unique_tokens==701,]

#removing non numeric attributes
News <- subset( News, select = -c(url, timedelta, is_weekend ) )

#classifiying if popular or not using threshold = 3rd quantile
q<-quantile(News$shares,0.75)
popular = ifelse(News$shares<=q, 0, 1)
#0 signifies not popular and 1 signifies popular
News = data.frame(News, popular)


#spliting data into train and test
n_random<-round(0.75*nrow(News))
index<-sample(1:nrow(News),n_random)
train<-News[index,]
test<-News[-index,]

# Create the forest.
output.forest <- randomForest(shares ~ ., data = train,ntree=100)

# View the forest results.
print(output.forest) 

#predicting 
random_forest_pred<-predict(output.forest,test)

#creating confusion matrix
confusion_matrix<-table(random_forest_pred, test$popular)

#calculating miss classification rate
miss_class<-1-(sum(diag(confusion_matrix))/sum(confusion_matrix))
print(miss_class) 
