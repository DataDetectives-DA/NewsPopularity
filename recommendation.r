library(dplyr)
data<- read.csv('OnlineNewsPopularity.csv',header =TRUE)
data <- subset( data, select = -c(url, timedelta, is_weekend ) )
data=data[!data$n_unique_tokens==701,]
data$shares<-as.numeric(data$shares)
q<-quantile(data$shares,0.75)
data$popular<-ifelse(data$shares>=q,1,0)

#set.seed(40) 
train<-sample_frac(data,0.8)
test<-setdiff(data,train)

distance<-0

recommend<-function(check,train_popular,k)
{ train_popular$dist<-0
for (i in 1:nrow(train_popular))
{
  for(j in 1:(ncol(train_popular)-2))  #excluding popular and dist column
  {
    distance<-distance + (train_popular[i,j]-check[,j])^2   #calculating euclidean distance
  }
  sqrtdist<-sqrt(distance)
  train_popular$dist[i]<-sqrtdist   #appending the distance  btw train and test sample data point
}
train_popular_sorted<-train_popular[order(train_popular$dist),]
train_popular_sorted_k<-train_popular_sorted[1:k, 1:(ncol(train_popular)-2)]
#print(train_popular_sorted_k)
centroid<-colMeans(train_popular_sorted_k)
print("Recommended following changes to make the article popular:\n")
print(colMeans(train_popular_sorted_k) ) #printing recommendations
return (centroid)
}

#taking one test sample which is not popular and  recommending the changes one need in that test sample to become popular
trying<-subset(test,popular==0)
testsample<-sample_n(trying,1)
populartrain<-subset(train,train$popular==1)

#calling recommendation function which tells what changes needed to be made in the given article to get the number of shares that makes it popularticle to get shares that makes it popular
#recommendation function takes arguments (test data point,trained data,k neigbours that need to looked to)
recommended_changes<-recommend(testsample,populartrain,2) 

