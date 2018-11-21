library(dplyr)
data<- read.csv('~/Sem 5/DA/project/OnlineNewsPopularity/OnlineNewsPopularity.csv',header =TRUE)
data<-data[-1]
data$shares<-as.numeric(data$shares)
data$popular<-ifelse(data$shares>=1400,1,0)

set.seed(40) 
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
  train_popular_sorted_k<-train_popular_sorted[1:k,]
  centroid<-colMeans(train_popular_sorted_k)
  return (centroid)
}

#taking one test sample which is not popular and  recommending the changes one need in that test sample to become popular
trying<-subset(test,test$popular==0)
check<-sample_n(trying,1)
testsample<-trying[1,]
populartrain<-subset(train,train$popular==1)
recommended_changes<-recommend(testsample,populartrain,2)
cat("Recommended changes are ",recommended_changes)
