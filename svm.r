#importing required libraries
library(caret)
library(scales)
library(e1071)

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

#SVM linear
svmLinearModel = svm(popular~.-shares,data = train,type="C-classification",kernel="linear")
svm_linear_pred = predict(svmLinearModel,test)

#creating confusion matrix
confusion_matrix_lin<-table(test$popular,svm_linear_pred)

#calculating miss classification rate
miss_class_lin<-1-(sum(diag(confusion_matrix_lin))/sum(confusion_matrix_lin))
print(miss_class_lin)


#SVM polynomial
svmPolyModel = svm(popular~.-shares,data = train,type="C-classification",kernel="polynomial")
svm_poly_pred = predict(svmPolyModel,test)

#creating confusion matrix
confusion_matrix_poly<-table(test$popular,svm_poly_pred)

#calculating miss classification rate
miss_class_poly<-1-(sum(diag(confusion_matrix_poly))/sum(confusion_matrix_poly))
print(miss_class_poly)

#SVM radial
svmRadialModel = svm(popular~.-shares,data = train,type="C-classification",kernel="radial")
svm_radial_pred = predict(svmRadialModel,test)

#creating confusion matrix
confusion_matrix_rad<-table(test$popular,svm_radial_pred)

#calculating miss classification rate
miss_class_rad<-1-(sum(diag(confusion_matrix_rad))/sum(confusion_matrix_rad))
print(miss_class_rad)
