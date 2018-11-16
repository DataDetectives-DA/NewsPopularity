# load required libraries
library(caret)
library(corrplot)
library(plyr)

# load required dataset
dat <- read.csv("OnlineNewsPopularity.csv")

# Set seed
set.seed(227)

# Remove variables having high missing percentage (50%)
dat1 <- dat[, colMeans(is.na(dat)) <= .5]
dim(dat1)

# Remove Zero and Near Zero-Variance Predictors
nzv <- nearZeroVar(dat1)
dat2 <- dat1[, -nzv]
dim(dat2)

# Identifying numeric variables
numericData <- dat2[sapply(dat2, is.numeric)]

# Calculate correlation matrix
descrCor <- cor(numericData)

# Print correlation matrix and look at max correlation
print(descrCor)
summary(descrCor[upper.tri(descrCor)])

# find attributes that are highly corrected
highlyCorrelated <- findCorrelation(descrCor, cutoff=0.7)

# print indexes of highly correlated attributes
print(highlyCorrelated)

# Indentifying Variable Names of Highly Correlated Variables
highlyCorCol <- colnames(numericData)[highlyCorrelated]

# Print highly correlated attributes
highlyCorCol

# Remove highly correlated variables and create a new dataset
dat3 <- numericData[, -which(colnames(dat2) %in% highlyCorCol)]
dim(dat3)

#Classifing as popular or not based on thershold = 3rd quantile of shares
#dat3['popular']<-0

q<-quantile(dat3$shares,0.75)

#for (i in 1:nrow(dat3))
#{
  #if(dat3$shares[i]>=q)
  #{
  #  dat3$popular[i]=1
 # }
#}


require(tree)
popular = ifelse(dat3$shares<=q, "No", "Yes")

dat4 = data.frame(dat3, popular)
#dat4<-dat4[,-1]

#spliting data into train and test
n_random<-round(0.7*nrow(dat4))
index<-sample(1:nrow(dat4),n_random)
train<-dat4[index,]
test<-dat4[-index,]

#creating model
tree.train = tree(popular~.-shares, data=train)

#summary of the model
summary(tree.train)

#prediction on test dataset
tree.pred = predict(tree.train, test, type="class")

#creating confusion matrix
confusion_matrix<-table(tree.pred, test$popular)

#calculating miss classification rate
miss_class<-1-(sum(diag(confusion_matrix))/sum(confusion_matrix))
print(miss_class)



