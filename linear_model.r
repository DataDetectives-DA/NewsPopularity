#LINEAR MODEL
# Reading data to a dataframe :

News <- read.csv('OnlineNewsPopularity.csv', header = TRUE)

News=News[!News$n_unique_tokens==701,]

News <- subset( News, select = -c(url, timedelta, is_weekend ) )

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


# Sampling the dataset into 75% : training data and 25% : test data:

set.seed(174004689)

NewsTrain <- sample(nrow(News),as.integer(nrow(News)*0.75))
train.news = News[NewsTrain,]
test.news = News[-NewsTrain,]

# Now, we fit a model with all the variables; shares being the dependent variable and all other explanatory variables from the dataset as the predictors.

fit_1 <- lm(shares ~ ., data = train.news)

summary(fit_1)

pred_1 = predict(fit_1, test.news)

sqrt(mean((test.news$shares - pred_1)^2))

# This model has a low R-square value, we can use a transformation on the model.

# Now, I try using a log-transformation on our target variable to optimise our fit model:

News$shares <- log(News$shares)

NewsTrain <- sample(nrow(News),as.integer(nrow(News)*0.75))
train.news = News[NewsTrain,]
test.news = News[-NewsTrain,]

# Now, I try fitting a model on the transformed target variable:

fit_2 <- lm(shares ~ ., data = train.news)

summary(fit_2)

# This model gives a better R-square value than the previous model.

# Now, I want to include only statistically significant variables in the model. So, I use the Stepwise regression step():

fit_step_reg <- step(fit_2)

summary(fit_step_reg)
#this is giving us 41 relevant variables and we also see the pca was also giving us almost the same results.

pred_2 = predict(fit_step_reg, test.news)

sqrt(mean((test.news$shares - pred_2)^2))

# We thus get an optimized model with R-square value of approximately 0.13 and Root mean square error of approximately 0.87. This model includes only the statiscal variables.

#Now,predicting whether the news is popular or not based on 3rd quantile value of log(shares)
q<-quantile(News$shares,0.75)

#for testing data
popular = ifelse(test.news$shares<=q, "Not Popular", "Popular")
test.news = data.frame(test.news, popular)

#for predicted data
popular_pred=ifelse(pred_2<=q, "Not Popular", "Popular")

#creating confusion matrix
confusion_matrix<-table(popular_pred, test.news$popular)

#calculating miss classification rate
miss_class<-1-(sum(diag(confusion_matrix))/sum(confusion_matrix))
print(miss_class)

 #misclassification ratecomes out to be 0.25 which is decent
