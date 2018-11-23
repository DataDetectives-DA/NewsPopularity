
Problem Statement\n:
1.Prediction of the popularity of the news article published online
2.Recommending what changes to make in the article to become popular.

Dataset:
https://archive.ics.uci.edu/ml/datasets/online+news+popularity
This dataset summarizes a heterogeneous set of features about articles published by Mashable in a period of two years
It has 39797 instances and 61 attributes.
Nominal features were transformed with one hot encoding.
3 types of keywords-worst,average and best
NLP features-LDA(latent Dirichlet allocation),Title Subjectivity,Title Sentiment Polarity,Rate of positive and Negative words.

Data Preprocessing and Exploratory Data Analysis

Removed Outlier and Checked for NA values.
Plotted Graphs to visualize the relation between variables.
PCA Analysis was done. But, we didn’t find it useful.
Stepwise feature selection was done which gave better results.
We used log transformation on the data to make it normally distributed.
This proves why this works better as a classification model as classification involves the log transform.

MODELS USED
We used different models to train our dataset and predict results according to them. 
Linear Regression
Logistic Regression
Random Forest
Support Vector Machine(SVM)
Decision Trees
Gradient Boosting Method
We considered the news with shares more than 3rd quartile as popular.


Recommendation System

Calculating the euclidean distance from the test data point to the train data point.
Calculating the centroid of K nearest neighbours to the test data point and suggesting the changes for that data point.






