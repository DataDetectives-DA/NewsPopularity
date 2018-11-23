# load required dataset
library(dplyr)
News <- read.csv("OnlineNewsPopularity.csv")
########################################
News$news_day <- rep("Sunday", nrow(News))
News$news_day[News$weekday_is_monday==1] <- "Monday"
News$news_day[News$weekday_is_tuesday==1] <- "Tuesday"
News$news_day[News$weekday_is_wednesday==1] <- "Wednesday"
News$news_day[News$weekday_is_thursday==1] <- "Thursday"
News$news_day[News$weekday_is_friday==1] <- "Friday"
News$news_day[News$weekday_is_saturday==1] <- "Saturday"

days<-aggregate(News$shares, by=list(Day=News$news_day), FUN=sum)


barplot(days$x, main="Days Distribution of shares",ylab="total no of shares",xlab="Day",col="blue",names=days$Day)
#######################################
News$channel <- rep("Lifestyle", nrow(News))
News$channel[News$data_channel_is_entertainment==1] <- "Entertainment"
News$channel[News$data_channel_is_bus==1] <- "Bussiness"
News$channel[News$data_channel_is_socmed==1] <- "Social Media"
News$channel[News$data_channel_is_tech==1] <- "Technology"
News$channel[News$data_channel_is_world==1] <- "World"

shares<-aggregate(News$shares, by=list(Category=News$channel), FUN=sum)

barplot(shares$x, main="Category Distribution of shares",ylab="total no of shares",xlab="category",col="blue",names=shares$Category)

########################################
q<-quantile(News$shares,0.75)
popular <-ifelse(News$shares<=q, 0, 1)
News <-data.frame(News, popular)

News_sub<-subset(News,popular==1)

title<-aggregate(News_sub$popular, by=list(n_token_title=News_sub$n_tokens_title), FUN=sum)


content<-aggregate(News_sub$popular, by=list(n_tokens_content=News_sub$n_tokens_content), FUN=sum)

keywords<-aggregate(News_sub$popular, by=list(num_keywords=News_sub$num_keywords), FUN=sum)

hist(News_sub$n_tokens_title, breaks=9, main="Number of title words vs popularity level",xlab="words in title",ylab="popularity",col="blue")

hist(News_sub$n_tokens_content, breaks=20, main="Number of content words vs popularity level",xlab="words in content",ylab="popularity",col="blue")

hist(News_sub$num_keywords, breaks=10,main="Number of keyword words vs popularity level",xlab="no of keywords",ylab="popularity",col="blue")
