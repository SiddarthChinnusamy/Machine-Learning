#Set working directory
setwd("D:/Walnut/Remarks File/Remarks_score")

#Loading necessary libraries
library(ggplot2)
library(Amelia)
library(tidytext)
require(openxlsx)
library(dplyr)

#Input data
data<-read.xlsx("Remarks_consolidated_V1.xlsx")
data1<-data[!is.na(data$Critical.Thinking.Post), ]

#convert text to tidy data
tidy_data<-data1 %>%
  unnest_tokens(word,Remarks)
head(tidy_data)

data_sentiment<-tidy_data%>%
  inner_join(get_sentiments("bing"))
head(data_sentiment)

f<-data_sentiment%>%
  count(School.Name,Facilitator,index=CT.Post.Grade,sentiment)%>%
  spread(sentiment,n,fill=0)%>%
  mutate(net=positive-negative)
ggplot(f,aes(index,net))+geom_col(fill="skyblue1")
ggplot(f,aes(index,net))+geom_col(fill="skyblue1")+facet_wrap(~Facilitator)


j<-data_sentiment%>%
  count(Facilitator,index=CT.Post.Grade,sentiment)%>%
  group_by(Facilitator,index)%>%
  mutate(total=sum(n),percent=n/sum(n))
ggplot(j,aes(index,percent,fill=sentiment))+geom_col()+facet_wrap(~Facilitator)

