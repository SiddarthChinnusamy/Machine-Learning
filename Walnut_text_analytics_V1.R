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
data1

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
  j
ggplot(j,aes(index,percent,fill=sentiment))+geom_col()+facet_wrap(~Facilitator)

g<-data_sentiment%>%
  count(School.Name,Facilitator,index=X15,sentiment)%>%
  spread(sentiment,n,fill=0)%>%
  mutate(net=positive/negative)

ggplot(g,aes(index,net))+geom_col(fill="wheat4")
ggplot(g,aes(index,net))+geom_col(fill="wheat4")+facet_wrap(~Facilitator)



h<-data_sentiment%>%
  count(School.Name,Facilitator,index=X15,sentiment)%>%
  group_by(index)%>%
  spread(sentiment,n)
#h
mutate(negper= (negative*100/(negative+positive)),posper=(positive*100/(negative+positive)))
h

h[is.na(h)]<-0
i<-h%>%
mutate(negper= (negative*100/(negative+positive)),posper=(positive*100/(negative+positive)))
i
i<-gather(i,senti,value,6:7)
i
ggplot(,aes(index,))+geom_col(fill="wheat4")
ggplot(h,aes(index,percent))+geom_col(fill="wheat4")+facet_wrap(~Facilitator)

