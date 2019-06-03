#Set working directory
setwd("D:/Walnut/Remarks File/Consolidated files")

#Loading necessary libraries
library(ggplot2)
library(Amelia)
library(tidytext)
require(openxlsx)
library(dplyr)

#Input data
f <- list.files()
temp<-data.frame()
data<-data.frame()

for (i in 1:length(f))
  {
    print(i)
    print(f[i])
    temp<-read.xlsx(f[i])
    data<-rbind(data,temp)
  }

names(data)[6]<-"Remarks"
names(data)[5]<-"Facilitator"

data<-data[-c(431), ]

#data$Range[i] <- ifelse(data$Marks < 10, 'g1',
 #                    ifelse(data$Marks >=10 & data$Marks < 20, 'g2',
  #                   ifelse(data$Marks >=20 & data$Makrs < 30, 'g3',
   #                  ifelse(data$Marks >=30 & data$Makrs < 40, 'g4',
    #                 ifelse(data$Marks >=40 & data$Makrs < 50, 'g5',
     #                ifelse(data$Marks >=50 & data$Makrs < 60, 'g6',
      #               ifelse(data$Marks >=60 & data$Makrs < 70, 'g7',
       #              ifelse(data$Marks >=70 & data$Makrs < 80, 'g8',
        #             ifelse(data$Marks >=80 & data$Makrs < 90, 'g9','g10')))))))))


#Changing character data types to factor
data$Standard.name<-as.factor(data$Standard.name)
data$School.Name<-as.factor(data$School.Name)
data$Age<-as.numeric(data$Age)

#Handling Missing values
missmap(data)

#Text analytics
#Converting sentences into words (Tokenising)
tidy_data<-data %>%
  unnest_tokens(word,Remarks)
head(tidy_data)

#Analysing the text using bing lexicon
#Assigning Postive/negative emotions to words
data_sentiment<-tidy_data%>%
  inner_join(get_sentiments("bing"))
head(data_sentiment)

#Percentage of positive and negative words used in remarks - Schoolwise  
bing_sentiment_counts_1<-data_sentiment %>%
  group_by(School.Name)%>%
  count(sentiment)%>%
  mutate(total=sum(n),percent=n*100/total)
head(bing_sentiment_counts_1)
ggplot(bing_sentiment_counts_1,aes(School.Name,percent,fill=sentiment))+geom_bar(stat="Identity")+scale_x_discrete(labels = abbreviate)

#Percentage of positive and negative words used in remarks - Standardwise  
bing_sentiment_counts_2<-data_sentiment%>%
  group_by(Standard.name)%>%
  count(sentiment)%>%
  mutate(total=sum(n),percent=n*100/total)
head(bing_sentiment_counts_2)
ggplot(bing_sentiment_counts_2,aes(Standard.name,percent,fill=sentiment))+geom_bar(stat="Identity")

#Percentage of positive and negative words used in remarks-Schoolwise andStandardwise  
bing_sentiment_counts_3<-data_sentiment%>%
  group_by(School.Name,Standard.name)%>%
  count(sentiment)%>%
  mutate(total=sum(n),percent=n*100/total)
head(bing_sentiment_counts_3)
ggplot(bing_sentiment_counts_3,aes(Standard.name,percent,fill=sentiment))+geom_col(show.legend = FALSE)+facet_wrap(~School.Name,scales="free")+coord_flip()

#Percentage of positive and negative words used in remarks-Facilitator wise  
data_sentiment$Facilitator<-as.factor(data_sentiment$Facilitator)
bing_sentiment_counts_5<-data_sentiment%>%
  group_by(Facilitator,Standard.name)%>%
  count(sentiment)%>%
  mutate(total=sum(n),percent=n*100/total)
head(bing_sentiment_counts_5)
ggplot(bing_sentiment_counts_5,aes(Standard.name,percent,fill=sentiment))+geom_col(show.legend = FALSE)+facet_wrap(~Facilitator,scales="free")+coord_flip()

# word contributions
word_counts<-data_sentiment%>%
count(word,sentiment)
head(word_counts)

top_words<-word_counts%>%
  group_by(sentiment)%>%
  top_n(10)%>%
  ungroup()%>%
  mutate(word=reorder(word,n))
ggplot(top_words,aes(word,n,fill=sentiment))+geom_col(show.legend = FALSE)+facet_wrap(~sentiment,scales="free")+coord_flip()


# Analysing the text using nrc lexicon
data2_sentiment<-tidy_data%>%
inner_join(get_sentiments("nrc"))

head(data2_sentiment)
filter(data2_sentiment,sentiment=="anticipation")

sentiment_counts_n<-data2_sentiment %>%
  count(School.Name,Standard.name,sentiment)

b<-sentiment_counts_n%>%
  group_by(School.Name,Standard.name)%>%
  mutate(total=sum(n),percent=(n*100)/total)
b
ggplot(b,aes(Standard.name,percent,fill=sentiment))+geom_bar(stat="identity")+facet_wrap(~School.Name)

# Analysing the text using afinn lexicon
data3_sentiment<-tidy_data%>%
  inner_join(get_sentiments("afinn"))
  head(data3_sentiment)
  
c<-data3_sentiment%>%
  group_by(School.Name,Standard.name)%>%
  summarise(Average=mean(score,na.rm=TRUE))

ggplot(c,aes(Standard.name,Average))+geom_bar(stat="identity")+facet_wrap(~School.Name)



