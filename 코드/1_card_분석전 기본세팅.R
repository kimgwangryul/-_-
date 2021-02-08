#패키지 및 라이브러리
library(dplyr)
library(ggplot2)

#데이터 가지고 놀 폴더지정       
getwd()
setwd("C:/Users/GWANGRYUL/Desktop/스터디/카드/카드데이터1/data04")

#데이터 불러오기
train <- read.csv("train.csv")
#test <- read.csv("test.csv")
attach(train)
#attach(test)

#오타수정
colnames(train)[8] <- "holiday"
colnames(test)[8] <- "holiday"

#데이터 둘러보기
names(train)
names(test)
length(unique(train$store_id))
length(unique(test$store_id))
head(train)