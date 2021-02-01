#패키지 및 라이브러리
library(dplyr)
library(ggplot2)

#데이터 가지고 놀 폴더지정       
getwd()
setwd("C:/Users/GWANGRYUL/Desktop/스터디/카드데이터1/data04")

#데이터 불러오기
train <- read.csv("train.csv")
test <- read.csv("test.csv")
attach(train)
attach(test)
#오타수정
colnames(train)[8] <- "holiday"
colnames(test)[8] <- "holiday"

#데이터 둘러보기
names(train)
names(test)
length(unique(train$store_id))
length(unique(test$store_id))
head(train)

#############################1###############################

#각 스토어별 빈도수 계산
count = train %>% filter(!is.na(amount)) %>%
  group_by(store_id) %>% tally()
head(count)
count[count$n==1,]
avg[avg$avg==max(avg$avg),]


#각 스토어별 평균 amount양 계산
avg = train %>% filter(!is.na(amount)) %>%
  group_by(store_id) %>% summarize(avg = mean(amount))
head(avg)

#데이터 합치기
data1 = merge(count, avg) #data1 = 총거래빈도 + 거래금액평균

#############################2###############################

#각 스토어별 time별 거래빈도 (하루를 4시간*6개로 나누자)
train$hour = as.numeric(substr(train$time, 1,2)) #결제Hour만 숫자형태로 따옴

#각 스토어별 time
hour1 = train %>%
  filter(amount > 0) %>% filter(hour >=2 & hour<6) %>% 
  group_by(store_id) %>% tally()

hour2 = train %>%
  filter(amount > 0) %>% filter(hour >=6 & hour<10) %>% 
  group_by(store_id) %>% tally()

hour3 = train %>%
  filter(amount > 0) %>% filter(hour >=10 & hour<14) %>% 
  group_by(store_id) %>% tally()

hour4 = train %>%
  filter(amount > 0) %>% filter(hour >=14 & hour<18) %>% 
  group_by(store_id) %>% tally()

hour5 = train %>%
  filter(amount > 0) %>% filter(hour >=18 & hour<22) %>% 
  group_by(store_id) %>% tally()

hour6 = train %>%
  filter(amount > 0) %>% filter(hour ==0|hour==1|hour==23|hour==24) %>% 
  group_by(store_id) %>% tally()


#데이터 합치기
#data2 = data1 + 시간대별 거래수
#Join이용해서 시간대별 거래수 채우기
data2 = left_join(data1 , hour1, by = c("store_id"="store_id"))
data2 = left_join(data2 , hour2, by = c("store_id"="store_id"))
data2 = left_join(data2 , hour3, by = c("store_id"="store_id"))
data2 = left_join(data2 , hour4, by = c("store_id"="store_id"))
data2 = left_join(data2 , hour5, by = c("store_id"="store_id"))
data2 = left_join(data2 , hour6, by = c("store_id"="store_id"))
data2[is.na(data2)] <- 0 #NA값 0으로 채워주기
data2

#열이름 바꿔주기
colnames(data2) <- c('store_id', 'n', 'avg', 'hour2', 'hour6', 'hour10', 'hour14', 'hour18', 'hour22')
data2_percent = data2[,4:9]/data2$n *100 #거래빈도 퍼센트화
data2 = cbind(data2, data2_percent) #데이터병합
colnames(data2) <- c('store_id', 'n', 'avg', 'hour2', 'hour6', 'hour10', 'hour14', 'hour18', 'hour22',
                     'hour2_p', 'hour6_p', 'hour10_p', 'hour14_p', 'hour18_p', 'hour22_p')
head(data2)
data2_p = data2[,c(-4:-9)]
head(data2_p)


################################3#################################

#할부

train$installments[is.na(train$installments)] <- 0 #NA(할부X)를 0 으로 채우기

#store별 할부 평균 개월수
m_install = 
  train %>% select(store_id, installments) %>%
  filter(!is.na(installments)) %>% 
  group_by(store_id) %>% summarise(mean_install = mean(installments))

#store별 할부 빈도수 
c_install = 
  train %>% select(store_id, installments) %>%
  filter(installments != 0) %>% 
  group_by(store_id) %>% tally()
#상대빈도로 바꿀필요가 있음(총 거래빈도수가 다르므로)

#환불 데이터 병합
data3 = left_join(data2 , m_install, by = c("store_id"="store_id"))
data3 = left_join(data3 , c_install, by = c("store_id"="store_id"))
colnames(data3)[c(2,17)] <- c('n',"count_install") #열이름 바꾸기 
data3$count_install[is.na(data3$count_install)] <- 0 #환불횟수 NA -> 0으로 바꾸기
data3$count_install_p = data3$count_install/data3$n #홧불횟수 상대 도수로 바꾸기
head(data3)


#################################################################
#데이터정리
#################################################################

#모든 정보 다 있는 데이터 
result = data3

View(result)
#SAS에서 분석할 데이터
result1 = data1
result2 = data2_p
result3 = data3[c(-4:-9,-17)]

##데이터 출력하기

write.csv(result1, "result1.csv")
write.csv(result2, "result2.csv")
write.csv(result3, "result3.csv")
write.csv(result, "result.csv")





############################################################

#cluster1d를 로그화(나중에 그래프 쉽게 보기 위해서)
cluster1d = read.csv("cluster1d.csv")를
cluster1d$n = log(cluster1d$n)
cluster1d$avg = log(cluster1d$avg)
cluster1d_log = cluster1d
head(cluster1d_log)
write.csv(cluster1d_log, "cluster1d_log.csv")
############################################################


#SAS에서 주성분분석한 result2 -> prin2 , result3 -> prin3 가져오기 

prin2 = read.csv("prin2.csv")
prin3 = read.csv("prin3.csv")
attach(prin2)
attach(prin3)

############################################################

#3차원 그래프 그리기
library(scatterplot3d)
library(rgl)
scatterplot3d(prin2$Prin1,prin2$Prin2,prin2$Prin3, color = prin2$CLUSTER)
#plot3d(prin2$Prin1,prin2$Prin2,prin2$Prin3, col = prin2$CLUSTER)

scatterplot3d(prin3$Prin1,prin3$Prin2,prin3$Prin3, color = prin3$CLUSTER)
#plot3d(prin3$Prin1,prin3$Prin2,prin3$Prin3, col = prin3$CLUSTER)






