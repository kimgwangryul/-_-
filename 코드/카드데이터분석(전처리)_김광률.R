#패키지 및 라이브러리
library(dplyr)
library(ggplot2)

#데이터 가지고 놀 폴더지정       
getwd()
setwd("C:/Users/GWANGRYUL/Desktop/스터디/카드데이터1/data04")

#데이터 불러오기
train <- read.csv("train.csv")
attach(train)
#오타수정
colnames(train)[8] <- "holiday"

#데이터 둘러보기
names(train)
length(unique(train$store_id))
head(train)


######################변수 추가##########################
#각 store별 빈도수 계산
count = train %>% filter(!is.na(amount)) %>%
  group_by(store_id) %>% tally()
head(count)

count[count$n==1,] #거래가 1개밖에 없는 가게들

##############################################################################

#각 store별 평균 amount양 계산
avg = train %>% filter(!is.na(amount)) %>%
  group_by(store_id) %>% summarize(avg = mean(amount))
head(avg)
avg[avg$avg==max(avg$avg),] #평균 amount양 최고가인 가게

##############################################################################

#각 store별 unique한 카드개수(고객수)
uniq = train %>% select(store_id, card_id) 
uniq = unique(uniq) #중복행 제거
uniq = uniq %>% group_by(store_id) %>% tally()
head(uniq)

##############################################################################

#각 store별 시간대별 거래횟수 (하루를 4시간*6개로 나누자)
train$hour = as.numeric(substr(train$time, 1,2)) #결제Hour만 숫자형태로 따옴

time = train %>% select(store_id)
time = unique(time)
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
#Join이용해서 시간대별 거래수 채우기
time = left_join(time , hour1, by = c("store_id"="store_id"))
time = left_join(time , hour2, by = c("store_id"="store_id"))
time = left_join(time , hour3, by = c("store_id"="store_id"))
time = left_join(time , hour4, by = c("store_id"="store_id"))
time = left_join(time , hour5, by = c("store_id"="store_id"))
time = left_join(time , hour6, by = c("store_id"="store_id"))
time[is.na(time)] <- 0 #NA값 0으로 채워주기
head(time)


#열이름 바꿔주기
colnames(time) <- c('store_id','hour2', 'hour6', 'hour10', 'hour14', 'hour18', 'hour22')
time_percent = time[,2:7]/count$n *100 #거래빈도 퍼센트화
time = cbind(time, time_percent) #데이터병합
colnames(time) <- c('store_id','hour2', 'hour6', 'hour10', 'hour14', 'hour18', 'hour22',
                     'hour2_p', 'hour6_p', 'hour10_p', 'hour14_p', 'hour18_p', 'hour22_p')
head(time)
time_p = time[,c(-2:-7)]
head(time_p)

##############################################################################

#각 store별 환불수

train$installments[is.na(train$installments)] <- 0 #NA(할부X)를 0 으로 채우기

#store별 할부 빈도수 & 평균할부개월수
c_install = 
  train %>% select(store_id, installments) %>%
  filter(installments != 0) %>% 
  group_by(store_id) %>% tally()
#상대빈도로 바꿀필요가 있음(총 거래빈도수가 다르므로)


#각 store별 평균할부개월수
m_install = 
  train %>% select(store_id, installments) %>%
  filter(!is.na(installments)) %>% 
  group_by(store_id) %>% summarise(mean_install = mean(installments))

#환불 데이터 병합
inst = train %>% select(store_id)
inst = unique(inst)
inst = left_join(inst , m_install, by = c("store_id"="store_id"))
inst = left_join(inst , c_install, by = c("store_id"="store_id"))
colnames(inst)[c(2,3)] <- c('n',"count_install") #열이름 바꾸기 
inst$count_install[is.na(inst$count_install)] <- 0 #환불횟수 NA -> 0으로 바꾸기
inst$count_install_p = inst$count_install/count$n*100 #전체 구매대비 환불률
head(inst)

##############################################################################

#각 store별 계절별(월별로 확대가능) 거래횟수

##############################################################################

#공휴일vs비공휴일 별 거래 -> 나아가서 평균차이 유의미한지 t-test까지

##############################################################################