#시계열 관련 변수들


###################################################################################################
#연, 월, 일, 시, 분, 초 변수 추가
train$year = as.numeric(substr(train$date, 1,4)) #년도만 숫자형태로 따옴
train$month = as.numeric(substr(train$date, 6,7)) #월만 숫자형태로 따옴
train$day = as.numeric(substr(train$date, 9,10)) #일만 숫자형태로 따옴
train$hour = as.numeric(substr(train$time, 1,2)) #Hour만 숫자형태로 따옴
train$min = as.numeric(substr(train$time, 4,5)) #Minute만 숫자형태로 따옴
train$sec = as.numeric(substr(train$time, 7,8)) #Second만 숫자형태로 따옴

head(train)


###################################################################################################

#각 스토어별 시간대별 거래빈도 (하루를 4시간*6개로 나누자)

#각 스토어별 time
hour1 = train %>% #2시~6시(새벽)
  filter(amount >= 0) %>% filter(hour >=2 & hour<6) %>% 
  group_by(store_id) %>% tally()

hour2 = train %>% #6시~10시(아침&출근)
  filter(amount >= 0) %>% filter(hour >=6 & hour<10) %>% 
  group_by(store_id) %>% tally()

hour3 = train %>% #10시~14시(낮&점심)
  filter(amount >= 0) %>% filter(hour >=10 & hour<14) %>% 
  group_by(store_id) %>% tally()

hour4 = train %>% #14시~18시(오후&일과)
  filter(amount >= 0) %>% filter(hour >=14 & hour<18) %>% 
  group_by(store_id) %>% tally()

hour5 = train %>% #18시~22시(저녁&퇴근이후)
  filter(amount >= 0) %>% filter(hour >=18 & hour<22) %>% 
  group_by(store_id) %>% tally()

hour6 = train %>% #22시~2시(심야)
  filter(amount >= 0) %>% filter(hour ==0|hour==1|hour==22|hour==23) %>% 
  group_by(store_id) %>% tally()


###################################################################################################


#각 store별 월별 거래빈도 & 평균amount

month_count = 
  train %>% filter(!is.na(amount)) %>% filter(amount >=0) %>%
  select(store_id, month) %>% 
  group_by(store_id, month) %>% tally() 

month_avg = 
  train %>% filter(!is.na(amount)) %>% filter(amount >=0) %>%
  select(store_id, month, amount) %>% 
  group_by(store_id, month)  %>% summarise(mean_month = mean(amount))

###################################################################################################


#각 store별 요일별 거래빈도 & 평균amount

week_count =
  train %>% select(store_id, month) %>% 
  group_by(store_id, train$days_of_week) %>% tally() 
colnames(week_count)[2] = 'days_of_week'

week_avg = 
  train %>% select(store_id, month, amount) %>% 
  group_by(store_id, train$days_of_week)  %>% summarise(mean_month = mean(amount))
colnames(week_avg)[2] = 'days_of_week'
###################################################################################################

#각 store별 공휴일/비공휴일별 거래빈도 & 평균amount

holiday_count = 
  train %>%
  select(store_id, month) %>% 
  group_by(store_id, train$holiday) %>% tally() 
colnames(holiday_count)[2] = 'holiday'

holiday_avg = 
  train %>% 
  select(store_id, month, amount) %>% 
  group_by(store_id, train$holiday)  %>% summarise(mean_month = mean(amount))
colnames(holiday_avg)[2] = 'holiday'




