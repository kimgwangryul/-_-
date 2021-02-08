#store_id별 관련 변수들

###################################################################################################
#각 store별 빈도수 계산
count = train %>% filter(!is.na(amount)) %>% filter(amount >= 0) %>%
  group_by(store_id) %>% tally()
head(count)
count[count$n==1,] #거래기록 1번인 가게들


###################################################################################################
#각 store별 평균 amount양 계산
avg = train %>% filter(!is.na(amount)) %>%
  group_by(store_id) %>% summarize(avg = mean(amount))
head(avg)
avg[avg$avg==max(avg$avg),] #거래평균 제일 높은 가게

###################################################################################################

#각 store별 unique한 카드개수(고객수)
uniq = train %>% select(store_id, card_id)
uniq = unique(uniq) #중복행 제거
uniq = uniq %>% group_by(store_id) %>% tally()
head(uniq)

###################################################################################################


#각 store별 할부수

train$installments[is.na(train$installments)] <- 0 #NA(할부X)를 0 으로 채우기

#store별 할부 빈도수
installment_count = 
  train %>% select(store_id, installments) %>%
  filter(installments != 0) %>% 
  group_by(store_id) %>% tally()
#나중에 상대빈도로 바꿀필요가 있음(총 거래빈도수가 다르므로)


#각 store별 평균할부개월수(할부를 한 사람만 계산했음, 일시불로 계산한 사람 제외)
installment_avg = 
  train %>% select(store_id, installments) %>%
  filter(!is.na(installments)) %>% filter(installments != 0) %>%
  group_by(store_id) %>% summarise(mean_install = mean(installments))



###################################################################################################

#각 store별 환불 빈도

minus_amount = 
  train %>% select(store_id, amount) %>%
  filter(amount < 0) %>% group_by(store_id) %>% tally()
#나중에 전체 n으로 나누기(상대도수로 봐야 유의미)


###################################################################################################

#공휴일에 문 닫는 store찾기

holiday_count

holiday_running = 
  holiday_count %>% select(store_id, train$holiday) %>%
  group_by(store_id) %>% tally()


holiday_close =
  holiday_running %>% filter(n == 1) 

holiday_open = 
  holiday_running %>% filter(n == 2)
###################################################################################################

#주7일운영 store vs 주7일이하운영 store
week_count

week_running =
  week_count %>% group_by(store_id) %>% tally()

#주7일운영
week_open = 
  week_running %>% filter(n == 7)
#주7일운영X
week_close =
  week_running %>% filter(n != 7)

###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################


