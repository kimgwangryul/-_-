#(추가)그래프 타이틀 붙여주기
#+ggtitle("그래프 타이틀") +
#  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) 



#월별 거래빈도수
M1 = train %>% group_by(month) %>% tally()
M1$month = c('Jan', 'Feb', 'Mar','Apr','May','Jun', 'Jul','Aug', 'Sep', 'Oct','Nov','Dec')
ggplot(data=M1, aes(x=month, y = n)) + geom_bar(stat = 'identity',fill ='orange1') +
  scale_x_discrete(limits=M1$month)

#월별 평균거래가격
M2 = train %>% group_by(month) %>% summarise(tot_mean = mean(amount))
M2$month = c('Jan', 'Feb', 'Mar','Apr','May','Jun', 'Jul','Aug', 'Sep', 'Oct','Nov','Dec')
ggplot(data=M2, aes(x=month, y = tot_mean)) + geom_bar(stat = 'identity',fill ='orange1') +
  scale_x_discrete(limits=M2$month)

#############################################################################
#요일별 거래빈도수
W1 = train %>% group_by(days_of_week) %>% tally()
W1$days_of_week = c('Mon', 'Tue', 'Wen','Thu','Fri','Sat', 'Sun')
ggplot(data=W1, aes(x=days_of_week, y = n)) + geom_bar(stat = 'identity', fill='salmon') +
  scale_x_discrete(limits=W1$days_of_week)

#요일별 평균거래가격
W2 = train %>% group_by(days_of_week) %>% summarise(tot_mean = mean(amount))
W2$days_of_week = c('Mon', 'Tue', 'Wen','Thu','Fri','Sat', 'Sun')
ggplot(data=W2, aes(x=days_of_week, y = tot_mean)) + geom_bar(stat = 'identity', fill= 'salmon') +
  scale_x_discrete(limits=W2$days_of_week)
#############################################################################


#공휴일/비공휴일 거래빈도수
H1 = train %>% group_by(holiday) %>% tally()
H1$holiday = c('non-holiday','holiday')
ggplot(data=H1, aes(x=holiday, y = n)) + geom_bar(stat = 'identity', fill= 'darkblue') +
  scale_x_discrete(limits=H1$holiday)

#의미없음(평균으로보는게 맞음)
h1 = train %>% select(date, holiday) %>% distinct() %>%
  filter(holiday == 1) %>%  tally() #공휴일수
h0 = train %>% select(date, holiday) %>% distinct() %>% 
  filter(holiday == 0) %>%  tally() #비공휴일수
H1$n = H1$n/c(as.numeric(h0),as.numeric(h1))
ggplot(data=H1, aes(x=holiday, y = n)) + geom_bar(stat = 'identity', fill= 'darkblue') +
  scale_x_discrete(limits=H1$holiday)


#공휴일/비공휴일 평균거래가격
H2 = train %>% group_by(holiday) %>% summarise(tot_mean = mean(amount))
H2$holiday = c('non-holiday','holiday')
ggplot(data=H2, aes(x=holiday, y = tot_mean)) + geom_bar(stat = 'identity', fill= 'darkblue') +
  scale_x_discrete(limits=H2$holiday)
#############################################################################


#시간대별 거래빈도수
T1 = train %>% group_by(hour) %>% tally()
ggplot(data=T1, aes(x=hour, y = n)) + geom_bar(stat = 'identity', fill="royalblue") +
  scale_x_discrete(limits=T1$hour)

#시간대별 평균거래가격
T2 = train %>% group_by(hour) %>% summarise(tot_mean = mean(amount))
ggplot(data=T2, aes(x=hour, y = tot_mean)) + geom_bar(stat = 'identity',fill='royalblue') +
  scale_x_discrete(limits=T2$hour) 


#############################################################################

#환불 경향성
hwan$minus_amount_p
hwan = result %>% select(store_id, count, minus_amount) 
hwan[is.na(hwan)] = 0
hwan$minus_amount_p = hwan$minus_amount/hwan$count*100
ggplot(data=hwan, aes(x=store_id, y = minus_amount_p)) + geom_bar(stat = 'identity', fill='skyblue') +
  scale_x_discrete(limits=hwan$store_id)


# x축:평균환불비율, y축:store의 개수 
# 보통 환불 안하는 경우가 많은듯
ggplot(data=hwan, aes(x=minus_amount_p)) + geom_histogram(binwidth = 1,color = 'black', fill = 'skyblue')
#로그 씌워봄
ggplot(data=hwan, aes(x=log(minus_amount_p))) + geom_histogram(binwidth = 0.1,color = 'black', fill = 'skyblue')

#############################################################################


#할부 경향성

hal = result %>% select(store_id, installment_count, installment_avg) 
hal[is.na(hal)] = 1
ggplot(data=hal, aes(x=store_id, y = installment_avg)) + geom_bar(stat = 'identity', fill='green') +
  scale_x_discrete(limits=hal$store_id) 

# x축:평균할부개월수, y축:store의 개수 
# 보통 일시불(1)과 3개월이 많음
ggplot(data=hal, aes(x=installment_avg)) + geom_histogram(binwidth = 1,color = 'black', fill = 'green')
#로그 씌워봄
ggplot(data=hal, aes(x=log(installment_avg))) + geom_histogram(binwidth = 0.1,color = 'black', fill = 'green')





