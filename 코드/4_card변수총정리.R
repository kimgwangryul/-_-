View(train)
count              #store별 거래빈도수
avg                #store별 평균거래가격
uniq               #store별 unique한 카드 개수(고객수)
installment_count  #store별 할부한 고객수
installment_avg    #store별 할부한 고객들의 평균 할부개월수
minus_amount       #store별 환불한 고객수        

month_count        #store별 월별 거래빈도
month_avg          #store별 월별 평균거래가격

hour1              #store별 2시~6시(새벽) 거래빈도
hour2              #store별 6시~10시(아침&출근) 거래빈도
hour3              #store별 10시~14시(낮&점심) 거래빈도
hour4              #store별 14시~18시(오후&일과) 거래빈도
hour5              #store별 18시~22시(저녁&퇴근이후) 거래빈도
hour6              #store별 22시~2시(심야) 거래빈도

holiday_count      #store별 공휴일/비공휴일 거래빈도수
holiday_avg        #store별 공휴일/비공휴일 평균거래가격
holiday_running
holiday_open       #공휴일에도 영업하는 store
holiday_close      #공휴일에 영업하지 않는 store

week_count         #store별 요일별 거래빈도수
week_avg           #store별 요일별 평균거래가격
week_running
week_open          #주7일 영업하는 store
week_close         #주7일 미만 영업하는 store



A = left_join(count , avg, by = c("store_id"="store_id"))
A = left_join(A , uniq, by = c("store_id"="store_id"))
A = left_join(A , installment_count, by = c("store_id"="store_id"))
A = left_join(A , installment_avg, by = c("store_id"="store_id"))
A = left_join(A , minus_amount, by = c("store_id"="store_id"))
#A = left_join(A , month_count, by = c("store_id"="store_id"))
#A = left_join(A , month_avg, by = c("store_id"="store_id"))
#A = left_join(A , holiday_count, by = c("store_id"="store_id"))
#A = left_join(A , holiday_avg, by = c("store_id"="store_id"))
A = left_join(A , holiday_open, by = c("store_id"="store_id"))
A = left_join(A , holiday_close, by = c("store_id"="store_id"))
#A = left_join(A , week_count, by = c("store_id"="store_id"))
#A = left_join(A , week_avg, by = c("store_id"="store_id"))
A = left_join(A , week_open, by = c("store_id"="store_id"))
A = left_join(A , week_close, by = c("store_id"="store_id"))
A = left_join(A , hour1, by = c("store_id"="store_id"))
A = left_join(A , hour2, by = c("store_id"="store_id"))
A = left_join(A , hour3, by = c("store_id"="store_id"))
A = left_join(A , hour4, by = c("store_id"="store_id"))
A = left_join(A , hour5, by = c("store_id"="store_id"))
A = left_join(A , hour6, by = c("store_id"="store_id"))

colnames(A) = c('store_id','count','avg','uniq','installment_count','installment_avg',
                'minus_amount','holiday_open','holiday_close','week_open','week_close',
                'hour2','hour6','hour10','hour14','hour18','hour22')
result = A
View(result)

