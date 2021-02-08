#군집분석의 결과 + 백분율로 수정한 store별 데이터
View(cluster_result)

#군집별 store개수
table(cluster_result$clusters)

#군집별 기초 통계량
g_median
g_mean
g_max
g_min

#군집분석 변수별 pair 그래프
par(mfrow=c(1,1))
pairs(cluster_result, main = "군집분석 변수별 pair 그래프",
      pch = 21, bg = c('red','orange','yellow','green','blue','darkblue','purple','pink')[unclass(cluster_result$clusters)])



#군집별로 데이터를 만들고 변수들의 분포를 보자

#그룹1
g1 = cluster_result %>% filter(clusters==1)
#그룹2
g2 = cluster_result %>% filter(clusters==2)
#그룹3
g3 = cluster_result %>% filter(clusters==3)
#그룹4
g4 = cluster_result %>% filter(clusters==4)
#그룹5
g5 = cluster_result %>% filter(clusters==5)
#그룹6
g6 = cluster_result %>% filter(clusters==6)
#그룹7
g7 = cluster_result %>% filter(clusters==7)
#그룹8
g8 = cluster_result %>% filter(clusters==8)



#군집간 변수별로 레이더 그래프 만들기(군집의 특징을 찾아보자)
#install.packages("fmsb")
library(fmsb)

#레이더 그래프를 만들어주기 위해서는 함
#첫행에는 각 열의 최대값
#두번째열에는 각 열의최소값이 필요함
#그 작업을 해주기 위한 함수

# manipulating dataset for radar chart
  # data frame includes possible maximum values as row 1 
  # and possible minimum values as row 2
  df_radarchart <- function(df) {
     df <- data.frame(df)
     dfmax <- apply(df, 2, max) 
     dfmin <- apply(df, 2, min) 
     as.data.frame(rbind(dfmax, dfmin, df))
    }
# maximum value as row 1, minimum value as row 2 : user-defined function df_radarchart
# standardization : scale()

  
#군집별 색정해주기
colors_in=c(rgb(1,0,0,0.4), rgb(0,1,0,0.4) , rgb(0,0,1,0.4) , rgb(1,1,0,0.4), rgb(1,0,1,0.4) , rgb(0,1,1,0.4), rgb(0.4,0.7,0.4,0.4) , rgb(0.4,0.2,0.2,0.4) )
colors_border=c(rgb(1,0,0,0.9), rgb(0,1,0,0.9) , rgb(0,0,1,0.9) , rgb(1,1,0,0.9), rgb(1,0,1,0.9) , rgb(0,1,1,0.9), rgb(0.4,0.7,0.4,0.9) , rgb(0.4,0.2,0.2,0.9) )



##평균값(g_mean)값

#군집들의 일반적(거래내역) 특징 - 평균값
g_mean1 <- df_radarchart(scale(g_mean[,c(2,3,4,9,10)]))
radarchart(g_mean1,pfcol=colors_in, pcol=colors_border, plwd=4 , plty=1 ) 
legend(x=1.5, y=1, legend = rownames(g_mean1[-c(9,10),]), bty = "n", pch=20 ,text.col = "grey", cex=1.2, pt.cex=3,col=colors_border)

#군집들의 공휴일/주중주말 오픈 유무값 - 평균값
g_mean2 <- df_radarchart(scale(g_mean[,c(5,6,7,8)]))
radarchart(g_mean2,pfcol=colors_in, pcol=colors_border, plwd=4 , plty=1 ) 
legend(x=1.5, y=1, legend = rownames(g_mean2[-c(9,10),]), bty = "n", pch=20 ,text.col = "grey", cex=1.2, pt.cex=3,col=colors_border)

#군집들의 시간대별 특징 - 평균값
g_mean3 <- df_radarchart(scale(g_mean[,c(11:16)]))
radarchart(g_mean3,pfcol=colors_in, pcol=colors_border, plwd=4 , plty=1 ) 
legend(x=1.5, y=1, legend = rownames(g_mean3[-c(9,10),]), bty = "n", pch=20 ,text.col = "grey", cex=1.2, pt.cex=3,col=colors_border)


##평균값 해석

#1군집
#거래빈도(count)가 매우 많음
#평균거래가격(avg)가 매우낮음
#단골고객비율(uniq_p)이 매우 낮음
#공휴일 open
#주말 open
#10시~18시 고르게 분포

#2군집
#거래빈도(count)가 매우 적음
#평균거래가격(avg)가 높음
#공휴일 close
#(대부분)주말 close
#1위: 14시~18시
#2위: 10시~14시, 18시~22시

#3군집
#공휴일 open
#주말 close
#10시~18시
#1위: 14시~18시
#2위: 10시~14시, 18시~22시

#4군집
#거래빈도(count)가 많음
#평균거래가격(avg)이 매우낮음
#평균할부개월수(installment_avg)가 매우 낮음
#환불비율(minus_amount_p)이 낮음
#공휴일 open
#주말 open
#1위: 18시~22시
#2위: 10시~14시
#3위: 14시~18시

#5군집
#평균할부개월수(installment_avg)가 매우 높음
#공휴일 open
#주말 open
#1위: 14시~18시(42%)
#2위: 10시~14시, 18시~22시

#6군집
#평균거래가격(avg)이 낮음
#단골고객비율(uniq_p)이 높음
#공휴일 open
#주말 open
#1위: 18시~22시(52%)
#2위: 22시~2시

#7군집
#공휴일 open
#주말 open
#1위: 22시~2시(60%)
#2위: 2시~6시

#8군집
#평균거래가격(avg)가 매우 높음
#환불비율(minus_amount_p)이 매우매우높음
#공휴일 open/close반반
#(대부분)주말 close
#1위: 10시~14시
#2위: 14시~18시
#3위: 6시~10시
#4위: 18시~22시

##중앙값(g_median)

#군집들의 일반적(거래내역) 특징 - 중앙값
g_median1 <- df_radarchart(scale(g_median[,c(2,3,4,9,10)]))
radarchart(g_median1,pfcol=colors_in, pcol=colors_border, plwd=4 , plty=1 ) 
legend(x=1.5, y=1, legend = rownames(g_median1[-c(9,10),]), bty = "n", pch=20 ,text.col = "grey", cex=1.2, pt.cex=3,col=colors_border)

#군집들의 공휴일/주중주말 오픈 유무 - 중앙값
g_median2 <- df_radarchart(scale(g_median[,c(5,6,7,8)]))
radarchart(g_median2,pfcol=colors_in, pcol=colors_border, plwd=4 , plty=1 ) 
legend(x=1.5, y=1, legend = rownames(g_median2[-c(9,10),]), bty = "n", pch=20 ,text.col = "grey", cex=1.2, pt.cex=3,col=colors_border)

#군집들의 시간대별 특징 - 중앙값
g_median3 <- df_radarchart(scale(g_median[,c(11:16)]))
radarchart(g_median3,pfcol=colors_in, pcol=colors_border, plwd=4 , plty=1 ) 
legend(x=1.5, y=1, legend = rownames(g_median3[-c(9,10),]), bty = "n", pch=20 ,text.col = "grey", cex=1.2, pt.cex=3,col=colors_border)



##중앙값 해석

#1군집
#거래빈도(count)가 매우 많음
#평균거래가격(avg)가 매우매우낮음
#단골고객비율(uniq_p)이 매우 낮음
#공휴일 open
#주말 open
#10시~18시 고르게 분포

#2군집
#거래빈도(count)가 매우 적음
#평균거래가격(avg)가 매우높음
#단골고객비율(uniq_p)이 높음
#환불(minus_amount_p)을 하지않음
#공휴일 close
#(대부분)주말 close
#1위: 14시~18시
#2위: 10시~14시, 18시~22시

#3군집
#거래빈도(count)가 적음
#공휴일 open
#주말 close
#10시~18시
#1위: 14시~18시
#2위: 10시~14시, 18시~22시

#4군집
#거래빈도(count)가 많음
#평균거래가격(avg)이 낮음
#할부(installment_avg)를 하지 않음
#환불비율(minus_amount_p)이 낮음
#공휴일 open
#주말 open
#1위: 18시~22시
#2위: 10시~14시
#3위: 14시~18시

#5군집
#평균할부개월수(installment_avg)가 매우 높음
#공휴일 open
#주말 open
#1위: 14시~18시(42%)
#2위: 10시~14시, 18시~22시

#6군집
#평균거래가격(avg)이 낮음
#단골고객비율(uniq_p)이 높음
#환불(minus_amount_P)비율이 낮음
#공휴일 open
#주말 open
#1위: 18시~22시(52%)
#2위: 22시~2시

#7군집
#환불(minus_amount_p)비율이 높음
#공휴일 open
#주말 open
#1위: 22시~2시(60%)
#2위: 2시~6시

#8군집
#거래빈도(count)가 매우매우 적음
#할부(installment_avg)를 하지 않음
#단골고객비율(uniq_p)이 낮음
#환불비율(minus_amount_p)이 매우높음
#공휴일 open/close반반
#(대부분)주말 close
#1위: 10시~14시
#2위: 14시~18시
#3위: 6시~10시
#4위: 18시~22시


#평균값으로 봤을때의 특징 vs 중앙값으로 보았을때의 특징

par(mfrow= c(1,2))
#군집들의 일반적(거래내역) 특징 - 평균값 vs 중앙값
radarchart(g_mean1,pfcol=colors_in, pcol=colors_border, plwd=4 , plty=1, title = "평균값" ) 
radarchart(g_median1,pfcol=colors_in, pcol=colors_border, plwd=4 , plty=1, title = "중앙값" ) 

#군집들의 공휴일/주중주말 오픈 유무 - 평균값 vs 중앙값
radarchart(g_mean2,pfcol=colors_in, pcol=colors_border, plwd=4 , plty=1, title = "평균값" ) 
radarchart(g_median2,pfcol=colors_in, pcol=colors_border, plwd=4 , plty=1, title = "중앙값" ) 

#군집들의 시간대별 특징 - 평균값 vs 중앙값
radarchart(g_mean3,pfcol=colors_in, pcol=colors_border, plwd=4 , plty=1, title = "평균값" ) 
radarchart(g_median3,pfcol=colors_in, pcol=colors_border, plwd=4 , plty=1, title = "중앙값") 


#레이더 차트의 개형이 비슷
# - 군집들의 공휴일/주중주말 오픈 유무
# - 군집들의 시간대별 특징 - 평균값 vs 중앙값

#레이더 차트의 개형이 다름
# - 군집들의 일반적(거래내역) 특징 - 평균값 vs 중앙값
