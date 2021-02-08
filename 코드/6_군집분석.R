#패키지 라이브러리 설치
#install.packages("NbClust")
library(NbClust)


#군집분석을 위해 변수 전처리
#1. uniq -> uniq/count * 100 (전체결제대비 unique한 고객 비율)
#2. installment_count 삭제
#3. minus_amount -> minus_amount/count * 100 (전체결제대비 환불결제 비율)
#4. hour -> hour/count*100 (시간대별 결제 비율)

#NA값을 0으로 대체한 데이터셋 result0하나 만들어줌(나중에 사용)
result0 = result
result0[is.na(result0)] = 0

clus = result
clus[is.na(clus)] = 0 #결측치 0으로 대체
clus$uniq_p = clus$uniq/clus$count*100
clus$minus_amount_p = clus$minus_amount/clus$count/100
clus$hour2_p = clus$hour2/clus$count*100
clus$hour6_p = clus$hour6/clus$count*100
clus$hour10_p = clus$hour10/clus$count*100
clus$hour14_p = clus$hour14/clus$count*100
clus$hour18_p = clus$hour18/clus$count*100
clus$hour22_p = clus$hour22/clus$count*100

clus = clus[,c(1,2,3,6,8,9,10,11,18:25)] #필요한 변수들만 선택


#군집분석을 위해 데이터 컬럼별 정규화(평균:0, 표준편차:1)
clus_scale = scale(clus[,-1]) #store는 군집분석할때는 잠깐 빼줘야함
clus_scale = as.data.frame(clus_scale)

#데이터 형태 중간점검
View(clus)
View(clus_scale)


#Ward 군집분석(군집간 거리를 최대로, 군집의개수를 최대한 고르게 분포할 수 있도록 하기 위해서)
d = dist(clus_scale)
fit.ward = hclust(d, method='ward.D')
#트리모형 그려보기
par(mfrow=c(1,1))
plot(fit.ward,hang=-1,cex=.8,main="Ward Clustering")

#몇개의 군집으로 나눌것인가?
devAskNewPage(ask=TRUE)
#무한행렬 오류로 인하여 
#5번째변수(holiday_close) : 6번째변수로 설명이됨(자유도, 배반관계)
#15번째변수(hour22_p) : 11,12,13,14변수로 설명이됨 (자유도 문제)는 제외하였음
nc=NbClust(clus_scale[,c(1,2,3,4,6,7,8,9,10,11,12,13,14)], distance="euclidean", min.nc=2, max.nc=15, method="ward.D")
#위에서 나온 그래프들을 토대로 군집을 8개로 정함


#8개로 나눈 군집분석
clusters <- cutree(fit.ward,k=8)
table(clusters)

cluster_result = cbind(clusters, clus) #군집분석이 포함된 데이터


#트리구조그림에서의 나뉜 군집의 시각화
par(mfrow=c(1,1))
plot(fit.ward,hang=-1, cex=.8, main="Ward Linkage Clustering\n8 Cluster Solution")
rect.hclust(fit.ward,k=8)


#군집별 기초통계량보기
g_median = aggregate(clus[,-1],by=list(cluster=clusters),median)
g_mean = aggregate(clus[,-1],by=list(cluster=clusters),mean)
g_max = aggregate(clus[,-1],by=list(cluster=clusters),max)
g_min = aggregate(clus[,-1],by=list(cluster=clusters),min)

#aggregate(as.data.frame(clus),by=list(cluster=clusters),median) 정규화된데이터셋의의 군집통계량


#군집별 기초통계량을 바탕으로 8개 군집의 특성을 잡아보자
g_median
g_mean
g_max
g_min