library(dplyr)

#####################################################  1. 데이터 수집  ################################################################## 

# 1.	먼저 read.csv로 아파트 매매 거래 현황 정보 apartment.csv 읽어옴

apartment <- read.csv("C:/Sources/apartment.csv", header = T)
str(apartment)



#####################################################  2. 전처리 및 가공 ################################################################## 
# 2. 결측값 처리: na가 존재하는지 확인하기
table(is.na(apartment))

# 이상값은 존재하지 않는 데이터 --> 이미 서울시가 정리한 데이터이므로 존재하지 않음
apartment <- na.omit(apartment)

# 필요없는 열 제거 ( 행정동.코드,자치구.코드, 공간좌표 )
apartment <- apartment[,-c(2,3,6)]
str(apartment)

# 문자형 데이터를 범주형 데이터로 변환( 자치구.명을 범주형 데이터로 변환)
apartment$자치구.명 <- factor(apartment$자치구.명)
apartment$행정동.명 <- factor(apartment$행정동.명)

str(apartment)
apartment$자치구.명
apartment$행정동.명


# 문자형 데이터를 숫자형 데이터로 변환( 소,중소, 중, 중대, 대형 거래금액이 문자형으로 저장되어있으므로 변환필요 )
apartment$소형.거래금액.평균. <- as.numeric(apartment$소형.거래금액.평균.)
apartment$소형.거래금액.평균.

apartment$중소형.거래금액.평균. <- as.numeric(apartment$중소형.거래금액.평균.)

apartment$중형.거래금액.평균. <- as.numeric(apartment$중형.거래금액.평균.)

apartment$중대형.거래금액.평균. <- as.numeric(apartment$중대형.거래금액.평균.)

apartment$대형.거래금액.평균. <- as.numeric(apartment$대형.거래금액.평균.)

# 변환시 생긴 결측값 처리
table(is.na(apartment))
str(apartment)

clean_apartment <- na.omit(apartment)
table(is.na(clean_apartment))

str(clean_apartment)
summary(clean_apartment)


####################### 각 자치구 아파트의 평방미터 별로 가격 평균 구하기
# 소형 평균
apts <- clean_apartment[,c(1,2,3,4,5)]
apts <- apts[apts$소형.거래금액.평균.!=0,]
str(apts)
summary(apts)

mean(apts$소형.거래금액.평균.) #2014부터 2020까지 모든 소형거래금액의 평균
summarize(group_by(apts, 거래년도, 자치구.명), avg = mean(소형.거래금액.평균.))  #년도별 소형거래금액의 평균

#중소형 평균
aptms <- clean_apartment[,c(1,2,3,6,7)]
aptms <- aptms[aptms$중소형.거래금액.평균.!=0,]
str(aptms)
summary(aptms)

mean(aptms$중소형.거래금액.평균.) 
summarize(group_by(aptms, 거래년도, 자치구.명), avg = mean(중소형.거래금액.평균.))  

#중형 평균
aptm <- clean_apartment[,c(1,2,3,8,9)]
aptm <- aptm[aptm$중형.거래금액.평균.!=0,]
str(aptm)
summary(aptm)

mean(aptm$중형.거래금액.평균.) 
summarize(group_by(aptm, 거래년도, 자치구.명), avg = mean(중형.거래금액.평균.))  


#중대형 평균

aptmb <- clean_apartment[,c(1,2,3,10,11)]
aptmb <- aptmb[aptmb$중대형.거래금액.평균.!=0,]
str(aptmb)
summary(aptmb)

mean(aptmb$중대형.거래금액.평균.) 
summarize(group_by(aptmb, 거래년도, 자치구.명), avg = mean(중대형.거래금액.평균.))  


#대형 평균
aptb <- clean_apartment[,c(1,2,3,12,13)]
summary(aptb)
aptb <- aptb[aptb$대형.거래금액.평균.!=0,]

str(aptb)
summary(aptb)

mean(aptb$대형.거래금액.평균.) 
summarize(group_by(aptb, 거래년도, 자치구.명), avg = mean(대형.거래금액.평균.))



#####################################################  3. 데이터탐색 , 4. 데이터 분석  ################################################################## 

####################### 시각화를 이용한 데이터 탐색하기


#ggplot을 이용한 시각화
library(ggplot2)

# 먼저 각 평방미터별 어느 금액의 아파트들이 거래가 활발하게 일어나고 있는지 파악하기 위해
# 각 평방미터의 거래건수와 거래금액.평균의 상관관계를 거래년도 별로 구분해 시각화 하여 살펴보기
apts %>% ggplot(aes(소형.60평방미터.이하..거래건수, 소형.거래금액.평균.)) + geom_point(alpha=0.2) + facet_wrap(~거래년도)
aptms %>% ggplot(aes(중소형.60...85평방미터..건래건수, 중소형.거래금액.평균.)) + geom_point(alpha=0.2) + facet_wrap(~거래년도)
aptm %>% ggplot(aes(중형.85...102평방미터..거래건수, 중형.거래금액.평균.)) + geom_point(alpha=0.2) + facet_wrap(~거래년도)
aptmb %>% ggplot(aes(중대형.102...135평방미터..거래건수, 중대형.거래금액.평균.)) + geom_point(alpha=0.2) + facet_wrap(~거래년도)
aptb %>% ggplot(aes(대형.135평방미터.이상..거래건수, 대형.거래금액.평균.)) + geom_point(alpha=0.2) + facet_wrap(~거래년도)



# 각 평수별 아파트에 대해 시각화 해보고 이를 토대로 서울 전체 자치구별 연도에따른 가격 상승폭 살펴보기
# 각 평방미터별 아파트 거래금액.평균 연도별 증가추세 시각화
  #소형
  s <- apts %>% group_by(거래년도, 자치구.명) %>% summarize(mean_cost = mean(소형.거래금액.평균.))
  s %>% ggplot(aes(거래년도, mean_cost)) + geom_point() + geom_line() + facet_wrap(~자치구.명)
  
  # 중소형
  ms <- aptms %>% group_by(거래년도, 자치구.명) %>% summarize(mean_cost = mean(중소형.거래금액.평균.))
  ms %>% ggplot(aes(거래년도, mean_cost)) + geom_point() + geom_line() + facet_wrap(~자치구.명)

  
  # 중형
  m <- aptm %>% group_by(거래년도, 자치구.명) %>% summarize(mean_cost = mean(중형.거래금액.평균.))
  m %>% ggplot(aes(거래년도, mean_cost)) + geom_point() + geom_line() + facet_wrap(~자치구.명)

  
  # 중대형
  mb <- aptmb %>% group_by(거래년도, 자치구.명) %>% summarize(mean_cost = mean(중대형.거래금액.평균.))
  mb %>% ggplot(aes(거래년도, mean_cost)) + geom_point() + geom_line() + facet_wrap(~자치구.명)

  
  # 대형
  b <- aptb %>% group_by(거래년도, 자치구.명) %>% summarize(mean_cost = mean(대형.거래금액.평균.))
  b %>% ggplot(aes(거래년도, mean_cost)) + geom_point() + geom_line() + facet_wrap(~자치구.명)
 
  
  
  
# 각 자치구에서 거래금액.평균의 최대 상승폭을 보여준 자치구들의 평수별 아파트 매매가격 연도별 변화 살펴보기
  #소형
  s %>% filter(자치구.명 == "성동구") %>% ggplot(aes(거래년도, mean_cost)) + geom_point() + geom_line()
  s %>% filter(자치구.명 == "송파구") %>% ggplot(aes(거래년도, mean_cost)) + geom_point() + geom_line()
  
  #중소형
  ms %>% filter(자치구.명 == "성동구") %>% ggplot(aes(거래년도, mean_cost)) + geom_point() + geom_line()
  ms %>% filter(자치구.명 == "송파구") %>% ggplot(aes(거래년도, mean_cost)) + geom_point() + geom_line()

  #중형
  m %>% filter(자치구.명 == "성동구") %>% ggplot(aes(거래년도, mean_cost)) + geom_point() + geom_line()
  m %>% filter(자치구.명 == "용산구") %>% ggplot(aes(거래년도, mean_cost)) + geom_point() + geom_line()
  m %>% filter(자치구.명 == "중랑구") %>% ggplot(aes(거래년도, mean_cost)) + geom_point() + geom_line()
  
  #중대형
  mb %>% filter(자치구.명 == "성동구") %>% ggplot(aes(거래년도, mean_cost)) + geom_point() + geom_line()
  
  #대형
  b %>% filter(자치구.명 == "동대문구") %>% ggplot(aes(거래년도, mean_cost)) + geom_point() + geom_line()
  
  
#####################################################  5. 예측모델  ################################################################## 

#회귀모델을 통해 미래의 아파트 가격을 예측해야한다.
apts_model = lm(소형.거래금액.평균.~거래년도, data = apts)
aptms_model = lm(중소형.거래금액.평균.~거래년도, data = aptms)
aptm_model=lm(중형.거래금액.평균.~거래년도, data = aptm)
aptmb_model=lm(중대형.거래금액.평균.~거래년도, data = aptmb)
aptb_model=lm(대형.거래금액.평균.~거래년도, data = aptb)

# 계수 확인하기
coef(apts_model)
coef(aptms_model)
coef(aptm_model)
coef(aptmb_model)
coef(aptb_model)

# 모델 적합 파악
summary(apts_model)
summary(aptms_model)
summary(aptm_model)
summary(aptmb_model)
summary(aptb_model)

# ex_model 모델의 요약을 보면 p-value가 1.524e-09로 매우 작은 값이다 따라서 통계적으로 유의미한 모델임을 알 수 있다.
# intercept 절편의 p-value 또한 1.75-09로 매우 낮으므로, 각 데이터들이 상하로 퍼져있지 않아,
# 절편에 대한 오차가 작게 나타난다.
# 따라서 모델 ex_model은 정확하게 예측하는 모델이라 평가할 수 있다.




#####################################################  7. 예측모델 시각화  ################################################################## 



# 각 평수별 예측모델 시각화
  # 소형
  plot(apts$거래년도, apts$소형.거래금액.평균. , main="소형")
  abline(apts_model, col = 'red')
  
  # 중소형
  plot(aptms$거래년도, aptms$중소형.거래금액.평균., main="중소형")
  abline(aptms_model, col = 'red')
  
  #중형
  plot(aptm$거래년도, aptm$중형.거래금액.평균., main="중형")
  abline(aptm_model, col = 'red')
  
  #중대형
  plot(aptmb$거래년도, aptmb$중대형.거래금액.평균., main="중대형")
  abline(aptmb_model, col = 'red')
  
  #대형
  plot(aptb$거래년도, aptb$대형.거래금액.평균., main="대형")
  abline(aptb_model, col = 'red')
  


# 2021년부터 2025년까지의 거래금액.평균 예측 결과와 학습된 모델 시각화

nx = data.frame(거래년도 = c(2021, 2022, 2023, 2024, 2025))

  #소형
  plot(nx$거래년도, predict(apts_model, nx), xlab= "거래년도", ylab= "예상 거래금액.평균." , col = 'red', cex = 2, pch = 20, main="소형")
  abline(apts_model, col = 'red')
  
  #중소형
  plot(nx$거래년도, predict(aptms_model, nx), xlab= "거래년도", ylab= "예상 거래금액.평균." , col = 'red', cex = 2, pch = 20, main="중소형")
  abline(aptms_model, col = 'red')
  
  #중형
  plot(nx$거래년도, predict(aptm_model, nx), xlab= "거래년도", ylab= "예상 거래금액.평균." , col = 'red', cex = 2, pch = 20, main="중형")
  abline(aptm_model, col = 'red')
  
  #중대형
  plot(nx$거래년도, predict(aptmb_model, nx), xlab= "거래년도", ylab= "예상 거래금액.평균." , col = 'red', cex = 2, pch = 20, main="중대형")
  abline(aptmb_model, col = 'red')
  
  #대형
  plot(nx$거래년도, predict(aptb_model, nx), xlab= "거래년도", ylab= "예상 거래금액.평균." , col = 'red', cex = 2, pch = 20, main="대형")
  abline(aptb_model, col = 'red')




# 학습된 모델과 최대 상승폭을 기록한 자치구의 예측모델 시각화하여 한 그래프에서 비교하기 

  #소형 (성동구와 비교)
  plot(nx$거래년도, predict(apts_model, nx),xlab= "거래년도", ylab= "예상 거래금액.평균." ,  col = 'red', cex = 2, pch = 20, ylim = c(5.0e+08, 1.3e+09), main="소형")
  abline(apts_model, col = 'red')
  
  ex_model = lm(소형.거래금액.평균.~거래년도, data = apts[apts$자치구.명 == "성동구",])
  abline(ex_model, col = 'blue')
  
  legend("topleft", legend = c("성동구 예측모델","apts 예측모델"), lwd = 2 ,col = c("blue", 'red'))
  
  
  #중소형 (성동구)
  plot(nx$거래년도, predict(aptms_model, nx),xlab= "거래년도", ylab= "예상 거래금액.평균." ,  col = 'red', cex = 2, pch = 20, ylim = c(5.0e+08, 1.5e+09), main="중소형")
  abline(aptms_model, col = 'red')
  
  ex_model2 = lm(중소형.거래금액.평균.~거래년도, data = aptms[aptms$자치구.명 == "성동구",])
  abline(ex_model2, col = 'blue')
  
  legend("topleft", legend = c("성동구 예측모델","aptms 예측모델"), lwd = 2 ,col = c("blue", 'red'))
  
  
  
  
  #중형
  plot(nx$거래년도, predict(aptm_model, nx),xlab= "거래년도", ylab= "예상 거래금액.평균." ,  col = 'red', cex = 2, pch = 20, ylim = c(5.0e+08, 1.7e+09), main="중형")
  abline(aptm_model, col = 'red')
  
  ex_model3 = lm(중형.거래금액.평균.~거래년도, data = aptm[aptm$자치구.명 == "용산구",])
  abline(ex_model3, col = 'purple')
  
  legend("topleft", legend = c("용산구 예측모델","aptm 예측모델"), lwd = 2 ,col = c("purple", 'red'))
  
  
  
  
  
  #중대형
  plot(nx$거래년도, predict(aptmb_model, nx),xlab= "거래년도", ylab= "예상 거래금액.평균." ,  col = 'red', cex = 2, pch = 20, ylim = c(5.0e+08, 1.9e+09), main="중대형")
  abline(aptmb_model, col = 'red')
  
  ex_model4 = lm(중대형.거래금액.평균.~거래년도, data = aptmb[aptmb$자치구.명 == "용산구",])
  abline(ex_model4, col = 'purple')
  
  legend("topleft", legend = c("용산구 예측모델","aptmb 예측모델"), lwd = 2 ,col = c("purple", 'red'))
  
  
  
  #대형
  plot(nx$거래년도, predict(aptb_model, nx),xlab= "거래년도", ylab= "예상 거래금액.평균." ,  col = 'red', cex = 2, pch = 20, ylim = c(5.0e+08, 1.5e+09), main="대형")
  abline(aptb_model, col = 'red')
  
  ex_model5 = lm(대형.거래금액.평균.~거래년도, data = aptb[aptb$자치구.명 == "동대문구",])
  abline(ex_model5, col = 'green')
  
  legend("topleft", legend = c("용산구 예측모델","aptb 예측모델"), lwd = 2 ,col = c("green", 'red'))


