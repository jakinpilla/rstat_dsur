#' ---
#' title: "ch015(비모수적 검정)"
#' author: "jakinpilla"
#' date : "`r format(Sys.time(), '%Y-%m-%d')`"
#' output: 
#'    github_document : 
#'        pandoc_args: --webtex
#'        toc : true
#' ---




#' #### 비모수적 검정은 언제 사용할까? 
#' 
#' ranking 순위화에 기초
#' 
#' 

#+ message = FALSE, warning = FALSE
library(ez)
library(ggplot2)
library(nlme)
library(pastecs)
library(reshape2)
library(WRS)
library(clinfun)
library(pgirmess)
library(car)
library(tidyverse)



#' #### 독립적인 두 조건의 비교: 윌콕스 순위합 검정 
#' 
#' Mann-Whitney test
#' 
#' Wilcoxon's rank-sum test
#'
#' 


#' #### 윌콕슨 순위합 검정의 이론 
#' 
#' 평균순위 : 1에서 그룹 크기까지의 합
#' 
#' $$ meanRank = \frac{N(N+1)}{2} $$
#' 
#' $$ W = rankSum - meanRank $$
#' 
#' W : 검정통계량, p-value, 참값 접근 방식, 정규 근사 접근방식식
#' 
#' 참값 접근 방식은 몬테카를로 방법을 이용해서 유의수준을 구한다.
#' 몬테카를로 방법은 표본과 부합하는 다수의 부분집합을 생성하되, 참가자들을 정확한 그룹에 넣는 것이 아니라 무작위로 선택한 그룹에 넣는다. 참가자들이 무작위로 그룹에 배정되므로 귀무가설이 참이다. 따라서, 귀무가설이 참인 자료에 기초해서 W값을 계산하면 된다. 
#' 
#' R은 사람들을 무작위로 배정해서 한 번만 분석하는 것이 아니라 그러한 과정을 수천번 반복하면서 귀무가설이 참일 때 나타나는 차이가 여러분의 자료에 나타난 차이만큼 큰 경우의 횟수를 센다.
#' 
#' 또다른 계산 방법은 정규 근사 접근 방식이다. W 통계량의 표집분포가 정규분포라고 가정한다. 그 가정이 성립한다면 z 값을 계산하는데 쓰이는 표준오차를 구할 수 있으며 따라서 p 값도 구할 수 있다. R은 기본적으로 표본크기가 40을 넘으면 정규 근사 접근 방식을 사용한다. 그리고 자료에 동순위 점수들이 있으면 좋든 싫든 정규 근사 접근 방식을 사용해야 한다.
#' 
#' 정규 근사 접근 방식으로 p 값을 계산할 때는 필요하다면 연속성 수정을 가할 수도 있다. R의 경우 따로 특별히 지정하지 않으면 연속성 수정을 가한다.
#' 
#' 
#' 
#' 
#' #### 자료 입력과 잠정 분석 

gl(2, 10, labels = c('Ecstacy', 'Alcohol'))

drugData <- read.delim('Drug.dat',  header = T)

drugData

#' 자가진단
#'
#' 이 자료들의 정규성 가정과 분산의 동질성 가정을 점검하라.

by(drugData[,c(2:3)], drugData$drug, stat.desc, basic=FALSE, norm=TRUE)

#' 일요일, 알코올의 경우 W= .96, 유의하지 않음 이므로 정규성을 가짐
#' 
#' 일요일, 엑스터시의 경우 W = .81, 유의함 이므로 정규분포가 아님
#' 
#' 수요일, 알코올의 경우 W = .75, 유의함이므로 정규분초가 아님
#' 
#' 수요일, 엑스터시의 경우 W = .94, 유의하지 않음 이므로 정규성을 가짐
#' 
#' 일요일 자료와 수요일 자료의 표집분포도 정규분포가 아닐 수 있다. 그러므로 비모수적 검정을 사용한다. 
#' 


leveneTest(drugData$sundayBDI, drugData$drug, center = "mean")

leveneTest(drugData$wedsBDI, drugData$drug, center = "mean")

#' 모두 유의하지 않으므로 분산의 동질성 가정이 성립한다.
#' 

# install.packages('Rcmdr', dependencies = T)
# library(Rcmdr)
#' Rcmdr를 사용하면 자료에 동순위 점수들이 있으면 참값 접근 방식을 사용할 수 없다는 점과 표분 크기가 크면 오랜 시간이 걸린다는 점은 명심하자.
#' 
#' #### R을 이용한 윌콕슨 순위합 검정 실행
#' 
sunModel <- wilcox.test(sundayBDI ~ drug, data = drugData); sunModel

wedModel <- wilcox.test(wedsBDI ~ drug, data = drugData); wedModel

#' Warning in wilcox.test.default(x = c(16L, 15L, 20L, 15L, 16L, 13L, 14L, :cannot compute exact p-value with ties
#' 
#' 기본 설정인 참값 접근 방식을 요구했지만, 동순위 점수들이 있어 그 접근방식을 사용할 수 없었다는 의미이다.
#' 
#' #### 효과크기 계산
#' 
#' 나만의 윌콕슨 순위합 검정

library(dplyr)
drugData %>% 
  filter(drug == 'Alcohol') %>% 
  dplyr::select(sundayBDI) %>%
  pull(sundayBDI) -> g1


drugData %>% 
  filter(drug == 'Ecstasy') %>% 
  dplyr::select(sundayBDI) %>%
  pull(sundayBDI) -> g2

n1 <- length(g1)
n2 <- length(g2)

w <- rank(c(g1, g2))
r1 <- w[1:n1]
r2 <- w[(n1+1):(n1+n2)]

w1 <- sum(r1)
w2 <- sum(r2)

wilc1 <- w1 - n1*(n1+1)/2
wilc2 <- w2 - n2*(n2+1)/2

wilc <- min(wilc1, wilc2)
wilc

m1 <- mean(r1)
m2 <- mean(r2)

m1
m2

#' 효과크기를 계산하는 함수를 제작한다.
#' 
rFormWilcox <- function(wilcoxModel, N) {
  z <- qnorm(wilcoxModel$p.value/2)
  r <- z / sqrt(N)
  
  cat(wilcoxModel$data.name, 'Effect Size, r = ', r)
}

#' 각 모델에 대해 이 함수를 사용한다.
#' 
rFormWilcox(sunModel, 20) 
rFormWilcox(wedModel, 20)

#' 일요일 자료의 효과크기는 작은 효과와 중간 효과의 사이이고(.3보다 작다) 수요일 자료의 효과크기는 아주 큰 효과에 해당한다.(큰 효과의 기준 .5보다 크다.)


#'
#' #### 윌콕슨 순위합 검정 결과의 보고
#' 
#' 약물 복용 다음 날(일요일)의 엑스터시 복용자의 우울증 수준(Mdn = 17.50)과 알코올 복용자의 우울증 수준(Mdn = 16.00)의 차이는 W = 35.5, p = .286, r = -.25로 유의하지 않았다. 그러나 주중(수요일)의 엑스터시 복용자의 우울증 수준(Mdn = 33.50)과 알코올 복용자의 우울증 수준(Mdn = 7.50)의 차이는 W = 4, p < .001, r = -.78 로 유의했다.