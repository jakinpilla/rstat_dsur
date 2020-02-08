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
#' W : 검정통계량, p-value, 참값 접근 방식, 정규 근사 접근방식
#' 
#' **참값 접근 방식**은 몬테카를로 방법을 이용해서 유의수준을 구한다.
#' 몬테카를로 방법은 표본과 부합하는 다수의 부분집합을 생성하되, 참가자들을 정확한 그룹에 넣는 것이 아니라 무작위로 선택한 그룹에 넣는다. **참가자들이 무작위로 그룹에 배정되므로 귀무가설이 참이다.** 따라서, 귀무가설이 참인 자료에 기초해서 W값을 계산하면 된다. 
#' 
#' R은 사람들을 무작위로 배정해서 한 번만 분석하는 것이 아니라 그러한 과정을 수천번 반복하면서 귀무가설이 참일 때 나타나는 차이가 여러분의 자료에 나타난 차이만큼 큰 경우의 횟수를 센다.
#' 
#' 또다른 계산 방법은 **정규 근사 접근 방식**이다. **W 통계량의 표집분포가 정규분포라고 가정한다. 그 가정이 성립한다면 z 값을 계산하는데 쓰이는 표준오차를 구할 수 있으며 따라서 p 값도 구할 수 있다.** R은 기본적으로 표본크기가 40을 넘으면 정규 근사 접근 방식을 사용한다. 그리고 자료에 동순위 점수들이 있으면 좋든 싫든 정규 근사 접근 방식을 사용해야 한다.
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
rFromWilcox <- function(wilcoxModel, N) {
  z <- qnorm(wilcoxModel$p.value/2)
  r <- z / sqrt(N)
  
  cat(wilcoxModel$data.name, 'Effect Size, r = ', r)
}

#' 각 모델에 대해 이 함수를 사용한다.
#' 
rFromWilcox(sunModel, 20) 
rFromWilcox(wedModel, 20)
rFromWilcox(wedModel, 20)

#' 일요일 자료의 효과크기는 작은 효과와 중간 효과의 사이이고(.3보다 작다) 수요일 자료의 효과크기는 아주 큰 효과에 해당한다.(큰 효과의 기준 .5보다 크다.)


#'
#' #### 윌콕슨 순위합 검정 결과의 보고
#' 
#' 약물 복용 다음 날(일요일)의 엑스터시 복용자의 우울증 수준(Mdn = 17.50)과 알코올 복용자의 우울증 수준(Mdn = 16.00)의 차이는 W = 35.5, p = .286, r = -.25로 유의하지 않았다. 그러나 주중(수요일)의 엑스터시 복용자의 우울증 수준(Mdn = 33.50)과 알코올 복용자의 우울증 수준(Mdn = 7.50)의 차이는 W = 4, p < .001, r = -.78 로 유의했다.
#' 
#' #### 연관된 두 조건의 비교: 윌콕슨 부호순위 검정
#' 
#' Wilcoxon signed-rank test
#' 
#' 두 가지 점수 집합을 비교하되, 그 점수들을 같은 참가자들에게서 측정한 상황에 쓰임. 일종의 종속 t 검정의 비모수적 버전에 해당한다. 
#' 
#' 
#' 자가진단 : 일요일에 대한 수요일의 BDI 점수 차이(변화량)을 계산하고, 이 변화량들의 정규성을 알코올 그룹과 엑스터시 그룹 각각 검사하라.

drugData %>% 
  mutate(diff = wedsBDI - sundayBDI) -> drugData_1

by(drugData_1$diff, drugData_1$drug, stat.desc, basic=FALSE, norm=TRUE)

#' #### 월콕슨 부호순위 검정의 이론
#' 
#' 만일 차이가 0이면 그 점수는 순위화에서 제외한다. 0이 아니면 그 부호를 기록해두되, 순위는 부호를 무시하고 차이의 크기(절대값)로 결정한다. 
#' 
#' 모든 점수 차이를 매긴 후에는, 양의 순위들의 합과 음의 순위들의 합을 따로 구한다.
#' 
#' 양, 음 순위합 중 더 작은 것이 최종적인 검정통계량 T가 된다.
#' 
#' 엑스터시 그룹의 경우 $T_{+} = 36$이고 $T_{-} = 0$이다. 그래서 최종 T는 0이다.
#' 
#' 알코올 그룹의 경우 $T_{+} = 8$이고 $T_{-} = 47$이다. 그래서 최종 T는 8이다.
#' 
#' 
#' 검정통계량 T의 유의확률은 이전처럼 평균($\overline{T}$)과 표준오차($SE_{\overline{T}}$)로 구한다.  이전 절의 윌콕슨 순위합 검정에서처럼, 평균과 표준오차 둘 다 표본크기 n의 함수이다.
#' 
#' $$\overline{T} = \frac{n(n+1)}{4}$$
#' 
#' $$SE_{\overline{T}} = \sqrt{\frac{n(n+1)(2n+1)}{24}}$$
#' 
#' 
#' 엑스터시 그룹에서는 차이가 0인 두 점수를 배재했었다. 따라서 n은 10이 아니라 8이어야 한다.
#' $$\overline{T}_{Ecstasy} = \frac{8(8+1)}{4} = 18$$
#' $$SE_{\overline{T}_{Ecstasy}} = \sqrt{\frac{8(8+1)(16+1)}{24}} = 7.14$$
#' 
#' 알코올 그룹에서는 점수가 제외된 참가자가 없었으므로 10을 그대로 사용한다.
#' $$\overline{T}_{Alcohol} = \frac{10(10+1)}{4} = 18$$
#' $$SE_{\overline{T}_{Alcohol}} = \sqrt{\frac{10(10+1)(20+1)}{24}} = 9.81$$
#' 
#' 검정통계량과 검정통계량의 평균, 그리고 표준오차를 알고 있으면 검정통계량을 z 점수로 손쉽게 변환할 수 있다. 
#' 
#' $$z = \frac{X - \overline{X}}{s} = \frac{T - \overline{T}}{SE_{\overline{T}}}$$
#' 
#' 여기에 각각 수치를 대입하여 각각의 z점수를 구해보자.
#' 
#' $$z_{Ecstasy} = \frac{T - \overline{T}}{SE_{\overline{T}}} = \frac{0-18}{7.14}  = -2.52$$
#' 
#' $$z_{Alcohol} = \frac{T - \overline{T}}{SE_{\overline{T}}} = \frac{8-27.5}{9.81}  = -1.99$$
#' 
#' 만일 z 점수의 크기가 1.96보다 크면 해당 검정은 p < .05 수준에서 유의한 것이다. 엑스터시 그룹과 알코올 그룹 모두, 수요일과 일요일의 우울증 점수의 차이는 유의하다.
#' 
#' #### R을 이용한 검정 실행
#' 
#' 자가진단: 데이터프레임의 점수들을 약물종류에 따라 분리하라

drugData %>%
  filter(drug == 'Ecstasy') -> ecstasyData


drugData %>%
  filter(drug == 'Alcohol') -> alcoholData


#' `윌콕스 부호순위 검정`을 수행한다. paired = TRUE 옵션을 지정하지 않으면 R은 `윌콕슨 순위합 검정`을 실시하므로 주의하자.
#' 
alcoholModel <- wilcox.test(alcoholData$wedsBDI, alcoholData$sundayBDI, paired = T, 
                            correct = F)

alcoholModel

ecstasyModel <- wilcox.test(ecstasyData$wedsBDI, ecstasyData$sundayBDI, paried = T, 
                            correct = FALSE)

ecstasyModel

#' #### 윌콕슨 부호순위 검정 결과의 해석
#' 
#' 중앙값들에 근거할 때,
#' 
#' 알코올의 경우,  `V = 8`은 앞에서 설명한 $T_{+}$ 값에 해당한다. `p = .047`로 유의하다. 즉 알코올을 복용한 경우, 일요일(복용 다음날) 보다 주중의 우울증 점수가 유의하게 더 낮다.
#' 
#' 엑스터시의 경우, `p = 0.001426`로 유의하다. 즉, 엑스터시를 복용한 경우 일요일(복용 다음날 보다)보다 주중의 우울증 점수가 더 유의하게 높다. 
#' 
#' 두 그룹의 결과를 비교해보면, 우울증에 대한 알코올 복용과 엑스터시 복용의 효과가 정반대임을 알 수 있다. 알코올을 먹으면 그 다음날은 약간 우울해지지만, 주중이 되면 우울증 수준이 떨어진다. 
#' 
#' 엑스터시를 먹으면 그 다음날 우울증이 심해지지만, 알코올과는 달리 주중이 되면 우울증 수준이 오히려 더 높아진다. 같은 참가자들로 이루어진 그룹들의 이러한 반대되는 효과를 상호작용이라고 한다.
#' 
#' #### 효과크기의 계산
#' 
#' 이전에 만들어 둔 rFromWilcox() 함수를 재활용할 수 있다.
#' 
#' 각 그룹의 참가자는 10명이지만, 그 사람들을 두 번 측정했기 때문에 관측값은 20개이다.
#' 
rFromWilcox(alcoholModel, 20)

rFromWilcox(ecstasyModel, 20)


#' 우울증 수준 변화에 대한 알코올 복용 효과는 중간 효과와 큰 효과 사이이다.
#' 
#' 우울증 수준 변화에 대한 엑스터시 복용의 효과는 큰 효과이다.
#' 
#' #### 윌콕슨 부호순위 검정 결과의 보고
#' 
#' 엑스터시 복용자의 경우, 수요일의 우울증 수준(Mdn = 33.50)이 일요일의 우울증 수준(Mdn= 17.50)에 비해 p = .047, r = -.56으로 유의하게 높았다. 그러나 알코올 복용자는 그 반대였다. 알코올 복용시에는 수요일의 우울증 수준(Mdn = 7.50)이 일요일의 우울증 수준(Mdn = 16.00)에 비해 p = .012, r = -.45로 유의하게 낮았다.
#' 
#' 
#' #### 윌콕슨 순위검정 요약
#' 
#'  - 윌콕슨 부호순위 검정은 두 조건을 검사한다. 이 검정은 각 조건에 같은 참가자들이 참여하되 그 자료가 종속 t 검정의 어떤 가정을 위반할 때 쓰인다.
#'  
#'  - 이 검정에서도 p 값으로 유의성을 판정한다. 만일 그 값이 .05보다 작으면 두 그룹의 차이가 유의한 것이다.
#'  
#'  - 결과를 보고할 때는 유의확률을 명시한다. 또한 중앙값과 해당 범위들도 보고하는 것이 좋다.
#'  
#' #### 여러독립 그룹의 차이: 크러스컬-윌리스 검정
#'  
#' #### 크러스컬-윌리스 검정의 이론
#'  
#' 순위화된 자료에 기초한다. 순위들을 매긴 후 점수들을 그룹별로 모아서 각 그룹의 순위들을 모두 더한다.
#' 
#' 그러한 순위합을 $R_{i}$라고 표기한다.(i는 특정그룹을 식별하는 색인임)
#'  
#' $$H = \frac{12}{N(N + 1)} \sum_{i=1}^{k} \frac{R^2_{i}}{n_{i}} - 3 \left(N + 1 \right) $$
#'
#' $$H = \frac{12}{80(81)} \left( \frac{927^2}{20} + \frac{883^2}{20} + \frac{883^2}{20} + \frac{547^2}{20} \right) - 3 \left( 81 \right)$$
#' 
#' 이 검정통계량은 카이제곱 분포를 따른다. 자유도는 그룹 수에서 1을 뺀 것이다.(k - 1)
#' 
#' #### 자료 입력과 잠정 분석
#' 
