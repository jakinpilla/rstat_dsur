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
#' 통계적인 가정(정규분포, 분산의 동질성 가정 등)이 성립하지 않을 때 사용한다.
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
#' 수요일, 알코올의 경우 W = .75, 유의함이므로 정규분포가 아님
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
soyaData <- read.delim("Soya.dat", header = T)
soyaData %>% str()
#' 대조군(콩 식사 없음)을 요인의 첫 수준으로 삼는 것이 좋다. 앞에서 자료를 직접 입력할 때는 그렇게 했지만, 지금은 그렇지 않다. 순서를 다음과 같이 바꾼다.
#' 

soyaData %>%
  mutate(Soya = case_when(
    Soya == 1 ~ 'No Soya',
    Soya == 2 ~ '1 Soya Meals',
    Soya == 3 ~ '4 Soya Meals',
    Soya == 4 ~ '7 Soya Meals'
  )) -> soyaData

soyaData$Soya <- factor(soyaData$Soya, 
                        levels = c('No Soya', '1 Soya Meals', '4 Soya Meals', '7 Soya Meals'))
soyaData %>% str()
soyaData$Soya

#' 분포의 정규성 검정 결과를 살펴보자. 
by(soyaData$Sperm, soyaData$Soya, stat.desc, basic=FALSE, norm=TRUE)

#' 콩을 먹지 않은 그룹은 $W(20) = .805, p = .001$, 매주 1회 그룹은 $W(20) = .826, p = .002$, 
#' 매주 4회 그룹은 $W(20) = .826, p = .002$, 매주 4회 그룹은 $W(20) = .743, p < .001$로 유의하다.
#' 즉, 정규분포로부터 벗어나 있다.
#' 
leveneTest(soyaData$Sperm, soyaData$Soya)

#' $F(3, 76) = 2.86, p = .042$ 동질성 가정이 위반되었다.
#' 
#' #### R을 이용한 크러스컬-윌리스 검정 실행
#' 
kruskal.test(Sperm ~ Soya, data = soyaData)


#' 각 그룹의 평균 순위를 구해 구면 크러스컬-윌리스 검정을 해석하는데 도움이 된다. 
#' 
#' 
soyaData$Ranks <- rank(soyaData$Sperm)


by(soyaData$Ranks, soyaData$Soya, mean)

soyaData %>%
  group_by(Soya) %>%
  summarise(mean_sperm  = mean(Sperm))


soyaData %>%
  ggplot(aes(Soya, Sperm)) +
  geom_boxplot()

#' #### 크러스컬-윌리스 검정을 위한 사후검정
#'
#' 비모수적 사후 검정을 수행하는 한 가지 방법은 모든 가능한 비교에 대해 윌콕슨 순위합 검정을 수행하는 것이다. 이 방법은 서로 다른 그룹의 평균 순위와의 차이를 어떤 임계값과 비교하는 것이다.  그 임계값은 z 점수(수행하는 비교 횟수에 맞게 수정된)와 전체 표본 크기, 그리고 비교하는 두 그룹의 표본 크기에 기초한 상수로 계산한다. 
#' 
#' $$ |\overline{R}_{u}} - \overline{R}_{v}| \ge $$
#' 
#' 
#' $$ z_{\alpha/k(k-1)} \sqrt{ \frac{N(N + 1)}{12} \left( \frac{1}{n_{u}} + \frac{1}{n_{v}} \right)} $$
#' 
#' $|\overline{R}_{u}} - \overline{R}_{v}|$은 그냥 비교하는 두 그룹의 평균 순위 차이의 크기이다. 
#' 
#' 우변에서 k는 그룹의 수이고, N은 전체표본크기이다. $n_{u}$는 비교하는 첫 그룹의 참가자 수이고 $n_{v}$는 둘째 그룹의 참가자 수이다.
#' 
#' $\alpha$는 통상 0.05이고 그룹의 수 k는 4이므로   $\alpha/k(k-1) = 0.05 / 4(4-1) = .00417$ 이다.
#' 
#' $z_{.00417}$의 의미는 모든 z중 자신보다 큰 값의 비율이 .00417인 z 값의 의미이므로 정규분포 확률표를 참고하면 2.64를 근사값으로 얻을 수 있다.
#' 
#' 만일 두 그룹의 평균 순위 차이의 크기가 임계 차이보다 크면, 두 그룹의 차이는 유의한 것이다. 
#' 
#' 이 모든 계산을 해 주는 `kruskalmc()` 함수를 이용한다. 
#' 
library(pgirmess)
kruskalmc(Sperm ~ Soya, data = soyaData)

#' 결과 중 difference 열이 TRUE이면 차이가 유의한 것이고, FALSE이면 유의하지 않은 것이다.
#' 
#' 결과적으로 지금 예제에서는 모든 차이가 임계 차이보다 작으므로, 그 열이 모두 FALSE이고 즉 모든 차이가 유의하지 않은 것이 되었다.
#' 
#' 하지만 이처럼 모든 그룹을 다른 모든 그룹과 비교할 때는 차이들의 유의성이 너무 엄격하게 판정된다. 그렇지 않으면 제1종 오류율이 상승하기 때문이다. 특정 그룹들만 집중해서 비교한다면 이러한 문제점을 완화할 수 있다.
#' 
#' 대조군은 'No Soya'이므로 이것을 기준으로 다음과 같이 비교한다.
#' 
#'  - 검정1: 'No Soya' vs '1 Soya Meals'
#'  
#'  - 검정2: 'No Soya' vs '4 Soya Meals'
#'  
#'  - 검정3: 'No Soya' vs '7 Soya Meals'
#'  
#'  이런 검정은 6회가 아니라 3회만 수행되므로 모든 결과가 덜 엄격히 판정된다. 그리고 이는 `kruskalmc()` 함수에 `cont` 옵션을 지정하기만 하면 자동으로 실현된다.
#'  
#'  cont 옵션은 one-tailed, two-tailed 두 가지 중 택일한다.
#'  
kruskalmc(Sperm ~ Soya, data = soyaData, cont = 'two-tailed')

#' 이번에는 'No Soya' 대 '7 Soya Meals' 그룹의 차이가 유의하게 나왔다. 이번 검정에서는 비교가 3회 뿐이라서 임계 차이를 계산할 때 그 세 검정의 평균순위들만 쓰여서 임계 차이가 적어졌기 때문이다.
#' 
#' 이 예는 맹목적으로 모든 그룹을 모든 그룹과 비교하는 것보다는 적절한 그룹들을 선택해서 집중적으로 비교하는 것이 더 낫다는 점을 잘 보여준다.
#'
#' #### 추세검정 : 용크헤이러-테르프스크라 검정
#' 
#' 용크헤이러-테르프스크라 검정은 비교하는 그룹들의 중앙값들에 어떤 순서 있는 패턴이 존재하는지를 검사한다. 
#' 
#' 이 검정은 그룹들의 중앙값들이 부호화 변수에 지정된 수준들의 순서를 기준으로 특정한 순서(오름차순인지 또는 내림차순)를 따르는지의 여부를 판정한다. 
#' 
#' 지금 예에서는 정자 수 중앙값들이 그룹들에 따라 증가 또는 감소하는 순서인지 판정한다.
#' 

jonckheere.test(soyaData$Sperm, as.numeric(soyaData$Soya))

#' JT는 검정통계량이다. 표본이 클 때는 이 검정통계량의 표집분포가 정규분포이고 그 평균과 표준편차를 손쉽게 정의, 계산할 수 있다. (JT = 912, Mean = 1.200, sd = 116.33)
#' 
#' 
#' p < .05이므로 통계적으로 유의한 추세가 존재한다. 즉 콩 식품 섭취량이 증가할 수록 정자 수가 감소한다.
#' 
#' #### 효과크기 계산
#' 
#' 자유도가 2 이상인 카이제곱 통계량을 손쉽게 효과크기 r로 변환하는 방법은 없다. 
#' 
#' #### 크리스컬-월리스 검정 결과의 보고
#' 
#' - 콩 식품 섭취는 정자 수에 유의하게 영향을 미쳤다. $H(3) = 8.66, p = .034$
#' 
#' - 그룹들 사이의 집중된 평균 순위 비교에 따르면, 콩 식품을 전혀 먹지 않았을 때에 비해 매주 1회 또는 4회 먹었을 때의 정자 수 차이는 유의하지 않았다. 그러나 콩 식품을 전혀 먹지 않았을 때에 비해 매주 7회 먹었을 때는 정자수가 유의하게 감소했다. 
#' 
#' - 만일 콩 식품을 매일 먹는다면 정자수가 유의하게 감소하지만, 그 보다 적게 먹는다면 유의한 차이가 생기지는 않을 것이다.
#' 
#' - 융크헤이러 검정에 따르면, 자료에 유의한 추세가 존재한다. 콩 식품을 먹을수록 정자 수 중앙값이 감소했다. $J = 912, p = .013$
#' 
#' 
#' 
#' #### 핵심정리
#' 
#' - 크러스컬-월리스 검정은 여러 조건을 비교한다. 이 검정은 각 조건에서 서로 다른 참가자들을 측정한 자료가 일원 독립 분산분석의 어떤 가정을 위반할 때 쓰인다.
#' 
#' - p 값으로 유의성을 판정한다. 만일 그 값이 .05보다 작으면 유의한 것이다.
#' 
#' - 주 분석 이후에 사후검정을 수행할 수 있다. 가능하면 집중된 비교를 수행하는 것이 이상적이다. 검정함수의 출력에서 difference 열이 TRUE이면 해당 비교의 그룹들이 유의하게 다른 것이다.
#' 
#' - 그룹들에 따라 평균들이 오름차순 또는 내림차순으로 나열될 것으로 예측한다면, 용크헤이러 추세 검정으로 그것을 확인할 수 있다.
#' 
#' - 결과 보고시 주 분석의 H 통계량과 자유도, 유의확률을 보고한다. 또한, 중앙값들과 해당 범위도 보고한다. 
#' 
#' #### 연관된 여러 그룹의 비교: 프리드먼 분산분석
#' 
#' 필수가정을 위반한 반복측정 자료에 대한 또 다른 대안이 존재한다. 바로 프리드먼 분산분석이 그것이다.
#' 
#' 조건이 둘 이상이고 모든 조건에서 같은 참가자들이 쓰인 상황에서 조건들의 차이를 비교하는데 쓰인다.
#' 
#' #### 프리드먼 분산분석의 이론
#'
#'
#' $$ F_{r} = \left[ \frac{12}{Nk(k+1)}  \sum_{i = 1}^{k} R^2 _{i} \right] - 3N(k+1) $$
#' 
#' 참가자들이 많으면(대략 10명 이상) 이 검정통계량은 카이제곱 분포를 따른다. 카이제곱 분포에는 자유도마다 하나의 값이 있는데, 여기서 자유도는 그룹 수에서 1을 뺀 것이다.
#' 
#' #### 자료입력과 잠정분석
#' 
dietData <- read.delim('Diet.dat', header = T)

dietData

stat.desc(dietData, basic = FALSE, norm = TRUE)


#' #### R을 이용한 프리드먼 분산분석 실행
#' 
#' 프리드먼 분산분석 함수는 `friedman.test()` 이다. 이 함수는 행렬을 요구하고 주어진 행렬에 있는 모든 변수를 계산에 포함한다. 따라서 분석과 무관한 변수는 미리 제거해야 한다.
#' 
#' 또한 이 함수는 결측값을 처리하지 못한다. 
#' 
friedman.test(as.matrix(dietData))

#' #### 프리드먼 분산분석 결과
#' 
#' p = .9048 이므로  유의하지 않다. 즉, 앤디 킨스 다이어트에 효험이 있다는 증거를 발견하지 못했다.
#' 
#' #### 프리드먼 분산분석의 사후검정
#' 
#' friedmanmc() 함수로 사후검정을 할 수 있다. 
#' 
friedmanmc(as.matrix(dietData))

#' difference 열을 보아야 하는데 TRUE이면 유의한 것이고 FALSE 이면 유의하지 않은 것이다.
#' 
#' #### 프리드먼 분산분석 결과의 보고
#' 
#' - 2개월의 다이어트 기간에서 참가자들의 몸무게가 유의하게 변하지는 않았다. $\chi^2(2) = 0.20, p > .05$ 이다.
#' 
#' - 본페로니 수정값을 이용해서 사후검정을 수행했다. 그 결과에 따르면 다이어트 시작 시점과 1개월 후의 몸무게 변화와 시작 시점과 2개월 후의 몸무게 변화, 그리고 1개월 후와 2개월 후의 몸무게 변화는 모두 유의하지 않았다.
#' 
#' 