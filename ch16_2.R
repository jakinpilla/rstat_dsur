#' ---
#' title: "ch016_2(다변량분산분석)"
#' author: "jakinpilla"
#' date : "`r format(Sys.time(), '%Y-%m-%d')`"
#' output: 
#'    github_document : 
#'        pandoc_args: --webtex
#'        toc : true
#'        toc_depth : 5
#' ---


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
# install.packages('mvoutlier')
library(mvoutlier)


#' ### R을 이용한 다변량분산분석
#' 
#' #### 자료입력
#' 
ocdData <- read.delim('OCD.dat', header = T)


ocdData$Group <- factor(ocdData$Group, levels = c('CBT', 'BT', 'No Treatment Control'), 
                        labels = c('CBT', 'BT', 'NT'))


ocd_scatter <- ggplot(ocdData, aes(Actions, Thoughts))
ocd_scatter + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~ Group, ncol = 3) +
  labs(x = "Number of Obession-Related Behaviours", y = "Number of Obsession-Related Thoughts")


ocdData %>%
  gather(type, value, -Group) -> molten_ocd_data 

molten_ocd_data %>% str()

molten_ocd_data %>%
  ggplot(aes(Group, value, fill = type)) +
  stat_summary(fun.y = mean, geom = 'bar', position = 'dodge') +
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', position = position_dodge(width = .90), width = .2) +
  scale_y_continuous(breaks = seq(0, 20, by = 2))

#' 강박관련 행동(Actions)의 경우, CBT와 NT에 비해 BT 그룹의 평균 횟수가 적다. 
#' 
#' 강박관련 생각(Thoughts)의 경우, BT와 NT에 비해 CBT 그룹의 평균 횟수가 적다.
#'  

molten_ocd_data %>% 
  as_tibble() %>%
  ggplot(aes(Group, value, colour = type)) +
  geom_boxplot()

#' 기술통계량을 살펴본다.
#' 
by(ocdData$Actions, ocdData$Group, stat.desc, basic = F)

by(ocdData$Thoughts, ocdData$Group, stat.desc, basic = F)

#' 가정들을 점검하자. 공분산행렬의 동질성을 점검해주는 함수가 따로 있지는 않다. 행렬의 성분들을 보고 직접 판단해야 한다. 
#' 
by(ocdData[, 2:3], ocdData$Group, cov)

#' 행렬의 대각성분은 각 결과변수의 분산이고, 비대각성분들은 공분산이다. 
#' 
#' 행동횟수의 분산들은 각 그룹마다 1.43, 3.12, 1.11이다.
#' 
#' 생각회수의 분산들은 각 그룹마다 각각 3.60, 4.40, 5.56이다.
#' 
#' 공분산들은 각 그룹마다 .04, 2.51, -1.11이다. 이는 생각 횟수와 행동 횟수의 관계가 그룹마다 다르다는 사실을 반영한다.
#' 
#' 전체적으로 볼때 행렬들이 그룹에 따라 다름을 암시하는 증거가 존재한다.
#' 
#'  만일 그룹 크기들이 다르다면 다음 두 사항을 염두에 두어야 한다.
#'   - (1) 만일 큰 표본들에서 큰 분산과 공분산이 나오면, 유의확률은 보수적이 된다.
#'   - (2) 작은 표본들에서 큰 분산과 공분산이 나오면 유의확률이 느슨해지므로, 다변량분산분석 결과의 유의한 차이를 신중하게 취급하는 것이 바람직하다.
#'   
#'   다변량 정규성은 `mvnormtest::mshapiro.test()`로 점검할 수 있다.
#'

# install.packages('mvnormtest')
library(mvnormtest)
ocdData$Group %>% unique()
#'            
ocdData %>%
  filter(Group == 'CBT') %>%
  select(Actions, Thoughts) %>%
  t() -> cbt; cbt


ocdData %>%
  filter(Group == 'BT') %>%
  select(Actions, Thoughts) %>%
  t() -> bt; bt

ocdData %>%
  filter(Group == 'NT') %>%
  select(Actions, Thoughts) %>%
  t() -> nt; nt

mshapiro.test(cbt)
mshapiro.test(bt)
mshapiro.test(nt)

#' CBT 그룹(p  = .777)과 BT 그룹(p = .175)은 유의하지 않으므로 다변량정규성에서 문제가 없다. 그러나 NT 그룹(p = .03)의 자료는 다변량정규성에서 유의하게 벗어났다.
#' 
#' `aq.plot()` 함수로 다변량 이상치들을 살펴볼 수 있다.
#' 
library(mvoutlier)
aq.plot(ocdData[, 2:3])

#' 오른쪽 위 그래프를 제외한 세 그래프에서 빨간색 변호를 찾아봐야 한다. 네 그림 모두 26번 사례를 이상치로 제시한다.
#' 
#' #### 대비설정
#' 
#' 지금 자료에서 비치료 대조군은 마지막 범주로 부호화되어 있으므로, 다음과 같이 대비를 설정한다.
#' 
contrasts(ocdData$Group) <- contr.treatment(3, base = 3)

#' 혹은 각 대비에 의미있는 이름을 부여하며 직접 대비를 설정할 수 있다.
#' 
CBT_vs_NT <- c(1, 0, 0)
BT_vs_NT <- c(0, 1, 0)
contrasts(ocdData$Group) <- cbind(CBT_vs_NT, BT_vs_NT)
ocdData %>% str()


#' #### 다변량분산분석 모형
#' 
#' 모형 서술에 여러개의 결과변수를 지정하려면 그 변수들을 cbind() 함수를 이용해 하나의 객체로 묶어야 한다.
#' 
outcome <- cbind(ocdData$Actions, ocdData$Thoughts)

ocdModel <- manova(outcome ~ Group, data = ocdData)

#' 필라이 대각합
summary(ocdModel, intercept = T) # 유의하다

#' 윌크스 람다
summary(ocdModel, intercept = T, test = 'Wilks') # 유의하다

#' 호텔링 대각합
summary(ocdModel, intercept = T, test = 'Hotelling') # 유의하지 않다.

#' 로이의 최대근
summary(ocdModel, intercept = T, test = 'Roy') # 유의하다

#'표본 크기가 같을때의 필라이 대각합의 강건성을 생각해보면, 필라이 대각합이 말해주는 결론을 믿는 것이 바람직하다.
#'
#' 그러므로 치료의 종류가 강박 장애에 유의한 효과가 있었다고 결론지어야 할 것이다.
#' 
#' 하지만 다변량 검정통계량은 어떤 그룹이 어떤 그룹과 다른지는 말해주지 않는다. 그리고 치료의 효과가 강박 관련 생각에 효과가 있었는지, 강박 관련 행동에 효과가 있었는지, 또는 둘의 조합에 효과가 있었는지 말해주지도 않는다. 효과의 본성을 파악하려면 일변량 검정통계량을 수행해봐야 한다.
#' 
#' #### 사후분석: 일변량 검정통계량
#' 
summary.aov(ocdModel)

#' 비치료 그룹에 비한 치료 그룹들의 차이는 강박 관련 행동에서나 강박 관련 생각에서나 유의하지 않았다. 이 두 결과에 따르면, 환자들이 겪는 강박 장애의 수준에 대한 치료 종류의 효과가 유의하지 않았다고 결론지어야 할 것이다.
#' 
#' 종속변수들의 상호작용 방식을 파악하려면 잠시 후에 설명할 판별함수 분석이란는 것을 수행해야 한다.
#' 
#' #### 대비 결과
#' 
actionModel <- lm(Actions ~ Group, data = ocdData)
thoughtModel <- lm(Thoughts ~ Group, data = ocdData)

summary.lm(actionModel)

summary.lm(thoughtModel)

#' 이전에 대비들을 직접 설정한 덕분에, 출력에 의미있는 대비 이름들이 표시되었다.(GroupCBT_vs_NT, GroupBT_vs_NT)
#' 
#' GroupCBT_vs_NT의 경우, 생각회수수의 차이와 행동횟수의 차이가 모두 유의하지 않다.
#' 
#' GroupBT_vs_NT의 경우, 생각횟수의 차이는 유의하지 않지만 행동 횟수의 차이는 유의하다. 이는 다소 의외의 결과이다. 행동 횟수에 대한 일변량분산분석이 유의하지 않았으므로 그룹들이 다르지 않을 것이라고 기대했는데, 그와는 반대의 결과가 나왔다.
#' 
#' #### 핵심정리
#' 
#' - 다변량분석은 여러 종속변수를 동시에 고려해서 그룹들의 차이를 검사할 때 쓰인다.
#' 
#' - 다변량분산분석은 다변량정규성과 공분산행렬의 동질성을 가정한다. 표본 크기들이 같을 때는 후자의 가정을 무시해도 된다. 그런 경우 일부 다변량분산분석 검정통계량들이 그 가정의 위반에 대해 강건하기 때문이다. 다변량 정규성은 샤피로-윌크 검정의 다변량 버전으로 점검할 수 있다. 
#' 
#' - 다변량분산분석에 사용할 수 있는 검정통계량은 네 가지이다. (필라이 대각합, 윌크스 람다, 호텔링 대각합, 로이의 최대근). 그 검정 결과가 유의하면 종속변수에 대한 그룹들의 차이가 유의한 것이다. 
#' 
#' - 다변량 분석 이후에 일변량분산분석으로 결과를 좀 더 분석할 수 있다. 더 나아가서, 각 분산분석을 대비를 이용해서 좀 더 분석할 수도 있다. 
#' 
#' ####  강건한 다변량분산분석
#' 
ocdData %>% as_tibble()

#' - `mulrank()`: 이 함수는 뮌첼과 브루너의 방법을 이용해서 순위화한 자료에 대해 다변량분산분석을 수행한다.
#' 
#' - `cmanova()`: 이 함수는 순위화한 자료에 대해 최와 마든의 강건한 검정을 수행한다. 이것을 크러스컬-윌리스 검정의 한 형태이다. 
#' 
#' 위 함수들은 넓은 형식의 자료를 요구한다.
#' 
ocdData$row <- rep(1:10, 3)

ocdData %>% str()

ocdData %>%
  gather(Outcome_Measure, Frequency, -c(Group, row)) %>%
  mutate(group_outcome_measure = paste0(Group, '_', Outcome_Measure)) %>%
  mutate(group_outcome_measure = factor(group_outcome_measure, 
                                        levels = c('CBT_Actions', 'CBT_Thoughts',
                                                   'BT_Actions', 'BT_Thoughts',
                                                   'NT_Actions', 'NT_Thoughts'))) %>%
  select(row, group_outcome_measure, Frequency) %>%
  spread(group_outcome_measure, Frequency) %>%
  select(-row) -> ocdRobust


ocdRobust

#' 점수들은 총 여섯 그룹인데, 우선은 참가자가 받은 치료의 종류에 따라 세 그룹(CBT, BT, NT)으로 나뉘고, 각 그룹 안에서 측정한 결과이 종류에 따라 두 그룹으로 나뉜다.

mulrank(3, 2, ocdRobust)

mulrank(3, 2, ocdRobust)$q.hat -> q_hat

rownames(q_hat) <- c('CBT', 'BT', 'NT')
colnames(q_hat) <- c('Actions', 'Thoughts')

q_hat


#' BT 그룹은 행동 횟수의 순위(.38)가 생각 횟수의 순위(.59)보다 낮고, CBT 그룹은 그 반대이다. 즉, 생각 횟수의 순위(.37)가 행동 횟수의 순위(.55)보다 낮다.
#' 
#' 즉, CBT는 행동보다 생각에 더 큰 영향을 미쳤고, BT는 생각보다 행동에 더 큰 영향을 미쳤다. 그러나 전반적인 효과는 유의하지 않았다.
#' 

cmanova(3, 2, ocdRobust)

#' $H(4) = 9.06, p = .060$으로 유효하지 않았다.
#' 
#' #### 다변량분산분석 결과의 보고
#' 
summary(ocdModel, intercept = T) 

#'강박 관련 생각과 행동 횟수에 대한 치료의 효과는 유의했다. $F(4, 54) = 2.65, p < .05$이다.
#'
#' - 필라이 대각합으로 판단할 때, 강박 관련 생각과 행동 횟수에 대한 치료의 효과는 유의했다. $V = .32, F(4, 54) = 2.56, p < .05$이다.
#' 
#' - 윌크스 람다 통계량으로 판단할 때, 강박 관련 생각과 행동 횟수에 대한 치료의 효과는 유의했다. $\Lambda = .70, F(4, 52) = 2.56, p < .05$이다.
#' 
#' - 호텔링 대각합으로 판단할 때, 강박 관련 생각과 행동 횟수에 대한 치료의 효과는 유의하지 않았다. $T = .41, F(4, 50) = 2.55, p > .05$이다.
#' 
#' - 로이의 최대근으로 판단할 때, 강박 관련 생각과 행동 횟수에 대한 치료의 효과는 유의했다. $\Theta = .35, F(2, 27) = 4.52, p < .05$이다.


summary.aov(ocdModel)


#' - 필라이 대각합으로 판단할 때, 강박 관련 생각과 행동 횟수에 대한 치료의 효과는 유의했다. $V = .32, F(4, 54) = 2.56, p < .05$이다. 그러나 결과변수들에 대한 개별적인 일변량분산분석에 따르면 강박 관련 `행동`에 대한 치료의 효과는 $F(2, 27) = 2.77, p > .05$로 유의하지 않았고, `생각`에 대한 치료효과도 $F(2, 27) = 2.15, p > .05$로 유의하지 않았다.
#' 
#' - 뮌첼과 브루너의 방법을 R에서 구현한 `mulrank()` 함수를 이용해서 순위화된 자료에 대해 다변량분산분석을 수행했다. 강박 장애 측정 결과에 대한 치료 종류의 주 효과는 $F = 1.64, p = .168$로 유의하지 않았다.
#' 
#' - 최와 미든의 방법을 R에서 구현한 `cmanova()` 함수를 이용해서 순위화된 자료에 대해 다변량분산분석을 수행했다. 강박 장애 측정 결과에 대한 치료 종류의 주 효과는 $H(4) = 9.06, p = .060$으로 유의하지 않았다.
#' 
#' #### 다변량분산분석에 대한 후속 분석으로서의 판별분석
#' 
#' 판별함수 분석 : Discriminant function analysis
#' 
#' 판별분석에서는 여러 예측변수를 기준으로 여러 그룹을 서로 분리하는(판별하는) 가장 좋은 방법이 무엇인지 살펴본다. 어떤 의미로 이는 다변량분산분석을 거꾸로 적용하는 것과 같다.
#' 
#' 다변량분산분석에서는 그룹들을 가장 잘 분리하는 선형 변량을 식별해서 그룹을 예측한다. 그 `선형 변량`이 바로 판별함수 분석의 `함수`이다.
#' 
#' lda() 함수의 `prior = prior probability` 옵션의 경우 그룹들의 표본 크기가 같을 때는 생략해도 된다.
#' 
#' 만약 그룹 크기들이 서로 다르면, 각 그룹의 사전확률을 해당 크기에 맞추어 계산해서 prior 옵션에 넣어주어야 한다. 
#' 
#' $$prior = c(n_{1}, n_{2}, n_{3}) / N$$
#' 
#' 
ocdDFA <- lda(Group ~ Actions + Thoughts, data = ocdData)

ocdDFA

#' 출력 결과에서 주목할 것은 Coefficients of linear discriminants 이다. 이는 선형 판별함수의 계수들이다. 이 값들은 이전에 구한 고유벡터 성분들과 일치한다. 변량들을 선형 회귀방정식으로 표현할 수 있다는 점을 고려한다면 선형 판별함수의 계수들이 회귀의 비표준화 베타 값들에 해당한다는 점을 이해할 수 있다.
#' 
#' 이 계수들은 변량에 대한 각 변수의 상대적 기여도를 나타낸다.
#' 
#' LD1 열은 변량 1의 계수들이다. 이 계수들을 보면 부호가 반대인데 이것은 행동 횟수와 생각 횟ㅅ수의 효과가 서로 반대임을 의미한다. 따라서 이 첫 변량을 행동 횟수와 생각 횟수를 구분하는 변량을 간주할 수 있다.
#' 
#' 변량 2의 계수들(LD2 열)에 따르면 생각 횟수와 행수 모두 변량 2와 같은 방향으로 강하게 연관되어 있다. 이 변량은 생각 횟수와 행수에 비슷한 방식으로 영향을 미치는 어떠한 것을 대표한다.
#' 
#'  즉, 첫 번째 변량은 생각횟수와 행동횟수에 다른 방식으로 영향을 주는 어떤 요인을 기준으로 그룹들을 분리하고, 둘째 변량은 생각 횟수와 행동 횟수에 같은 방식으로 영향을 미치는 어떤 요인을 기준으로 그룹들을 분리한다고 할 수 있다.
#'  
#' proportion of trace를 보면 첫 변량은 전체 변동의 82.2%를 설명하고 둘째 변량은 17.8%만 설명한다.
#' 

predict(ocdDFA)

par(mfrow = c(1, 1))
plot(ocdDFA)

#' 변량 1은 BT그룹과 CBT 그룹을 분리하는 경계선이다.
#' 
#' 변량 2는 NT와 나머지 그룹들을 분리하지만 엄격하지는 않다.
#' 
#' 변량들은 그룹들의 조합을 유의하게 판별한다.
#' 
#' - 주된 다변량분산분석 후에 판별분석을 실행해서 두 가지 판별함수를 찾아냈다. 첫 판별함수는 분산의 82.2%를 설명하고, 둘째 판별함수는 17.8%만 설명했다. 판별함수들의 계수들에 따르면, 판별함수 1은 강박 행동 횟수($b = .603$)와 강박 생각 횟수($b = -.335$)를 분리한다. 둘째 변량도 행동 횟수($b = -.425$)와 생각 횟수($b = -.339$)를 분리한다. 판별점수 그래프를 보면, 첫 판별함수는 BT 그룹과 CBT 그룹을 분리하고, 둘째 판별함수는 비치료 대조군(NT)과 두 치료 그룹들을 분리한다.
#' 
#' #### 추가설명
#' 
#' 
#' #### 일변량분산분석 대 판별분석의 선택
#' 
#' 판별분석의 장점은 자료에 깔린 바탕 차원들에 관한 정보를 알 수 있다는 점이다.
#' 
#' #### 똑똑한 알렉스의 과제
#' 
#' 
