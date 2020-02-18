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
#' 







