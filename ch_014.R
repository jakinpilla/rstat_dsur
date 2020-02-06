#' ---
#' title: "ch001(Tree-Based Method)"
#' author: "jakinpilla"
#' date : "`r format(Sys.time(), '%Y-%m-%d')`"
#' output: 
#'    github_document : 
#'        toc : true
#' ---

#+ message = FALSE, warning = FALSE
library(ez)
library(ggplot2)
library(nlme)
library(pastecs)
library(reshape2)
library(WRS)
dateData <- read.delim('LooksOrPersonality.dat',  header = T)

speedData <- melt(dateData, 
                  id = c('participant', 'gender'),
                  measured = c('att_high', 'av_high', 'ug_high',
                               'att_some', 'av_some', 'ug_some',
                               'att_none', 'av_none', 'ug_none'))

names(speedData) <- c('participant', 'gender', 'groups', 'dateRating')

speedData$personality <- gl(3, 60, labels = c('Charismatic', 'Average', 'Dullard'))

speedData$looks <- gl(3, 20, 180, labels = c('Attractive', 'Average', 'Ugly'))

library(tidyverse)

speedData %>%
  arrange(participant)


by(speedData$dateRating, 
   list(speedData$looks, speedData$personality, speedData$gender), 
   stat.desc, 
   basic = FALSE)  



#' 혼합분산분석 ------------------------------------------------------------------

SomevsNone <- c(1, 1, -2)
HivsAv <- c(-1, 1, 0)
contrasts(speedData$personality) <- cbind(SomevsNone, HivsAv)

AttractivevsUgly <- c(1, 1, -2)
AttractivevsAv <- c(-1, 1, -2)
contrasts(speedData$looks) <- cbind(AttractivevsUgly, AttractivevsAv)

speedModel <- ezANOVA(
  data = speedData,
  dv = .(dateRating),
  wid = .(participant),
  between = .(gender),
  within = .(looks, personality),
  type = 3, 
  detailed = T
)

speedModel



#' 일반선형모형으로서의 혼합설계 ---------------------------------------------------------

AttractivevsAv <- c(1, 0, 0)
UglyvsAv <- c(0, 0, 1)
contrasts(speedData$looks) <- cbind(AttractivevsAv, UglyvsAv)

HivsAv <- c(1, 0, 0)
DullvsAv <- c(0, 0, 1)
contrasts(speedData$personality) <- cbind(HivsAv, DullvsAv)

speedData$looks
speedData$personality


#' 모형구축 --------------------------------------------------------------------

baseline <- lme(dateRating ~ 1, 
                random = ~1|participant/looks/personality, 
                data = speedData, 
                method ='ML')

looksM <- update(baseline, 
                 .~. + looks)

personalityM <- update(looksM, .~. + personality)

genderM <- update(personalityM, .~. + gender)

looks_gender <- update(genderM, .~. + looks:gender)

personality_gender <- update(looks_gender, .~. + personality:gender)

looks_personality <- update(personality_gender, .~. + looks:personality)

speedDateModel <- update(looks_personality, .~. + looks:personality:gender)

anova(baseline, 
      looksM,
      personalityM,
      genderM, 
      looks_gender, 
      personality_gender,
      looks_personality, 
      speedDateModel)


summary(speedDateModel)


#' 성별의 주 효과 ----------------------------------------------------------------

by(speedData$dateRating, speedData$gender, stat.desc, basic = F)

genderBar <- ggplot(speedData, aes(gender, dateRating))

genderBar + 
  stat_summary(fun.y = mean, 
               geom = 'bar', 
               fill = 'White', 
               colour = 'Black') +
  stat_summary(fun.data = mean_cl_boot, 
               geom = 'pointrange') +
  labs(x = 'Gender', y = 'Mean Rating of Date')



#' 외모의 주효과 -----------------------------------------------------------------


by(speedData$dateRating, speedData$looks, stat.desc, basic = F)

looksBar <- ggplot(speedData, aes(looks, dateRating))

looksBar + 
  stat_summary(fun.y = mean, 
               geom = 'bar', 
               fill = 'White', 
               colour = 'Black') +
  stat_summary(fun.data = mean_cl_boot, 
               geom = 'pointrange') +
  labs(x = 'looks', y = 'Mean Rating of Date')


#' 성격의 주효과 -----------------------------------------------------------------

by(speedData$dateRating, speedData$personality, stat.desc, basic = F)

personalityBar <- ggplot(speedData, aes(personality, dateRating))

personalityBar + 
  stat_summary(fun.y = mean, 
               geom = 'bar', 
               fill = 'White', 
               colour = 'Black') +
  stat_summary(fun.data = mean_cl_boot, 
               geom = 'pointrange') +
  labs(x = 'personality', y = 'Mean Rating of Date')



#' 외모와 성격의 상호작용 ------------------------------------------------------------

speedData %>% colnames()

by(speedData$dateRating, list(speedData$personality, speedData$gender), stat.desc, 
   basic = F)

genderCharisma <- ggplot(speedData, aes(personality, dateRating, colour=gender))

genderCharisma + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line", aes(group = gender)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width= .2) +
  labs(x = 'Charisma', y = 'Mean Rating of Date', colour = 'Gender') +
  scale_y_continuous(limits = c(0, 100))
                 



lookscharisma <- ggplot(speedData, aes(looks, dateRating, colour=personality))

lookscharisma + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line", aes(group = personality)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width= .2) +
  labs(x = 'looks', y = 'Mean Rating of Date', colour = 'Gender')
  #scale_y_continuous(limits = c(0, 100))

#' 외모와 성격: 매력적 대 평범함, 높은 카리스마 대 보통 카리스마 ------------------------------------

speedData %>%
  filter(groups %in% c('att_high', 'att_some', 'av_high', 'av_some')) -> sample_1

sample_1 %>%
  ggplot(aes(looks, dateRating, colour = personality)) +
  stat_summary(fun.y = mean, geom = 'point') +
  stat_summary(fun.y = mean, geom = "line", aes(group = personality)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width= .2) +
  labs(x = 'Attractiveness', y = 'Mean Rating of Date', colour = 'Charisma') +
  scale_y_continuous(limits = c(0, 100))
  

#' looksAttractivevsAv:personalityHivsAv
#' 
#' Value Std.Error  DF  t-value p-value
#' 
#' -17.0  3.395006 108 -5.00736  0.0000
#' 
#' 카리스마 수준이 높을 때에 비해 카리스마 수준이 보통일때 외모 수준의 하락에 따른 
#' 평가의 하락폭이 더 컸다.
#' 


#' 외모와 성격의 상호작용: 추함 대 평법함, 높은 카리스마 대 보통 카리스마 -------------------------------

#' looksUglyvsAv:personalityHivsAv                     16.0  3.395006 108  4.71280  0.0000


speedData %>%
  filter(groups %in% c('av_high', 'av_some', 'ug_high', 'ug_some')) -> sample_2

sample_2 %>%
  ggplot(aes(looks, dateRating, colour = personality)) +
  stat_summary(fun.y = mean, geom = 'point') +
  stat_summary(fun.y = mean, geom = "line", aes(group = personality)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width= .2) +
  labs(x = 'Attractiveness', y = 'Mean Rating of Date', colour = 'Charisma') +
  scale_y_continuous(limits = c(0, 100))

#' 높은 카리스마와 보통 카리스마 모두 평범한 외모에서 못생긴 외모로 이동함에 따라 
#' 상대한 관한 관심이 떨어졌다. 그런데 그 하락 폭은 외모가 평범한 상대일 때 약간 더 크다.
#' 

#' 외모와 성격의 상호작용: 매력적 대 평범함, 어루숙함 대 보통 카리스마 ---------------------------------

speedData %>%
  filter(groups %in% c('att_some', 'av_some', 'att_none', 'av_none')) -> sample_3

sample_3 %>%
  ggplot(aes(looks, dateRating, colour = personality)) +
  stat_summary(fun.y = mean, geom = 'point') +
  stat_summary(fun.y = mean, geom = "line", aes(group = personality)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width= .2) +
  labs(x = 'Attractiveness', y = 'Mean Rating of Date', colour = 'Charisma') +
  scale_y_continuous(limits = c(0, 100))

#' looksAttractivevsAv:personalityDullvsAv            -13.4  3.395006 108 -3.94697  0.0001

#' 외모 수준의 하락에 따른 평가의 하락 폭이 보통 카리스마와 어수룩한 성격에서 유의하게 달랐다.
#' 이러한 유의성은 그래프가 보여주는 것과 모순된다.

#'
#' ### 그래프에 상호작용이 나타나지 않아도 대비가 유의한 이유
#' 
#' 고차 상호작용이 유의하다면 개별 주 효과나 더 낮은 차수의 상호작용들은 해석하지 말아야 한다.
#' looks~personality 상호작용 중 looksAttractivevsAv:personalityDullvsAv에 그래프를 보면 두 선이 평행하다. 흔히 대비가 유의하지 않다고 생각할 수 있다. 하지만 검정 통계량들을 보면 유의하다. 이러한 모순은 더 높은 차수의 상호작요인 looks~personality~gender 상호작용의 영향때문이다.
#' 


#' 외모와 성격의 상호작용 4: 추함 대 평범함, 어수룩함 대 보통 카리스마 ----------------------------------
speedData %>%
  filter(groups %in% c('av_some', 'av_none', 'ug_some', 'ug_none')) -> sample_4

sample_4 %>%
  ggplot(aes(looks, dateRating, colour = personality)) +
  stat_summary(fun.y = mean, geom = 'point') +
  stat_summary(fun.y = mean, geom = "line", aes(group = personality)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width= .2) +
  labs(x = 'Attractiveness', y = 'Mean Rating of Date', colour = 'Charisma') +
  scale_y_continuous(limits = c(0, 100))


#'
# summary(speedDateModel)
# looksUglyvsAv:personalityDullvsAv                   16.8  3.395006 108  4.94845  0.0000

#' 통계적으로 유의하다. 외모 수준의 하락에 따른 평가의 하락 폭이 어수룩한 성격보다 보통
#' 카리스마 성격에서 유의하게 더 컸다.


#'
#' 외모, 성격, 성별의 삼원 상호작용 -----------------------------------------------------

anova(baseline, looksM, personalityM, genderM, looks_gender, personality_gender, 
      looks_personality, speedDateModel)


# speedDateModel         8 22 1148.462 1218.707 -552.2309 7 vs 8  79.59473  <.0001

#' 삼원 상호작용은 유의하다. 이것이 최종 모형에서 가장 높은 차수의 유의한 효과이므로 다른
#' 낮은 차수 효과들은 해석할 필요 없이 이 효과만 해석하면 된다.
#' 

speedData %>%
  ggplot(aes(looks, dateRating, colour = personality)) +
  stat_summary(fun.y = mean, geom = 'point') +
  stat_summary(fun.y = mean, geom = "line", aes(group = personality)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width= .2) +
  labs(x = 'Attractiveness', y = 'Mean Rating of Date', colour = 'Charisma') +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~gender)


#' 오른쪽 그래프의 해석(남자들 그래프...) ----
#'
#' 외모가 매력적이면 카리스마 수준에 상관없이 상대에게 높은 관심을 보인다.
#' 
#' 여자의 외모가 못생겼을때 남자들은 카리스마 수준에 상관없이 상대에 관심이 없었다.
#' (데이터에서 gender가 Male이라는 의미는 대상자를 평가가 참가자의 성별이 남성이라는 뜻이므로...) 
#' 결국 남자는 이쁘면 된다는 의미이다.
#' 
#' 카리스마 수준이 영향을 미친 것은 상대의 외모가 평범할 때 뿐이다. 
#'
#' 왼쪽 그래프의 해석(여자들 그래프...) ----
#' 
#' 여자들은 데이트 상대의 카리스마 수준이 높으면 매력 수준에 무관하게 관심을 많이 보였다.
#' 
#' 여자들은 남자들의 성격이 어수룩하면 잘생긴 정도와 무관하게 관심을 두지 않았다.
#' 
#' 매력 수준이 영향을 미친 것은 상대의 성격이 보통일 때 뿐이다.
#' 
#' 결국 여자들은 매력보다는 카리스마를 우선시한다.
#' 


#' 외모, 성격, 성별의 상호작용 1: 매력적 대 평범함, 높은 카리시마 대 보통 카리스마, 남성 대 여성 ---------------

# summary(speedDateModel)


#' looksAttractivevsAv:personalityHivsAv:genderMale     5.8  4.801263 108  1.20802  0.2297

speedData %>%
  filter(groups %in% c('att_high', 'att_some', 'av_high', 'av_some')) -> sample_1

sample_1 %>%
  ggplot(aes(looks, dateRating, colour = personality)) +
  stat_summary(fun.y = mean, geom = 'point') +
  stat_summary(fun.y = mean, geom = "line", aes(group = personality)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width= .2) +
  labs(x = 'Attractiveness', y = 'Mean Rating of Date', colour = 'Charisma') +
  scale_y_continuous(limits = c(0, 100)) + 
  facet_wrap(~gender)

#' 외모가 평범할 때는 카리스마가 높은 상대가 보통인 상대보다 높은 평가를 받았다. 가장 중요한 것은, 
#' 그러한 패턴이 남녀 모두에게 나타났다는 점이다. 이는 이 대비가 유의하지 않다는 것을 의미한다.
#' 


#' 외모, 성격, 성별의 상호작용 2: 추함 대 평범함, 높은 카리스마 대 보통 카리스마, 남성 대 여성 ----------------
#' 
#' 

speedData %>%
  filter(groups %in% c('av_high', 'av_some', 'ug_high', 'ug_some')) -> sample_2

sample_2 %>%
  ggplot(aes(looks, dateRating, colour = personality)) +
  stat_summary(fun.y = mean, geom = 'point') +
  stat_summary(fun.y = mean, geom = "line", aes(group = personality)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width= .2) +
  labs(x = 'Attractiveness', y = 'Mean Rating of Date', colour = 'Charisma') +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~gender)

#' 데이크 상대가 카리스마적이면, 외모가 평범한 상대에 비해 못생긴 상대의 평가 하락이 여자들보다는 남자들에서 두드러졌다. 이것은 유의미한 대비이다.
#' 
#' 

#' 외모, 성격, 성별의 상호작용 3: 매력적 대 평범함, 어수룩함 대 보통 카리스마, 남성 대 여성 ------------------


speedData %>%
  filter(groups %in% c('att_some', 'av_some', 'att_none', 'av_none')) -> sample_3

sample_3 %>%
  ggplot(aes(looks, dateRating, colour = personality)) +
  stat_summary(fun.y = mean, geom = 'point') +
  stat_summary(fun.y = mean, geom = "line", aes(group = personality)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width= .2) +
  labs(x = 'Attractiveness', y = 'Mean Rating of Date', colour = 'Charisma') +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~gender)
  

summary(speedDateModel)

#' looksAttractivevsAv:personalityDullvsAv:genderMale  36.2  4.801263 108  7.53968  0.0000  
#' 
#' 상대의 성격이 어수룩할 때는, 매력적인 상대에 비한 평범한 상대의 평가 하락이 여자들보다
#' 남자들에서 더욱 두드러졌다.
#' 



#' 외모, 성격, 성별의 상호작용 4: 추함 대 평범함, 어수룩함 대 보통 카리스마 ----------------------------


#' looksUglyvsAv:personalityDullvsAv:genderMale         4.7  4.801263 108  0.97891  0.3298
#' 
#' 대비가 유의하지 않음
#' 
speedData %>%
  filter(groups %in% c('av_some', 'av_none', 'ug_some', 'ug_none')) -> sample_4

sample_4 %>%
  ggplot(aes(looks, dateRating, colour = personality)) +
  stat_summary(fun.y = mean, geom = 'point') +
  stat_summary(fun.y = mean, geom = "line", aes(group = personality)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width= .2) +
  labs(x = 'Attractiveness', y = 'Mean Rating of Date', colour = 'Charisma') +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~gender)


#' 남자들은 성격이야 어떻든 외모가 매력적인 상대와 데이트하고 싶어 하는 것으로 보인다. 여자들은
#' 그와 거의 완전한 반대이다. 여성 참가자들은 외모와는 무관하게 카리스마가 넘치는 사람과 데이트하고 
#' 싶어한다.
#' 

#' ### 효과크기 계산
#' 

# install.packages("devtools")
library(devtools)
# install_github("Frostarella/DSUR.noof")
library(DSUR.noof)

rcontrast(-1.20802, 108)
rcontrast(3.85315, 108) # 추함 대 평범함, 높은 카리스마 대 보통 카리스마, 남성 대 여성
rcontrast(-7.53968, 108) # 매력적 대 평범함, 어수룩함 대 보통 카리스마, 남성 대 여성
rcontrast(-0.97891, 108) # 추함 대 평범함, 어수룩함 대 보통 카리스마, 남성 대 여성



#' ### 혼합 분산분석 결과의 보고 ----------------------------------------------------------

#' - 참가자가 데이트 상대에게 보인 관심에 대한 데이트 상대의 매력의 주 효과들은 khai^2(2) = 68.30, 
#' p < .0001로 유의했고, 데이트 상대의 카리스마의 주 효과들은 khai^2(2) = 138.76, p < .0001로 유의했다.
#' 그러나 남성 참가자들과 여성 참가자들의 평가는 대체로 같았다.
#' 
#' - 데이트 상대 매력과 참가자 성별의 상호작용은 khai^2(2) = 39.54, p < .0001로 유의한 효과를 보였고,
#' 데이트 상대 카리스마 수준과 참가자 성별의 상호작용은 khai^(2) = 57.96, p < .0001로 유의했으며, 데이트 상대 카리스마 수준과 데이트 상대 매력 수준의 상호작용도 khai^2(4) = 77.14, p < .0001로 유의했다.
#' 
#' - 가장 중요하게는, 외모(매력)와 성격(카리스마), 성별의 상호작용이 khai^2(4) = 79.59, p < .0001로 유의했다. 이는 앞에서 서술한 외모\*성격의 상호작용의 효과가 남성 참가자들과 여성 참가자들에서 달랐음을
#' 뜻한다. 이 상호작용은 데이트 상대의 매력 수준에 따른 차이(평범함 외모 범주 대 기타 범주들)에 대한 
#' 데이트 상대의 카리스마 수준에 따른 차이(보통 카리스마 대 기타 범주들)에 대한 성별 차이에 관한 것이다. 대비들을 이용해서 이 상호작용을 좀 더 분해했다.
#' 
#' - 첫 대비는 높은 카리스마 대 보통 카리스마에 대한 매력적 상대와 평범한 상대의 평가 차이의 성별 차이를 비교한다. 이 대비는 b = 5.8, t(108) = 1.21, p = .230, r = .12로 유의하지 않았다. 이 결과는, 데이트 
#' 상대의 카리스마 수준이 높을 때보다 보통일 때 매력 수준의 하락에 따른 평가 하락폭이 남녀 모두 컸음을 말해준다.
#' 
#' - 둘째 대비는 높은 카리스마 대 보통 카리스마에 대한 못생긴 상대와 평범한 상대의 평가 차이의 성별 차이를 비교한다. 이 대비는 b = -18.5, t(108) = -3.85, p < .001, r = .35 로 유의했다. 이 결과는, 
#' 데이트 상대의 카리스마가 보통일 때는 매력 수준의 하락에 따른 평가 하락폭이 모두 비슷했지만, 카리스마 수준이 높을 때는 평범한 외모에 비한 추한 대상에 대한 평가의 하락이 여자들보다 남자들이 훨씬 컸음을 말해준다.
#' 
#' - 셋째 대비는 어수룩함 대 보통 카리스마에 대한 매력적인 상대와 평범한 상대의 평가 차이의 성별 차이를r 비교한다. 이 대비는 b = 6.2, t(108) = 7.54, p < .001, r = .59로 유의했다. 이 결과는, 카리스마 수준이 보통인 데이트 상대의 경우에는 매력 수준의 하락에 따른 평가의 하락폭이 남녀가 거의 비슷했지만, 상대의 성격이 어수룩할 때는, 매력적인 상대에 비한 평범한 상대의 평가 하락이 여자들보다 남자들에서 더욱 두드러졌음을 말해준다. 
#' 
#' - 마지막 대비는 어수룩함 대 보통 카리스마에 대한 못생긴 상대와 평범한 상대의 평가 차이의 성별 차이를 비교한다. 이 대비는 b = 4.7, t(108) = .98, p = .330, r = .09로 유의하지 않았다. 이 결과는, 남녀 모두 데이트 상대의 매력 수준 감소에 따른 평가 하락폭이 상대가 어수룩한 성격일 때보다 보통 카리스마일 때 더 컸음을 의미한다.
#' 

#' ### 강건한 혼합 설계 분석 ------------------------------------------------------------

#' --tsplit() : 절사평균에 기초해서 이원 혼합 분산분석 수행
#' 
#' --sppba() : 이원 혼합 설계의 요인 A의 주 효과를 M 추정량과 부트스크랩을 이용하여 계산
#' 
#' - sppbb() : 이원 혼합 설계의 요인 B의 주 효과를 M 추정량과 부트스트랩을 이용하여 계산
#' 
#' - sppbi() : 이 함수는 이원 혼합 설계의 요인 A\*B의 주 효과를 M 추정량과 부트스트랩을 이용하여 계산
#' 
#' 


pictureData <- read.delim("ProfilePicture.dat", header = T)
pictureData %>% colnames()

#' relationship_status : 연애상태
#' 
#' couple : 3주 동안 어떤 남자와 같이 찍은 사진을 올렸을 때 남자의 친구 요청 횟수
#' 
#' alone : 3주 동안 자신만 나온 사진을 올렸을때 남자의 친구 요청 횟수
#' 
#' 
pictureData %>% head

#' 강건한 함수는 넓은 형식을 요구
#' 
pictureData$row <- c(1:17, 1:23)

profileMelt <- melt(pictureData, 
                    id = c('case', 'row', 'relationship_status'), 
                    measured = c('couple', 'alone'))


names(profileMelt) <- c('case', 'row', 'relationship_status', 'profile_picture',
                        'friend_requests')


profileData <- dcast(profileMelt, 
                    row ~ relationship_status + profile_picture, 
                    value = 'friend_requests')

profileData$row <- NULL

profileData

#' tsplit() : 절사평균에 기초하여 이원 혼합 분산분석 시행

tsplit(2, 2, profileData)

#' sppba() : 이원 혼합 설계의 요인 A의 주효과를 M 추정량과 부트스트랩을 이용하여 계산

sppba(2, 2, profileData, est = mom, nboot = 2000)

#' sppbb() : 이 함수는 이원 혼합 설계의 요인 B의 주효과를 M 추정량과 부트스트랩을 
#' 이용해서 계산
#' 
sppbb(2, 2, profileData, est = mom, nboot = 2000)

#' sppbi() : 이 함수는 이원 혼합 설계의 요인 A\*B의 주효과를 M 추정량과 부트스트랩을 이용해서 계산
#' 
sppbi(2, 2, profileData, est = mom, nboot = 2000)
