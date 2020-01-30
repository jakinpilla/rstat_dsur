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



# 혼합분산분석 ------------------------------------------------------------------

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



# 일반선형모형으로서의 혼합설계 ---------------------------------------------------------

AttractivevsAv <- c(1, 0, 0)
UglyvsAv <- c(0, 0, 1)
contrasts(speedData$looks) <- cbind(AttractivevsAv, UglyvsAv)

HivsAv <- c(1, 0, 0)
DullvsAv <- c(0, 0, 1)
contrasts(speedData$personality) <- cbind(HivsAv, DullvsAv)

speedData$looks
speedData$personality


# 모형구축 --------------------------------------------------------------------

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


# 성별의 주 효과 ----------------------------------------------------------------

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



# 외모의 주효과 -----------------------------------------------------------------


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


# 성격의 주효과 -----------------------------------------------------------------

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


