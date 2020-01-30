library(car)
# install.packages('compute.es')
library(compute.es)
library(ggplot2)
library(tidyverse)
library(multcomp)
# install.packages('pastecs')
library(pastecs)
library(reshape2)
# install.packages('WRS')
# library(WRS)


gender <- gl(2, 24, labels = c('Female', 'Male'))
alcohol <- gl(3, 8, 48, labels = c('None', '2 Pints', '4 Pints'))

gender

alcohol


attractiveness <- c(65, 70, 60, 60, 60, 55, 60, 55, 70, 65, 
                    60, 70, 65, 60, 60, 50, 55, 65, 70, 55,
                    55, 60, 50, 50, 50, 55, 80, 65, 70, 75, 
                    75, 65, 45, 60, 85, 65, 70, 70, 80, 60,
                    30, 30, 30, 55, 35, 20, 45, 40)


gogglesData <- data.frame(gender, alcohol, attractiveness)

dim(gogglesData)



ggplot(gogglesData, aes(alcohol, attractiveness)) + geom_boxplot() + facet_wrap(~gender)

by(gogglesData$attractiveness, gogglesData$gender, stat.desc)
by(gogglesData$attractiveness, gogglesData$alcohol, stat.desc)
by(gogglesData$attractiveness, list(gogglesData$alcohol, gogglesData$gender), stat.desc)

leveneTest(gogglesData$attractiveness, gogglesData$gender, center = median)
leveneTest(gogglesData$attractiveness, gogglesData$alcohol, center = median)
leveneTest(gogglesData$attractiveness, interaction(gogglesData$alcohol, gogglesData$gender), center = median)

contrasts(gogglesData$alcohol) <- cbind(c(-2, 1, 1), c(0, -1, 1))
contrasts(gogglesData$gender) <- c(-1, 1)

gogglesData$alcohol
gogglesData$gender


gogglesModel <- aov(attractiveness ~ alcohol*gender, data = gogglesData)

gogglesData %>%
  group_by(alcohol) %>%
  summarise(mean.attractiveness = mean(attractiveness)) 

ggplot(gogglesData, aes(alcohol, attractiveness)) + 
  stat_summary(fun.y = mean, geom ='bar', position = 'dodge', fill = 'white', colour = 'Black') + 
  stat_summary(fun.data = mean_cl_normal, geom='pointrange')

  
  
ggplot(gogglesData, aes(gender, attractiveness)) + 
  stat_summary(fun.y = mean, geom ='bar', position = 'dodge', fill = 'white', colour = 'Black') + 
  stat_summary(fun.data = mean_cl_normal, geom='pointrange')


gogglesData %>%
  ggplot(aes(alcohol, attractiveness, colour = gender)) +
  stat_summary(fun.y = mean, geom='point') +
  stat_summary(fun.y = mean, geom = 'line', aes(group = gender)) +
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width =.2) 

gogglesModel <- aov(attractiveness ~ gender + alcohol + gender:alcohol, data = gogglesData)
summary.lm(gogglesModel)


gogglesData %>%
  mutate(alcohol = ifelse(alcohol %in% c('2 Pints', '4 Pints'), 'Alcohol', 'None')) %>%
  mutate(alcohol = factor(alcohol, levels = c('None', 'Alcohol')))-> gogglesData_1


gogglesData_1 %>%
  ggplot(aes(alcohol, attractiveness, colour = gender)) +
  stat_summary(fun.y = mean, geom='point') +
  stat_summary(fun.y = mean, geom = 'line', aes(group = gender))

  
  
  # 단순효과분석
gogglesData$simple <- gl(6, 8)
gogglesData$simple <- factor(gogglesData$simple, 
                             levels = c(1:6), 
                             labels = c('F_None', 'F_2pints', 'F_4pints', 'M_None', 'M_2pints', 'M_4pints'))

gogglesData

alcEffect1 <- c(-2, 1, 1, -2, 1, 1)
alcEffect2 <- c(0, -1, 1, 0, -1, 1)
gender_none <- c(-1, 0, 0, 1, 0, 0)
gender_twoPint <- c(0, -1, 0, 0, 1, 0)
gender_fourPint <- c(0, 0, -1, 0, 0, 1)


simpleEff <- cbind(alcEffect1, alcEffect2, gender_none, 
                   gender_twoPint, gender_fourPint)


contrasts(gogglesData$simple) <- simpleEff

simpleEffectModel <- aov(attractiveness ~ simple, data = gogglesData)

summary(simpleEffectModel)
summary.lm(simpleEffectModel)

# 사후검정
pairwise.t.test(gogglesData$attractiveness, gogglesData$alcohol, 
                p.adjust.method = 'bonferroni')

postHocs <- glht(gogglesModel, linfct = mcp(alcohol = 'Tukey'))

summary(postHocs)
confint(postHocs)


plot(gogglesModel)


