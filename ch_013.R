setwd("~/rstat_andy_field")
gl(3, 60, labels = c('Beer', 'Wine', 'Water'))
attitudeData <- read.delim('Attitude.dat', header = T)


library(tidyverse)

attitudeData %>%
  gather(key, value, -participant) %>%
  rename(groups = key, 
         attitude = value) %>%
  as_tibble() %>%
  mutate(drink = gl(3, 60, labels = c('Beer', 'Wine', 'Water'))) %>%
  mutate(imagery = gl(3, 20, 180, labels = c('Positive', 'Negative', 'Neutral'))) -> data

data %>% View()


data %>% str()
data$participant <- factor(data$participant, levels = c('P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7', 'P8', 'P9', 'P10'))

data %>%
  arrange(participant)

data %>%
  arrange(participant) -> longAttitude

# first: install dependent packages
# install.packages(c("MASS", "akima", "robustbase"))

# second: install suggested packages
# install.packages(c("cobs", "robust", "mgcv", "scatterplot3d", "quantreg", "rrcov", "lars", "pwr", "trimcluster", "parallel", "mc2d", "psych", "Rfit"))

# third: install WRS
# install.packages("WRS", repos="http://R-Forge.R-project.org", type="source")

attitudeData <- read.delim('Attitude.dat', header = T)
attitudeData %>%
  gather(groups, attitude, -participant) %>%
  # rename(groups = key, 
  #        attitude = value) %>%
  # as_tibble() %>%
  mutate(drink = gl(3, 60, labels = c('Beer', 'Wine', 'Water'))) %>%
  mutate(imagery = gl(3, 20, 180, labels = c('Positive', 'Negative', 'Neutral'))) -> longAttitude

longAttitude %>% str()


library(compute.es)
library(pastecs)

# install.packages('multcomp')
library(multcomp)
# install.packages('ez')
library(ez)


by(longAttitude$attitude, list(longAttitude$drink, longAttitude$imagery), stat.desc, basic = FALSE)

AlcoholvsWater <- c(1, 1, -2)
BeervsWine <- c(-1, 1, 0)
contrasts(longAttitude$drink) <- cbind(AlcoholvsWater, BeervsWine)

NegativevsOthers <- c(1, -2, 1)
PositivevsNeutral <- c(-1, 0, 1)
contrasts(longAttitude$imagery) <- cbind(NegativevsOthers, PositivevsNeutral)


longAttitude$drink
longAttitude$imagery

longAttitude %>% str()

attitudeModel <- ezANOVA(data = longAttitude, 
                         dv = .(attitude), 
                         wid = .(participant), 
                         within = .(imagery, drink), 
                         type = 3, 
                         detailed = T)

attitudeModel




# 상호작용 효과 -----------------------------------------------------------------

pairwise.t.test(longAttitude$attitude, longAttitude$groups,
                paired = T, 
                p.adjust.method = 'bonferroni')






longAttitude

by(longAttitude$attitude, longAttitude$drink, stat.desc, basic = FALSE)

longAttitude %>% str()



# 일반선형모형으로서의 요인 반복측정 설계 ---------------------------------------------------

library(nlme)
baseline <- lme(attitude ~ 1, random = ~1|participant/drink/imagery, 
                data = longAttitude, 
                method = 'ML')

drinkModel <- update(baseline, .~. + drink)

imageryModel <- update(drinkModel, .~. + imagery)

attitudeModel <- update(imageryModel,  .~. + drink:imagery)

anova(baseline, drinkModel, imageryModel, attitudeModel)

summary(attitudeModel)


# 요인 반복측정 분산분석 ------------------------------------------------------------

attitudeModel_1 <- ezANOVA(data = longAttitude, 
                           dv = .(attitude),
                           wid = .(participant),
                           within = .(imagery, drink), 
                           type = 3, 
                           detailed = T)

attitudeModel_1


summary(attitudeModel)
# install.packages("devtools")
library(devtools)
# install_github("Frostarella/DSUR.noof")
library(DSUR.noof)

rcontrast(3.18, 38)
rcontrast(-1.47, 38)






