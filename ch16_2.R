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


