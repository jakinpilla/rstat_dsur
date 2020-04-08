#' ---
#' title: "ch07_1(회귀)"
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
library(gmodels)
library(MASS)
library(nlme) # 다층모형을 위해

#' ### 7.1 이번 장에서 배우는 내용
#' 
#' ### 7.2 회귀의 소개
#' 
#' 단순회귀, 다중회귀
#' 
#' 최소제곱법(method of least squares) 수학적 기법을 이용해 자료를 가장 잘 서술하는 직선을 선택
#' 
#' #### 7.2.1 직선에 관한 중요한 정보 몇 가지
#' 
#' 기울기 $b_{1}$, 절편 $b_{0}$
#' 
#' $b_{1}$, $b_{0}$들을 회귀계수라고 한다. 
#' 
#' $$Y_{i} = (b_{0} + b_{1} X_{i}) + \epsilon_{i}$$
#' 
#' #### 7.2.2. 최소제곱법
#' 
#' line of best fit
#' 
#' #### 7.2.3. 적합도 평가: 제곱합, r, $R^{2}$
#' 
#' 