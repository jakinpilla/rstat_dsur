#' ---
#' title: "ch016(다변량분산분석)"
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


#' #### 이번 장에서 배우는 내용
#' 
#' 결과측도가 하나가 아니라 여러 개인 모형을 분석할 수 있도록 분산분석을 확장하는 것이 가능하다. 그러한 확장을 다변량분산분석(MANOVA)이라고 부른다.
#' 
#' #### 다변량 분석은 언제 사용할까?
#' 
#' 종속변수가 여러개인 상황에 사용한다.
#' 
#' 다변량분산분석은 여러 개의 종속변수를 동시에 살펴보도록 고안된 것이며, 그래서 다변량 검정이라고도 부른다.
#' 
#' #### 소개: 분산분석과 다변량분산분석의 유사점과 차이점
#' 
#' 검정 횟수가 많을수록 집단별 오류율이 상승한다.
#' 
#' 측정한 종속변수가 많을수록 개별 분산분석을 더 많이 수행해야 하고, 그러면 제1종 오류(귀무가설이 참이지만 이에 불구하고 귀무가설을 기각하는 오류)를 범할 확률이 높아진다.
#' 
#' 모든 종속변수를 하나로 포함해서 분석하는 다변량 분산분석은 결과변수들 사이의 관계를 고려한다.
#' 
#' 다변량분산분석은 여러 차원의 조합에 따른 그룹 차이를 검출하는 능력을 갖추고 있다.
#' 
#' **분산분석은 오직 한 변수에 대한 그룹 차이를 검출할 수 있지만 다변량분산분석은 변수들의 조합에 따른 그룹 차이를 검출할 수 있다.**
#' 
#' 분산분석을 여러 번 수행하는 것보다 다변량분산분석을 한 번 수행하는 것이 낫다.
#' 
#' #### 다변량분산분석에서 주의할 점
#' 
#' 한 번은 발견법적 근거에 따라 선택한 변수들을 분석하고, 다른 한 번은 이론적으로 의미가 있는 변수들을 분석하는 등의 조합이 가능하다. 요지는, 힘들게 측정한 것이 아까우니 모든 종속변수를 한꺼번에 다변량분산분석에 집어넣는 식의 단순한 접근 방식은 금물이라는 것이다.
#' 
#' #### 이번 장의 예제
#' 
#' OCD : obessive compulsive disorder
#' 
#' CBT(Cognitive behaviour therapy) : 인지행동치료를 받은 강박 장애 환자들의 그룹룹
#' 
#' BT(behaviour therapy) : 행동치료를 받은 강박 장애 환자들의 그룹룹
#' 
ocdData <- read.delim('OCD.dat', header = T); ocdData
#' 
#' ####  다변량분산분석의 이론
#' 
#' #### 행렬의 소개
#' 
#' 정방행렬(square matrix), 대각성분(diagonal component), 단위행렬(identity matrix)
#' 
#' 행벡터(row vector), 열벡터(column vector)
#' 
#' #### 몇 가지 주요 행렬과 그 기능
#' 
#' 다변량분산분석은 각 종속변수가 설명하는 변동에 관한 정보를 담은 행렬을 활용한다.
#' 
#' 다변량분산분석의 검정통계량은 여러 종속변수에 대한 체계적 변동 대 비체계적 변동의 비를 비교해서 유도한다.
#' 
#' 그러한 비교에는 모든 종속변수의 체계적 변동을 나타내는 행렬과 모든 종속변수의 비체계적 변동을 나타내는 행렬의 비가 쓰인다.
#' 
#' 분산분석의 검정통계량과 다변량분산분석의 검정통계량은 모두 비체계적 변동에 대한 체계적 변동의 효과의 비를 나타내지만, 분산분석에서는 그러한 각 변동이 하나의 값인 반면 다변량분산분석에서는 각각이 여러 개의 분산과 공분산을 담은 행렬이다.
#' 
#' - **H: hypothesis sum of squares and cross-products matrix**
#' 
#' - **E: error sum of squares and cross-products matrix**
#' 
#' - **T: total sum of squares and cross-products matrix**
#' 
#' 교차곱: 두 변수의 차이를 결합한 총량을 나타내는 값....두 변수들의 차이들을 각각 곱해서 합한 것이다.
#' 
#' #### 직접 해보는 다변량분산분석: 가상의 예
#' 
#' ##### 종속변수 1 Actions에 대한 일변량분산분석
#' 

ocdData$Group <- factor(ocdData$Group, levels = c('CBT', 'BT', 'No Treatment Control'))
ocdData %>% str()

#' ##### SST_Actions: 총 제곱합
ocdData %>%
  summarise(var.Actions = var(Actions)) -> var.Actions.df

SST_Actions <- var.Actions.df$var.Actions*(30-1); SST_Actions


#' ##### SSM_Actions
ocdData %>%
  summarise(total.mean = mean(Actions)) -> total_mean
#' 
ocdData %>%
  group_by(Group) %>%
  summarise(mean.Ations = mean(Actions)) -> ea_group_means; ea_group_means

vec.mean.actions <- ea_group_means$mean.Ations

SSM_Actions <- 10*(vec.mean.actions[1] - total_mean)^2 + 10*(vec.mean.actions[2] - total_mean)^2 + 10*(vec.mean.actions[3] - total_mean)^2 

SSM_Actions <- SSM_Actions$total.mean

SSM_Actions

#' ##### SSR_Actions
ocdData %>%
  group_by(Group) %>%
  summarise(var.Actions = var(Actions)) -> ea_group_vars; ea_group_vars

SSR_Actions <- ea_group_vars$var.Actions[1]*(10-1) + ea_group_vars$var.Actions[2]*(10-1) + ea_group_vars$var.Actions[3]*(10-1)

SSR_Actions

#' ##### F 검정통계량
SS <- c(SSM_Actions, SSR_Actions)
df <- c(2, 27)


df <- data.frame(SS, df) %>%
  mutate(MS = SS/df)

F_stat <- df$MS[1] / df$MS[2]; F_stat

#' ##### 종속변수 2(Thoughts)에 대한 일변량분산분석
#' 
#' ##### SST_Thoughts: Thoughts 총 제곱합

ocdData %>%
  summarise(var.Thoughts = var(Thoughts)) -> var.Thoughts.df

SST_Thoughts <- var.Thoughts.df$var.Thoughts*(30-1); SST_Thoughts

#' ##### SSM_Thoughts: Thoughts 모형 제곱합
ocdData %>%
  summarise(total.mean = mean(Thoughts)) -> total_mean
 
ocdData %>%
  group_by(Group) %>%
  summarise(mean.Thoughts = mean(Thoughts)) -> ea_group_means; ea_group_means

vec.mean.Thoughts <- ea_group_means$mean.Thoughts

SSM_Thoughts <- 10*(vec.mean.Thoughts[1] - total_mean)^2 + 10*(vec.mean.Thoughts[2] - total_mean)^2 + 10*(vec.mean.Thoughts[3] - total_mean)^2 

SSM_Thoughts <- SSM_Thoughts$total.mean

SSM_Thoughts

#' ##### SSR_Thoughts: Thoughts 잔차 제곱합
ocdData %>%
  group_by(Group) %>%
  summarise(var.Thoughts = var(Thoughts)) -> ea_group_vars; ea_group_vars

SSR_Thoughts <- ea_group_vars$var.Thoughts[1]*(10-1) + ea_group_vars$var.Thoughts[2]*(10-1) + ea_group_vars$var.Thoughts[3]*(10-1)

SSR_Thoughts

#' ##### F 검정통계량
SS <- c(SSM_Thoughts, SSR_Thoughts)
df <- c(2, 27)
df <- data.frame(SS, df) %>%
  mutate(MS = SS/df)

F_stat <- df$MS[1] / df$MS[2]; F_stat

#' #### 종속변수들의 관계: 교차곱
#' 
#' 세가지 교차곱: 총교차곱, 모형교차곱, 잔차교차곱...
#' 
#' ##### 총교차곱
#' 
#' $$ CP_{T} = \sum_{i=1}^{n} \left( x_{i(Actions)} - \overline{X}_{total(Actions)} \right) \left( x_{i(Thoughts)} - \overline{X}_{total(Thoughts)} \right)$$
#' 
ocdData %>%
  as_tibble() %>%
  mutate(actions_total_actions_mean = Actions - mean(Actions)) %>%
  mutate(actions_total_actions_mean = round(actions_total_actions_mean, 2)) %>%
  mutate(thoughts_total_thoughts_mean = Thoughts - mean(Thoughts)) %>%
  mutate(thoughts_total_thoughts_mean = round(thoughts_total_thoughts_mean, 2)) %>%
  mutate(d_1_d_2 = actions_total_actions_mean * thoughts_total_thoughts_mean) %>%
  mutate(d_1_d_2 = round(d_1_d_2, 2)) -> ocdDatas_cp_total

ocdDatas_cp_total

ocdDatas_cp_total %>%
  summarise(sum.d_1_d_2 = sum(d_1_d_2))

#' 
#' ##### 모형교차곱
#' 
#' 실험조작이 종속변수들의 관계에 얼마나 영향을 미치는지에 대한 정보
#' 

#' $$ CP_{M} = \sum_{grp=1}^{k} n \left[ \left( \overline{x}_{grp(Actions)} - \overline{X}_{total(Actions)}} \right) \left( \overline{x}_{grp(Thoughts)} - \overline{X}_{total(Thoughts)}} \right) \right] $$

ocdData %>%
  summarise(mean.Actions = mean(Actions)) %>% pull() -> mean_total_actions

ocdData %>%
  summarise(mean.Thoughts = mean(Thoughts)) %>% pull() -> mean_total_thoughts

ocdData %>%
  group_by(Group) %>%
  summarise(mean.Actions = mean(Actions),
            mean.Thoughts = mean(Thoughts)) %>%
  ungroup() %>%
  mutate(d1 = mean.Actions - mean_total_actions) %>%
  mutate(d2 = mean.Thoughts - mean_total_thoughts) %>%
  mutate(d1_d2 = d1 * d2) %>%
  mutate(n_d1_d2 = 10*d1_d2) -> ocdData_cp_model

ocdData_cp_model

ocdData_cp_model %>%
  summarise(sum.n_d1_d2 = sum(n_d1_d2))

#' ##### 잔차교차곱
#' 
#' 개인차 또는 모형에 존재하는 오차가 종속변수들의 관계에 미치는 영향이 어느 정도인지를 나타냄.
#' 
#' 

#' $$ CP_{R} = \sum_{i=1}^{n} \left( x_{i(Actions)} - \overline{X}_{grp(Actions)} \right) \left( x_{i(Thoughts)} - \overline{X}_{grp(Thoughts)} \right)$$

ocdData %>%
  as_tibble() %>%
  group_by(Group) %>%
  mutate(d1 = Actions - mean(Actions)) %>%
  mutate(d2 = Thoughts - mean(Thoughts)) %>%
  mutate(d1_d2 = d1 * d2) -> ocdData_cp__residuals

ocdData_cp__residuals %>% 
  ungroup() %>%
  summarise(sum.d1_d2 = sum(d1_d2))
 

#' #### 총 SSCP 행렬(T)
#' 
#' |   |  열 1 Actions | 열 2 Thoughts   |
#' |:----:|:----:|:----:|
#' | 행 1 Actions | $SS_{Actions}$ | $CP_{T}$ |
#' | 행 2 Thoughts | $CP_{T}$ | $SS_{Thoughts}$ |
#' 

#' SST_Actions
SST_Actions

#' CPT
ocdDatas_cp_total %>%
  summarise(sum.d_1_d_2 = sum(d_1_d_2)) %>% pull() -> CPT; CPT # 5.44

#' SST_Thoughts
SST_Thoughts 


#' $$ T  = \begin{pmatrix} 61.47 & 5.44\\
#' 5.44 & 141.47
#' \end{pmatrix}$$
#' 
#' #### 잔차 SSCP 행렬(E)
#' 
#' |   |  열 1 Actions | 열 2 Thoughts   |
#' |:----:|:----:|:----:|
#' | 행 1 Actions | $SS_{R(Actions)}$ | $CP_{R}$ |
#' | 행 2 Thoughts | $CP_{R}$ | $SS_{R(Thoughts)}$ |
#' 
#' SSR_Actions

SSR_Actions

#' CPR
ocdData_cp__residuals %>%
  ungroup() %>%
  summarise(sum.d1_d2 = sum(d1_d2)) %>%
  pull() -> CPR; CPR

#' SSR_Thoughts
SSR_Thoughts

#' $$ E  = \begin{pmatrix} 51 & 13\\
#' 13 & 122
#' \end{pmatrix}$$
#' 
#' #### 모형 SSCP 행렬(H)
#' 
#' |   |  열 1 Actions | 열 2 Thoughts   |
#' |:----:|:----:|:----:|
#' | 행 1 Actions | $SS_{M(Actions)}$ | $CP_{M}$ |
#' | 행 2 Thoughts | $CP_{M}$ | $SS_{M(Thoughts)}$ |
#' 
#' SSM_Actions

SSM_Actions

#' CPM
ocdData_cp_model %>%
  summarise(sum.n_d1_d2 = sum(n_d1_d2)) %>%
  pull() -> CPM; CPM

#' SSM_Thoughts
SSM_Thoughts

#' $$ H  = \begin{pmatrix} 10.47 & -7.53\\
#' 13 & 122
#' \end{pmatrix}$$
#' 
#' 각 성분의 의미를 생각해보면, 모형 SSCP 행렬이 각 종속변수에 존재하는 체계적 변동의 양과 종속변수들 사이에 존재하는 모형에 의한 상호의존성의 총량을 나타낸다는 점을 명확히 알 수 있다.
#' 
#' 총교차곱은 모형교차곱과 잔차교차곱의 합이다.
#' 
#' $$ T = H + E $$
#' 
#' $$ T  = \begin{pmatrix} 10.47 & -7.53\\ -7.53 & 19.47 \end{pmatrix} + \begin{pmatrix} 51 & 13\\ 13 & 122 \end{pmatrix} $$
#' 
#' 결국 다변량분산분석의 계산은 일변량분산의 것과 개념적으로 동일하다.
#' 
#' #### 다변량분산분석 검정통계량의 이론
#' 
#' 일변량분산분석에서는 비체계적 변동에 대한 체계적 변동의 비(즉 $SS_{M}$을 $SS_{R}로 나눈 값$)를 계산한다. 이를 다변량분산분석에 그대로 적용한다면 행렬 H를 행렬 E로 나누어야 할 것이다.
#' 
#' 한 행렬을 다른 행렬로 나누려면 한 행렬에 다른 행렬의 역(역행렬)을 곱해야 한다.
#' 
#' 정리하자면, **다변량분산분석의 검정통계량은 모형 SSCP 행렬에 잔차 SSCP 행령의 역을 곱한 것이다.**
#' 
#' $$ HE^{-1} $$
#' 

c(51, 13, 13, 122) %>% matrix(2, 2) %>% solve() -> inv_E
c(10.47, -7.53, -7.53, 19.47) %>% matrix(2, 2) -> H

H %*% inv_E


#' $HE^{-1}$은 모형의 비체계적 변동에 대한 모형의 체계적 변동의 비를 나타내고 따라서 일변량분산분석의 F 비에 대응된다.
#' 
#' **그런데, 문제는 체계적 변동에 해당하는 행렬을 비체계적 변동에 해당하는 행렬로 나누면 하나의 값이 아니라 여러 개의 값으로 된 행렬이 나온다는 점이다. 지금 예에서 그러한 행렬은 네 개의 값으로 구성된다. 그리고 종속변수가 셋인 상황에서는 그러한 행렬이 아홉 갱의 값으로 구성된다. 일반화하면, 그러한 행렬의 성분은 항상 $p^{2}$개 인데, 여기서 $p$는 종속변수 개수이다. 유의성을 판정하려면 그러한 여러 개의 값을 하나의 의미있는 값으로 요약해야 한다. 이 지점이 바로, 다변량분산분석에 깔린 수학을 이해하려는 시도를 포기하고 그냥 개념적으로만 설명할 수 밖에 없는 지점이다.**
#' 
#' ##### 판별함수 변량
#' 
#' 통계적 유의성을 평가할 값이 하나가 아니라 여러 개라는 문제는 종속변수들을 바탕 차원(underlying) 차원 또는 요인들로 변환함으로써 크게 단순화할 수 있다.
#' 
#' 일단의 종속변수로부터 하나의 독립변수를 예측하려 한다. 즉 종속변수들의 바탕 선형 차원들을 계산할 수 있다.
#' 
#' 종속변수들의 그러한 선형결합(일차결합)을 변량(variate)이라고 부른다. 또는 잠재변수(latent variable)이나 요인(factor)이라고도 부른다.
#' 
#' 지금 맥락에서 우리가 원하는 것은 그러한 선형 변량으로부터 한 참가자가 속한 그룹을 예측하는 것이다.
#' (즉, 참가자가 CBT를 받았는지, BT를 받았는지, 아니면 아무 치료도 받지 않았는지를 예측하는 것이다. )
#' 
#' 즉, 그러한 변량은 참가자의 그룹을 판별하는 용도로 쓰인다. 그래서 그러한 변량을 판별함수(discriminant function) 또는 판별함수 변량(discriminant function variate)이라고 부른다.
#' 
#' 판별함수는 일단의 종속변수로부터 여러 개를 뽑아낼 수 있지만, 다중회귀에서는 모든 독립변수가 하나의 모형에 포함된다.
#' 
#' 판별함수들은 최대화(maximazation: 그룹들의 차이가 최대가 되는)라고 부르는 수학적 절차를 이용해서 구한다.
#' 
#' 첫 판별함수($V_{i}$)는 그룹들의 차이가 최대가 되는 종속변수들의 선형결합이다. 따라서 첫 판별함수에 대해서는 비체계적 변동에 대한 체계적 변동의 비 ($SS_{M} / SS_{R}$)가 최대가 된다.(이 비가 일변량분산분석의 `F비`에  대응됨을 기억하자.)
#' 
#' 따러서 첫 판별함수 변량에 대해서는 F비가 가능한 최대의 값이 된다.
#'
#' $$ y_{i} = b_{0} + b_{1} X_{1i} + b_{2} X_{2i} $$
#' 
#' $$ V_{1i} = b_{0} + b_{1} DependentVar_{1i} + b_{2} DependentVar_{2i} $$
#' 
#' $$ V_{1i} = b_{0} + b_{1} Actions_{i} + b_{2} Thoughts_{i} $$
#' 
#' 회귀에서는 최소제곱법으로 $b$ 값들을 구하지만 **판별함수 분석에서는 행렬 $HE^{-1}$의 고유벡터(eigenvector)들을 이용해서 판별함수 $b$를 구한다.** 이 때 $b_{0}$는 무시해도 된다. 그 값은 그냥 기하학 공간에서 변량의 위치를 나타낼 뿐, 그룹을 판별하는데 필요하지는 않기 때문이다.
#' 
#' 종속변수가 두 개 뿐이고 독립변수의 그룹이 둘 뿐인 상황에서는 판별함수 변량이 하나밖에 없다. 이렇게 되면 상황이 아주 간단해진다. 종속변수들 자체는 볼 필요없이 종속변수들의 판별함수만 보면 판별함수의 단일한 $SS_{M}/SS_{R}$ 값을 구할 수 있으며, 그 값으로 유의성을 판정할 수 있다.
#' 
#' 그러나 종속변수가 셋 이상이거나 독립변수의 수준이 셋 이상일 때는 변량이 둘 이상이 된다. 구할 수 있는 변량의 개수는 p(the number of dependent variables)와 k-1(the number of independent variable's level) 중 더 작은 것이다.
#' 
#' 변량을 서술하는 방정식의 $b$값들은 $HE^{-1}$ 행렬의 고유벡터들을 이용해서 구한다.(지금 예의 행렬에서는 두 개의 고유벡터를 뽑을 수 있다.) 
#' 
#' 한 고유벡터로는 첫 변량의 $b$를 구하고, 다른 고유벡터로는 둘째 변량의 $b$값들을 구한다.
#' 
#' 개념적으로 어떤 행렬의 고유벡터는 그 행렬을 대각행렬로 바꾸는 변환을 가해도 변하지 않는 벡터이다.
#' 
#' 대각행렬(diagonal matrix)이란 비대각성분들이 모두 0인 정방행렬이다. (따라서, 유의성 판정 시 고려할 값들이 보통의 정방행렬보다 적다.)
#' 
#' 그러므로, $HE^{-1}$ 행렬의 고유벡터들과 고유값들만 구하면 비체계적 변동에 대한 체계적 변동의 비를 구할 수 있다. (이들은 변환에 의해 변하지 않으므로)
#' 
#' 게다가, 행렬의 모든 성분을 고려할 필요가 없으므로 계산량도 적다.
#' 
#' 지금 예에서 $HE_{-1}$ 행렬의 고유벡터들은 다음과 같다. 
#' 
#' $$ eigenvector_{1}  = \begin{pmatrix} 0.603\\ -0.335 \end{pmatrix} $$
#' 
#' $$ eigenvector_{2}  = \begin{pmatrix} 0.425\\ -0.339 \end{pmatrix} $$
#' 
#' 이제 이상의 값들을 앞에 나온 공식에 대입하면 다음과 같은 판별함수 변량 공식들이 나온다.
#' 
#' $$V_{1i} = b_{0} + 0.603 Actions_{i} - 0.335 Thoughts_{i} $$
#' 
#' $$V_{2i} = b_{0} + 0.425 Actions_{i} + 0.339 Thoughts_{i} $$

ocdData %>%
  mutate(V1 = .603*Actions - .335*Thoughts) %>%
  mutate(V2 = .425*Actions + .339*Thoughts) -> ocdData_transformed

ocdData_transformed

#' ##### H
#' 
#' SSM_V1, CPM, SSM_V2을 구하여 $E$ 행렬을 만들어보자
#' 
#' ##### SSM_V1

ocdData_transformed %>%
  summarise(total.mean = mean(V1)) -> total_mean_v1

ocdData_transformed %>%
  group_by(Group) %>%
  summarise(mean.V1 = mean(V1)) -> ea_group_means_v1; ea_group_means_v1

vec.mean_v1 <- ea_group_means_v1$mean.V1

SSM_V1 <- 10*(vec.mean_v1[1] - total_mean_v1)^2 + 10*(vec.mean_v1[2] - total_mean_v1)^2 + 10*(vec.mean_v1[3] - total_mean_v1)^2 

SSM_V1 <- SSM_V1$total.mean

SSM_V1 # 9.61

#' ##### SSM_V2

ocdData_transformed %>%
  summarise(total.mean = mean(V2)) -> total_mean_v2

ocdData_transformed %>%
  group_by(Group) %>%
  summarise(mean.V2 = mean(V2)) -> ea_group_means_v2; ea_group_means_v2

vec.mean_v2 <- ea_group_means_v2$mean.V2

SSM_V2 <- 10*(vec.mean_v2[1] - total_mean_v2)^2 + 10*(vec.mean_v2[2] - total_mean_v2)^2 + 10*(vec.mean_v2[3] - total_mean_v2)^2 

SSM_V2 <- SSM_V2$total.mean

SSM_V2 # 6.30


#' ##### CPM

ocdData_transformed %>%
  summarise(mean.V1 = mean(V1)) %>% pull() -> mean_total_v1

ocdData_transformed %>%
  summarise(mean.V2 = mean(V2)) %>% pull() -> mean_total_v2

ocdData_transformed %>%
  group_by(Group) %>%
  summarise(mean.V1 = mean(V1),
            mean.V2 = mean(V2)) %>%
  ungroup() %>%
  mutate(d1 = mean.V1 - mean_total_v1) %>%
  mutate(d2 = mean.V2 - mean_total_v2) %>%
  mutate(d1_d2 = d1 * d2) %>%
  mutate(n_d1_d2 = 10*d1_d2) -> ocdData_transformed_cp_model

ocdData_transformed_cp_model %>%
  summarise(sum.n_d1_d2 = sum(n_d1_d2)) %>%
  pull() -> CPM; CPM # 0

#' 교차곱 CPM은 0이다. 

c(9.61, 0, 0, 1.96) %>% matrix(2, 2) -> H

H

#' ##### E
#' 
#' SSR_V1, CPR, SSR_V2을 구하여 $E$ 행렬을 만들어보자
#' 
#' ##### SSR_V1
#' 
ocdData_transformed %>%
  group_by(Group) %>%
  summarise(var.V1 = var(V1)) -> ea_group_v1_vars; ea_group_v1_vars


SSR_V1 <- ea_group_v1_vars$var.V1[1]*(10-1) + ea_group_v1_vars$var.V1[2]*(10-1) + ea_group_v1_vars$var.V1[3]*(10-1)

SSR_V1 # 26.98

#' ##### SSR_V2
#' 
ocdData_transformed %>%
  group_by(Group) %>%
  summarise(var.V2 = var(V2)) -> ea_group_V2_vars; ea_group_V2_vars

SSR_V2 <- ea_group_V2_vars$var.V2[1]*(10-1) + ea_group_V2_vars$var.V2[2]*(10-1) + ea_group_V2_vars$var.V2[3]*(10-1)

SSR_V2 # 26.99

#' ##### CPR 
#' 

ocdData_transformed %>%
  as_tibble() %>%
  group_by(Group) %>%
  mutate(d1 = V1 - mean(V1)) %>%
  mutate(d2 = V2 - mean(V2)) %>%
  mutate(d1_d2 = d1 * d2) -> ocdData_transformed_cp_residuals

ocdData_transformed_cp_residuals %>% 
  ungroup() %>%
  summarise(sum.d1_d2 = sum(d1_d2)) %>% pull() -> CPR; CPR # 0

#' 교차곱 CPR은 0이다.
#' 
c(26.98, 0, 0, 26.99) %>% matrix(2, 2) -> E

E

#' 교차곱들이 모두 0임을 알 수 있는데 이느 자료에서 뽑은 이 변량들이 모두 직교이기 때문이다. 이는 이 변량들이 서로 독립이라는 뜻이다. 즉, 추출한 변량들은 우리가 측정한 종속변수들의 선형결합으로부터 구축된 개별적인 차원들에 해당한다.
#' 
#' 이러한 자료 축약 기법에는 변량 점수들(독립변수들이 아니라)로 구축한 $HE^{-1}$ 행렬의 모든 비대각성분이 0이라는 매우 유용한 성질이 있음을 주목하자.
#' 
#' 일반적으로 종속변수가 $p$개 일때 축약을 거치지 않으면 비체계적 변동 대 체계적 변동의 비를 나타내는 값이 $p^{2}$이지만, 축약을 거치면 $p$가 된다.

H %*% solve(E)

#' 이러한 축약을 통해서 원래의 $HE^{-1}$ 행령의 고유값들이 드러난다. 이 행렬의 대각성분이 바로 고유값이다.

#' 다음으로는 이러한 값들이 전적으로 우연히 얻을 수 있는 값에 비해 얼마나 큰지 평가하는 네 가지 방법에 대해 알아본다.
#' 
#' 
#' ##### 필라이-바틀렛 대각합(V)
#' 
#' pilai-bartlett trace
#' 
#' $$ V = \sum_{i = 1}^{s} \frac{\lambda_{i}}{1 + \lambda_{i}}  $$
#' 
((.356) / (1 + .356)) + ((.073) / (1 + .073)) # .33

#' ##### 호텔링-롤리 대각합
#' 
#' Hotelling-Lawley trace
#' 
#' $$T = \sum_{i = 1}^{s} \lambda_{i}$$
#' 
#' 모든 변량의 고유값을 합친 것이다.
#' 
.356 + .073 # .429

#' ##### 윌크스 람다
#' 
#' $$\Lambda = \prod_{i = 1}^{s} \frac{1}{1+\lambda_{i}}$$
#' 
(1/(1+.356)) * (1/(1 + .073)) # .687

#' ##### 로이의 최대근
#' 
#' Roy's largest root
#' 
#' $$\theta = \lambda_{max}$$
#' 
.356

#' 로이의 최대근은 첫 판별함수 변량에 대한 설명되지 않는 변동 대 설명된 변동의 비를 나타낸다.
#' 
#' 이 값은 개념적으로 일변량분석의 F비와 같다. 많은 경우 이 검정통계량이 가장 강력하다.
#' 
#' #### 다변량분산분석 수행시 주의할 점
#' 
#' - 첫째, 검정의 가정들을 점검해야 한다.
#' - 둘째, 주 분석 결과의 유의성을 평가하는데 흔히 쓰이는 네 가지 방법 중 적절한 것을 골라야 한다.
#' - 셋째, 다변량분산분석 이후의 분석도 고민해보아야 한다. 분산분석에서처럼 다변량분산분석도 2단계 검정이다. 첫 단계에서는 전반적인 보편적인 검정을 수행하고, 그 다음으로 좀 더 구체적인 절차를 통해 특정 그룹들의 차이를 파악한다.
#' 
#' #### 검정의 가정들과 그 점검방법
#' 
#' - 독립성: 관측들이 통계적으로 독립이어야 한다.
#' - 임의표집: 해당 모집단이 무작위로 추출되고 구간 수준에서 측정한 자료이어야 한다.
#' - 다변량정규성: 각 그룹에서 종속변수들이 (집합적으로) 다변량 정규성을 가진다.
#' - 공분산행렬의 동질성: 다변량분석에서는 각 종속변수에 대해 그룹들의 분산들이 대체로 같을 뿐만 아니라, 임의의 두 종속변수의 상관관계가 모든 그룹에서 같다고 가정한다. 이러한 가정은 그룹들의 모집단 분산-공분산 행렬(variance-covariance matrices)들이 같은지로 점검한다.
#' 
#' 다변량정규성 가정은 샤피로 검정함수로 점검하거나 `mvoutlier::aq.plot()` 함수로 그래프로 확인한다.
#' 
#' 공분산행렬의 동질성 가정은 박스 검정(`Box's test`)으로 점검한다. 박스의 검정은 다변량 정규성의 위반에 영향을 크게 받는다. 즉, 다변량정규성이 성립하지 않으면 공분산행렬들이 비슷하더라도 유의한 검정결과가 나오기 쉽다. 또한, 표본이 크면 공분산행렬들이 대체로 비슷하더라도 박스 검정의 결과가 유의하게 나올 수 있다.
#' 
#' #### 검정통계량 선택
#' 
#' 그룹 차이들이 첫 변량애 집중되어 있을 때는(사회과학 연구에서는 그런 경우가 많다.), 로이의 통계량이 가장 검정력이 높음이 판명되었다.
#' 
#' 그룹들이 둘 이상의 변량에서 서로 다르면 검정력은 그 반대다. 즉 필라이 대각합이 가장 강하고 로이의 최대근이 가장 약하다.
#' 
#' 스티븐스는 표본 크기가 크지 않을 때는 종속변수를 10개 미만으로 유지하는 것이 좋다고 권한다.
#' 
#' 강건함의 관점에서 볼때 네 검정통계량은 다변량 정규성의 위반에 대해 비교적 강건하다. 단 로이의  최대근은 평점 분포에 영향을 받는다.
#' 
#' 또한 로이의 최대근은 공분산행렬의 동질성이 성립하지 않을 때는 강건하지 않다. 
#' 
#' 브레이와 맥스웰은 표본 크기들이 같으면 필라이-버틀렛 대각합이 가정들의 위반에 대해 가장 강건하다고 결론지었다. 그러나 표본 크기들이 같지 않으면 그 대각합은 공분산행렬의 동질성 가정의 위반에 영향을 받는ㄴ다. 
#' 
#' 정리하자면, 만일 그룹 크기들이 다르면 공분산행렬의 동질성을 점검한다. 만일 공분산행렬들이 같으면 그리고 다변량 정규성 가정이 성립한다면 필라이 대각합을 사용한다.
#' 
#' #### 후속분석
#' 
#' 전통적인 접근 방식은, 다변량분산분석이 유의하다는 결과가 나온 후에 각 종속변수에 대해 개별적인 분산분석을 수행하는 것이다.
#' 
#' 유의한 다변량분산분석 이후의 개별 분산분석을 그 다변량분산분석이 `보호한다`. 다른 말로 하면 전반적인 다변량 검정이 후속 개별 분산분석에서의 제1종 오류율의 상승을 방지해 준다. 어차피 주된 다변량 분산분석의 결과가 유의하지 않다면 이후의 개별 검정들은 무시하면 되기 때문이다.
#' 
#' 후속 분산분석은 모든 종속변수에 대해 수행하지만, 주된 다변량분산분석은 그룹 차이가 정말로 존재하는 종속변수만 보호할 뿐이다. 따라서 후속 분산분석에 본페로니 수정을 적용하는 것을 고려할 필요가 있다.
#' 
#' 주된 다변량분산분석 후에 후속 분산분석들을 수행할 때는, 다변량분산분석의 결과가 유의하게 나온 이유가 그룹들을 구분하는 바탕 차원들의 집합을 나타내는 종속변수들 때문이라고 가정한다. 그래서 어떤 연구자들은 그룹들을 가장 잘 분리하는(또는 판별하는) 종속 변수들의 선형결합들을 찾아내는 판별분석을 사용하는 것이 좋다고 주장한다. 
#' 
#' 판별분석은 실제로 종속변수들과 그룹 소속 사이의 관계를 밝히는 데 유용하다. 여러 번의 분산분석에 비한 이 접근 방식의 장점은 본질적이고 이론적인 차원들을 반영한다고 간주하는 바탕 차원들의 집합의 관점에서 종속변수들을 축약하고 설명한다는 점이다. 
#' 
