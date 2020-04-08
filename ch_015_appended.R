#' ---
#' title: "ch015(비모수적 검정)"
#' author: "jakinpilla"
#' date : "`r format(Sys.time(), '%Y-%m-%d')`"
#' output: 
#'    github_document : 
#'        pandoc_args: --webtex
#'        toc : true
#' ---


#' 종속변수들의 관계: 교차곱
#' 
#' 세가지 교차곱: 총교차곱, 모형교차곱, 잔차교차곱...
#' 
#' 총교차곱
#' 
#' $$ CP_{T} = \sum_{i=1}^{n} \left( x_{i(Actions)} - \overline{X}_{total(Actions)} \right) \left( x_{i(Thoughts)} - \overline{X}_{total(Thoughts)} \right)$$

#' 
#' $$ CP_{M} = \sum_{grp=1}^{k} n \left[ \left( \overline{x}_{grp(Actions)} - \overline{X}_{total(Actions)}} \right) \left( \overline{x}_{grp(Thoughts)} - \overline{X}_{total(Thoughts)}} \right) \right] $$
#' 

#' $$ CP_{R} = \sum_{i=1}^{n} \left( x_{i(Actions)} - \overline{X}_{grp(Actions)} \right) \left( x_{i(Thoughts)} - \overline{X}_{grp(Thoughts)} \right)$$
#' 
#' 
#' 
#' #### 총 SSCP 행렬(T)
#' 
#' |   |  열 1 Actions | 열 2 Thoughts   |
#' |:----:|:----:|:----:|
#' | 행 1 Actions | $SS_{Actions}$ | $CP_{T}$ |
#' | 행 2 Thoughts | $CP_{T}$ | $SS_{Thoughts}$ |
#' 
#' 
#' $$ T  = \begin{pmatrix} 61.47 & 5.44\\
#' 5.44 & 141.47
#' \end{pmatrix}$$
#' 
#' 
#' $$\chi^{2} = \frac{O}{M}$$
#' 
#' $$ln(O_{food and then dance}) = b_{0} + b_{1} \times 0$$

