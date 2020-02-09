ch015(비모수적 검정)
================
jakinpilla
2020-02-09

  - [크러스컬-윌리스 검정을 위한 사후검정](#크러스컬-윌리스-검정을-위한-사후검정)

#### 크러스컬-윌리스 검정을 위한 사후검정

비모수적 사후 검정을 수행하는 한 가지 방법은 모든 가능한 비교에 대해 윌콕슨 순위합 검정을 수행하는 것이다. 이 방법은 서로
다른 그룹의 평균 순위와의 차이를 어떤 임계값과 비교하는 것이다. 그 임계값은 z 점수(수행하는 비교 횟수에 맞게
수정된)와 전체 표본 크기, 그리고 비교하는 두 그룹의 표본 크기에 기초한 상수로 계산한다.

  
![ |\\overline{R}\_{u}} - \\overline{R}\_{v}| \\ge
](https://latex.codecogs.com/png.latex?%20%7C%5Coverline%7BR%7D_%7Bu%7D%7D%20-%20%5Coverline%7BR%7D_%7Bv%7D%7C%20%5Cge%20
" |\\overline{R}_{u}} - \\overline{R}_{v}| \\ge ")  

  
![ z\_{\\alpha/k(k-1)} \\sqrt{ \\frac{N(N + 1)}{12} \\left(
\\frac{1}{n\_{u}} + \\frac{1}{n\_{v}} \\right)}
](https://latex.codecogs.com/png.latex?%20z_%7B%5Calpha%2Fk%28k-1%29%7D%20%5Csqrt%7B%20%5Cfrac%7BN%28N%20%2B%201%29%7D%7B12%7D%20%5Cleft%28%20%5Cfrac%7B1%7D%7Bn_%7Bu%7D%7D%20%2B%20%5Cfrac%7B1%7D%7Bn_%7Bv%7D%7D%20%5Cright%29%7D%20
" z_{\\alpha/k(k-1)} \\sqrt{ \\frac{N(N + 1)}{12} \\left( \\frac{1}{n_{u}} + \\frac{1}{n_{v}} \\right)} ")  

![|\\overline{R}\_{u}} -
\\overline{R}\_{v}|](https://latex.codecogs.com/png.latex?%7C%5Coverline%7BR%7D_%7Bu%7D%7D%20-%20%5Coverline%7BR%7D_%7Bv%7D%7C
"|\\overline{R}_{u}} - \\overline{R}_{v}|")은 그냥 비교하는 두 그룹의 평균 순위 차이의
크기이다.

우변에서 k는 그룹의 수이고, N은 전체표본크기이다.
![n\_{u}](https://latex.codecogs.com/png.latex?n_%7Bu%7D "n_{u}")는 비교하는
첫 그룹의 참가자 수이고 ![n\_{v}](https://latex.codecogs.com/png.latex?n_%7Bv%7D
"n_{v}")는 둘째 그룹의 참가자 수이다.

![\\alpha](https://latex.codecogs.com/png.latex?%5Calpha "\\alpha")는 통상
0.05이고 그룹의 수 k는 4이므로 ![\\alpha/k(k-1) = 0.05 / 4(4-1) =
.00417](https://latex.codecogs.com/png.latex?%5Calpha%2Fk%28k-1%29%20%3D%200.05%20%2F%204%284-1%29%20%3D%20.00417
"\\alpha/k(k-1) = 0.05 / 4(4-1) = .00417") 이다.

![z\_{.00417}](https://latex.codecogs.com/png.latex?z_%7B.00417%7D
"z_{.00417}")의 의미는 모든 z중 자신보다 큰 값의 비율이 .00417인 z 값의 의미이므로 정규분포 확률표를 참고하면
2.64를 근사값으로 얻을 수 있다.
