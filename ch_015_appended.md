ch015(비모수적 검정)
================
jakinpilla
2020-03-12

  - [총 SSCP 행렬(T)](#총-sscp-행렬t)

종속변수들의 관계: 교차곱

세가지 교차곱: 총교차곱, 모형교차곱, 잔차교차곱…

총교차곱

  
![ CP\_{T} = \\sum\_{i=1}^{n} \\left( x\_{i(Actions)} -
\\overline{X}\_{total(Actions)} \\right) \\left( x\_{i(Thoughts)} -
\\overline{X}\_{total(Thoughts)}
\\right)](https://latex.codecogs.com/png.latex?%20CP_%7BT%7D%20%3D%20%5Csum_%7Bi%3D1%7D%5E%7Bn%7D%20%5Cleft%28%20x_%7Bi%28Actions%29%7D%20-%20%5Coverline%7BX%7D_%7Btotal%28Actions%29%7D%20%5Cright%29%20%5Cleft%28%20x_%7Bi%28Thoughts%29%7D%20-%20%5Coverline%7BX%7D_%7Btotal%28Thoughts%29%7D%20%5Cright%29
" CP_{T} = \\sum_{i=1}^{n} \\left( x_{i(Actions)} - \\overline{X}_{total(Actions)} \\right) \\left( x_{i(Thoughts)} - \\overline{X}_{total(Thoughts)} \\right)")  

  
![ CP\_{M} = \\sum\_{grp=1}^{k} n \\left\[ \\left(
\\overline{x}\_{grp(Actions)} - \\overline{X}\_{total(Actions)}}
\\right) \\left( \\overline{x}\_{grp(Thoughts)} -
\\overline{X}\_{total(Thoughts)}} \\right) \\right\]
](https://latex.codecogs.com/png.latex?%20CP_%7BM%7D%20%3D%20%5Csum_%7Bgrp%3D1%7D%5E%7Bk%7D%20n%20%5Cleft%5B%20%5Cleft%28%20%5Coverline%7Bx%7D_%7Bgrp%28Actions%29%7D%20-%20%5Coverline%7BX%7D_%7Btotal%28Actions%29%7D%7D%20%5Cright%29%20%5Cleft%28%20%5Coverline%7Bx%7D_%7Bgrp%28Thoughts%29%7D%20-%20%5Coverline%7BX%7D_%7Btotal%28Thoughts%29%7D%7D%20%5Cright%29%20%5Cright%5D%20
" CP_{M} = \\sum_{grp=1}^{k} n \\left[ \\left( \\overline{x}_{grp(Actions)} - \\overline{X}_{total(Actions)}} \\right) \\left( \\overline{x}_{grp(Thoughts)} - \\overline{X}_{total(Thoughts)}} \\right) \\right] ")  

  
![ CP\_{R} = \\sum\_{i=1}^{n} \\left( x\_{i(Actions)} -
\\overline{X}\_{grp(Actions)} \\right) \\left( x\_{i(Thoughts)} -
\\overline{X}\_{grp(Thoughts)}
\\right)](https://latex.codecogs.com/png.latex?%20CP_%7BR%7D%20%3D%20%5Csum_%7Bi%3D1%7D%5E%7Bn%7D%20%5Cleft%28%20x_%7Bi%28Actions%29%7D%20-%20%5Coverline%7BX%7D_%7Bgrp%28Actions%29%7D%20%5Cright%29%20%5Cleft%28%20x_%7Bi%28Thoughts%29%7D%20-%20%5Coverline%7BX%7D_%7Bgrp%28Thoughts%29%7D%20%5Cright%29
" CP_{R} = \\sum_{i=1}^{n} \\left( x_{i(Actions)} - \\overline{X}_{grp(Actions)} \\right) \\left( x_{i(Thoughts)} - \\overline{X}_{grp(Thoughts)} \\right)")  

#### 총 SSCP 행렬(T)

|              |                                      열 1 Actions                                       |                                       열 2 Thoughts                                        |
| :----------: | :------------------------------------------------------------------------------------: | :---------------------------------------------------------------------------------------: |
| 행 1 Actions  | ![SS\_{Actions}](https://latex.codecogs.com/png.latex?SS_%7BActions%7D "SS_{Actions}") |           ![CP\_{T}](https://latex.codecogs.com/png.latex?CP_%7BT%7D "CP_{T}")            |
| 행 2 Thoughts |          ![CP\_{T}](https://latex.codecogs.com/png.latex?CP_%7BT%7D "CP_{T}")          | ![SS\_{Thoughts}](https://latex.codecogs.com/png.latex?SS_%7BThoughts%7D "SS_{Thoughts}") |

  
![ T = \\begin{pmatrix} 61.47 & 5.44\\\\
5.44 & 141.47
\\end{pmatrix}](https://latex.codecogs.com/png.latex?%20T%20%20%3D%20%5Cbegin%7Bpmatrix%7D%2061.47%20%26%205.44%5C%5C%0A5.44%20%26%20141.47%0A%5Cend%7Bpmatrix%7D
" T  = \\begin{pmatrix} 61.47 & 5.44\\\\
5.44 & 141.47
\\end{pmatrix}")  

  
![\\chi^{2} =
\\frac{O}{M}](https://latex.codecogs.com/png.latex?%5Cchi%5E%7B2%7D%20%3D%20%5Cfrac%7BO%7D%7BM%7D
"\\chi^{2} = \\frac{O}{M}")  

  
![ln(O\_{food and then dance}) = b\_{0} + b\_{1}
\\times 0](https://latex.codecogs.com/png.latex?ln%28O_%7Bfood%20and%20then%20dance%7D%29%20%3D%20b_%7B0%7D%20%2B%20b_%7B1%7D%20%5Ctimes%200
"ln(O_{food and then dance}) = b_{0} + b_{1} \\times 0")
