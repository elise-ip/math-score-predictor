# AP Score Analysis
# Chem 120 prediction

AP_AB <- read_excel("C:/Users/ebrad/Desktop/Stats Research/AP/AP.AB.xlsx")

AB = AP_AB[[1]]
GPA120 = AP_AB[[2]]

#A5 = 5 on AB test, A in Chem 120
#A4 = 4 on AB test, A in Chem 120
#A3 = 3 on AB test, A in Chem 120
#B5 = 5 on AB test, B in Chem 120
#B4 = 4 on AB test, B in Chem 120
#B3 = 3 on AB test, B in Chem 120
#C5 = 5 on AB test, C in Chem 120
#C4 = 4 on AB test, C in Chem 120
#C3 = 3 on AB test, C in Chem 120
#D5 = 5 on AB test, D in Chem 120
#D4 = 4 on AB test, D in Chem 120
#D3 = 3 on AB test, D in Chem 120
#F5 = 5 on AB test, F in Chem 120
#F4 = 4 on AB test, F in Chem 120
#F3 = 3 on AB test, F in Chem 120
# (not included when AB = 3 and BC = 5)

A5 = sum( (AB > 4 & GPA120 >= 3.7) == TRUE)
A4 = sum( (AB > 3 & AB < 5 & GPA120 >= 3.7) == TRUE)
A3 = sum( (AB < 4 & GPA120 >= 3.7) == TRUE)
B5 = sum( (AB > 4 & GPA120 >= 2.7 & GPA120 <= 3.3) == TRUE)
B4 = sum( (AB > 3 & AB < 5 & GPA120 >= 2.7 & GPA120 <= 3.3) == TRUE)
B3 = sum( (AB < 4 & GPA120 >= 2.7 & GPA120 <= 3.3) == TRUE)
C5 = sum( (AB > 4 & GPA120 >= 1.7 & GPA120 <= 2.3) == TRUE)
C4 = sum( (AB > 3 & AB < 5 & GPA120 >= 1.7 & GPA120 <= 2.3) == TRUE)
C3 = sum( (AB < 4 & GPA120 >= 1.7 & GPA120 <= 2.3) == TRUE)
D5 = sum( (AB > 4 & GPA120 >= 0.7 & GPA120 <= 1.3) == TRUE)
D4 = sum( (AB > 3 & AB < 5 & GPA120 >= 0.7 & GPA120 <= 1.3) == TRUE)
D3 = sum( (AB < 4 & GPA120 >= 0.7 & GPA120 <= 1.3) == TRUE)
F5 = sum( (AB > 4 & GPA120 < 0.7) == TRUE)
F4 = sum( (AB > 3 & AB < 5 & GPA120 < 0.7) == TRUE)
F3 = sum( (AB < 4 & GPA120 < 0.7) == TRUE)
m = matrix( c(A5, A4, A3, B5, B4, B3, C5, C4, C3, D5, D4, D3, F5, F4, F3), nrow=3, ncol=5)
m



AP_BC <- read_excel("AP/AP.BC.xlsx")

BC = AP_BC[[1]]
GPA120 = AP_BC[[2]]

#A5 = 5 on BC test, A in Chem 120
#A4 = 4 on BC test, A in Chem 120
#A3 = 3 on BC test, A in Chem 120
#B5 = 5 on BC test, B in Chem 120
#B4 = 4 on BC test, B in Chem 120
#B3 = 3 on BC test, B in Chem 120
#C5 = 5 on BC test, C in Chem 120
#C4 = 4 on BC test, C in Chem 120
#C3 = 3 on BC test, C in Chem 120
#D5 = 5 on BC test, D in Chem 120
#D4 = 4 on BC test, D in Chem 120
#D3 = 3 on BC test, D in Chem 120
#F5 = 5 on BC test, F in Chem 120
#F4 = 4 on BC test, F in Chem 120
#F3 = 3 on BC test, F in Chem 120
# (not included when BC = 3 and AB = 5)


A5 = sum( (BC > 4 & GPA120 >= 3.7) == TRUE)
A4 = sum( (BC > 3 & BC < 5 & GPA120 >= 3.7) == TRUE)
A3 = sum( (BC < 4 & GPA120 >= 3.7) == TRUE)
B5 = sum( (BC > 4 & GPA120 >= 2.7 & GPA120 <= 3.3) == TRUE)
B4 = sum( (BC > 3 & BC < 5 & GPA120 >= 2.7 & GPA120 <= 3.3) == TRUE)
B3 = sum( (BC < 4 & GPA120 >= 2.7 & GPA120 <= 3.3) == TRUE)
C5 = sum( (BC > 4 & GPA120 >= 1.7 & GPA120 <= 2.3) == TRUE)
C4 = sum( (BC > 3 & BC < 5 & GPA120 >= 1.7 & GPA120 <= 2.3) == TRUE)
C3 = sum( (BC < 4 & GPA120 >= 1.7 & GPA120 <= 2.3) == TRUE)
D5 = sum( (BC > 4 & GPA120 >= 0.7 & GPA120 <= 1.3) == TRUE)
D4 = sum( (BC > 3 & BC < 5 & GPA120 >= 0.7 & GPA120 <= 1.3) == TRUE)
D3 = sum( (BC < 4 & GPA120 >= 0.7 & GPA120 <= 1.3) == TRUE)
F5 = sum( (BC > 4 & GPA120 < 0.7) == TRUE)
F4 = sum( (BC > 3 & BC < 5 & GPA120 < 0.7) == TRUE)
F3 = sum( (BC < 4 & GPA120 < 0.7) == TRUE)
m = matrix( c(A5, A4, A3, B5, B4, B3, C5, C4, C3, D5, D4, D3, F5, F4, F3), nrow=3, ncol=5)
m


------------------------
  
#AP code for Calc prediction

AP_AB <- read_excel("AP/Calc2AB.xlsx")

AB = AP_AB[[1]]
GPAcalc2 = AP_AB[[2]]

#A5 = 5 on AB test, A in Calc 2
#A4 = 4 on AB test, A in Calc 2
#A3 = 3 on AB test, A in Calc 2
#B5 = 5 on AB test, B in Calc 2
#B4 = 4 on AB test, B in Calc 2
#B3 = 3 on AB test, B in Calc 2
#C5 = 5 on AB test, C in Calc 2
#C4 = 4 on AB test, C in Calc 2
#C3 = 3 on AB test, C in Calc 2
#D5 = 5 on AB test, D in Calc 2
#D4 = 4 on AB test, D in Calc 2
#D3 = 3 on AB test, D in Calc 2
#F5 = 5 on AB test, F in Calc 2
#F4 = 4 on AB test, F in Calc 2
#F3 = 3 on AB test, F in Calc 2


A5 = sum( (AB > 4 & GPAcalc2 >= 3.7) == TRUE)
A4 = sum( (AB > 3 & AB < 5 & GPAcalc2 >= 3.7) == TRUE)
A3 = sum( (AB < 4 & GPAcalc2 >= 3.7) == TRUE)
B5 = sum( (AB > 4 & GPAcalc2 >= 2.7 & GPAcalc2 <= 3.3) == TRUE)
B4 = sum( (AB > 3 & AB < 5 & GPAcalc2 >= 2.7 & GPAcalc2 <= 3.3) == TRUE)
B3 = sum( (AB < 4 & GPAcalc2 >= 2.7 & GPAcalc2 <= 3.3) == TRUE)
C5 = sum( (AB > 4 & GPAcalc2 >= 1.7 & GPAcalc2 <= 2.3) == TRUE)
C4 = sum( (AB > 3 & AB < 5 & GPAcalc2 >= 1.7 & GPAcalc2 <= 2.3) == TRUE)
C3 = sum( (AB < 4 & GPAcalc2 >= 1.7 & GPAcalc2 <= 2.3) == TRUE)
D5 = sum( (AB > 4 & GPAcalc2 >= 0.7 & GPAcalc2 <= 1.3) == TRUE)
D4 = sum( (AB > 3 & AB < 5 & GPAcalc2 >= 0.7 & GPAcalc2 <= 1.3) == TRUE)
D3 = sum( (AB < 4 & GPAcalc2 >= 0.7 & GPAcalc2 <= 1.3) == TRUE)
F5 = sum( (AB > 4 & GPAcalc2 < 0.7) == TRUE)
F4 = sum( (AB > 3 & AB < 5 & GPAcalc2 < 0.7) == TRUE)
F3 = sum( (AB < 4 & GPAcalc2 < 0.7) == TRUE)
m = matrix( c(A5, A4, A3, B5, B4, B3, C5, C4, C3, D5, D4, D3, F5, F4, F3), nrow=3, ncol=5)
m



AP_BC <- read_excel("AP/Calc3BC.xlsx")

BC = AP_BC[[1]]
GPAcalc3 = AP_BC[[2]]

#A5 = 5 on BC test, A in Calc 3
#A4 = 4 on BC test, A in Calc 3
#A3 = 3 on BC test, A in Calc 3
#B5 = 5 on BC test, B in Calc 3
#B4 = 4 on BC test, B in Calc 3
#B3 = 3 on BC test, B in Calc 3
#C5 = 5 on BC test, C in Calc 3
#C4 = 4 on BC test, C in Calc 3
#C3 = 3 on BC test, C in Calc 3
#D5 = 5 on BC test, D in Calc 3
#D4 = 4 on BC test, D in Calc 3
#D3 = 3 on BC test, D in Calc 3
#F5 = 5 on BC test, F in Calc 3
#F4 = 4 on BC test, F in Calc 3
#F3 = 3 on BC test, F in Calc 3


A5 = sum( (BC > 4 & GPAcalc3 >= 3.7) == TRUE)
A4 = sum( (BC > 3 & BC < 5 & GPAcalc3 >= 3.7) == TRUE)
A3 = sum( (BC < 4 & GPAcalc3 >= 3.7) == TRUE)
B5 = sum( (BC > 4 & GPAcalc3 >= 2.7 & GPAcalc3 <= 3.3) == TRUE)
B4 = sum( (BC > 3 & BC < 5 & GPAcalc3 >= 2.7 & GPAcalc3 <= 3.3) == TRUE)
B3 = sum( (BC < 4 & GPAcalc3 >= 2.7 & GPAcalc3 <= 3.3) == TRUE)
C5 = sum( (BC > 4 & GPAcalc3 >= 1.7 & GPAcalc3 <= 2.3) == TRUE)
C4 = sum( (BC > 3 & BC < 5 & GPAcalc3 >= 1.7 & GPAcalc3 <= 2.3) == TRUE)
C3 = sum( (BC < 4 & GPAcalc3 >= 1.7 & GPAcalc3 <= 2.3) == TRUE)
D5 = sum( (BC > 4 & GPAcalc3 >= 0.7 & GPAcalc3 <= 1.3) == TRUE)
D4 = sum( (BC > 3 & BC < 5 & GPAcalc3 >= 0.7 & GPAcalc3 <= 1.3) == TRUE)
D3 = sum( (BC < 4 & GPAcalc3 >= 0.7 & GPAcalc3 <= 1.3) == TRUE)
F5 = sum( (BC > 4 & GPAcalc3 < 0.7) == TRUE)
F4 = sum( (BC > 3 & BC < 5 & GPAcalc3 < 0.7) == TRUE)
F3 = sum( (BC < 4 & GPAcalc3 < 0.7) == TRUE)
m = matrix( c(A5, A4, A3, B5, B4, B3, C5, C4, C3, D5, D4, D3, F5, F4, F3), nrow=3, ncol=5)
m



