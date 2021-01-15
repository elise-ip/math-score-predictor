# M140 vs Math 140 GPA
# library(aod)
library(readxl)
install.packages("ggplot2")
library("ggplot2")
M_140 <- read_excel("C:/Users/ebrad/Desktop/Stats Research/Math 140/M_140.xlsx")

# DEFINE COLUMNS
M140 = M_140[[1]]
GPA140 = M_140[[2]]

#linear regression
fit = lm(GPA140 ~ M140, data=M_140)
summary(fit)
ggplot(M_140, aes(M140, GPA140)) + ggtitle("M140 vs GPA140") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(name="M140", limits=c(25, 65)) + scale_y_continuous(name="GPA140", limits=c(0, 4)) + geom_point() + geom_smooth(method = "lm", se=FALSE, colour="purple") 


#logistic regression for passing
pass140 = as.integer(GPA140 >= 1.7)
M_140$pass140=pass140

mylogit <- glm(pass140 ~ M140, data = M_140, family = "binomial")
summary(mylogit)

#prediction for passing
x <- data.frame(M140 = 42)
p <- predict(mylogit, x)
p

#logistic regression for B- and above
succeed140 = as.integer(GPA140 >= 2.7)
M_140$succeed140=succeed140

mylogit_b <- glm(succeed140 ~ M140, data = M_140, family = "binomial")
summary(mylogit_b)

#prediction for B- and above
x <- data.frame(M140 = 42)
s <- predict(mylogit_b, x)
s



# 2x2 Contingency Tables

# pp = pass Math 140, pass M140
# pf = pass Math 140, fail M140
# fp = fail Math 140, pass M140
# ff = fail Math 140, fail M140
pp = sum( (GPA140 >= 1.7 & M140 >= 42) == TRUE)
pf = sum( (GPA140 >= 1.7 & M140 < 42) == TRUE)
fp = sum( (GPA140 < 1.7 & M140 >= 42) == TRUE)
ff=  sum( (GPA140 < 1.7 & M140 < 42) == TRUE)
m = matrix( c(pp, pf, fp, ff), nrow=2, ncol=2)
m
chisq.test(m, correct=FALSE)
#p = 0.1194



# 3x2 Contingency Table

# pa = pass M140, A,B in Math 140
# pc = pass M140, C in Math 140
# pf = pass M140, fail Math 140 
# fa = fail M140, A,B in Math 140
# fc = fail M140, C in Math 140
# ff = fail M140, fail Math 140

# M140 cutoff = 42
pa = sum( (GPA140 >= 2.7 & GPA140 <= 4.0 & M140 >= 42) == TRUE)
pc = sum( (GPA140 >= 1.7 & GPA140 < 2.7 & M140 >= 42) == TRUE)
pf = sum( (GPA140 < 1.7 & M140 >= 42) == TRUE)
fa = sum( (GPA140 >= 2.7 & GPA140 <= 4.0 & M140 < 42) == TRUE)
fc = sum( (GPA140 >= 1.7 & GPA140 < 2.7 & M140 < 42) == TRUE)
ff=  sum( (GPA140 < 1.7 & M140 < 42) == TRUE)
m = matrix( c(pa, pc, pf, fa, fc, ff), nrow=3, ncol=2)
m
chisq.test(m, correct=FALSE)
#p-value = 0.03661


#--------------------------------------------

# MAX SAT/ACT vs Math 140 GPA
library("ggplot2")
MAX140 <- read_excel('C:/Users/ebrad/Desktop/Stats Research/Math 140/MAX140.xlsx')

# DEFINE COLUMNS
MAX = MAX140[[1]]
GPA140 = MAX140[[2]]

#linear regression
fit = lm(GPA140 ~ MAX, data=MAX140)
summary(fit)

ggplot(MAX140, aes(MAX, GPA140)) + ggtitle("MAX vs GPA140")  + theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(name="MAX", limits=c(15, 36)) + scale_y_continuous(name="GPA140", limits=c(0, 4)) + geom_point() + geom_smooth(method = "lm", se=FALSE, colour="blue") 

#logistic regression for passing
pass140 = as.integer(GPA140 >= 1.7)
MAX140$pass140=pass140

mylogit <- glm(pass140 ~ MAX, data = MAX140, family = "binomial")
summary(mylogit)

#prediction for passing
x <- data.frame(MAX = 24)
p <- predict(mylogit, x)
p

#logistic regression for B- and above
succeed140 = as.integer(GPA140 >= 2.7)
MAX140$succeed140=succeed140

mylogit_b <- glm(succeed140 ~ MAX, data = MAX140, family = "binomial")
summary(mylogit_b)

#prediction for B- and above
x <- data.frame(MAX = 24)
s <- predict(mylogit_b, x)
s


# 2x2 Contingency Tables

# pp = pass Math 140, pass MAX SAT/ACT
# pf = pass Math 140, fail MAX SAT/ACT
# fp = fail Math 140, pass MAX SAT/ACT
# ff = fail Math 140, fail MAX SAT/ACT


pp_24 = sum( (GPA140 >= 1.7 & MAX >= 24) == TRUE)
pf_24 = sum( (GPA140 >= 1.7 & MAX < 24) == TRUE)
fp_24 = sum( (GPA140 < 1.7 & MAX >= 24) == TRUE)
ff_24 =  sum( (GPA140 < 1.7 & MAX < 24) == TRUE)
m_24 = matrix( c(pp_24, pf_24, fp_24, ff_24), nrow=2, ncol=2)
m_24
chisq.test(m_24, correct=FALSE)
#p = 1.84e-06
# 
# 
# pp = sum( (GPA140 >= 1.7 & MAX >= 26) == TRUE)
# pf = sum( (GPA140 >= 1.7 & MAX < 26) == TRUE)
# fp = sum( (GPA140 < 1.7 & MAX >= 26) == TRUE)
# ff =  sum( (GPA140 < 1.7 & MAX < 26) == TRUE)
# m = matrix( c(pp, pf, fp, ff), nrow=2, ncol=2)
# m_26 = m
# chisq.test(m_26, correct=FALSE)
# #p = 0.0001641
# 
# pp = sum( (GPA140 >= 1.7 & MAX >= 27) == TRUE)
# pf = sum( (GPA140 >= 1.7 & MAX < 27) == TRUE)
# fp = sum( (GPA140 < 1.7 & MAX >= 27) == TRUE)
# ff =  sum( (GPA140 < 1.7 & MAX < 27) == TRUE)
# m = matrix( c(pp, pf, fp, ff), nrow=2, ncol=2)
# m_27 = m
# chisq.test(m_27, correct=FALSE)
# #p = 3.45e-05


# 3x2 Contingency Table

# pa = pass MAX SAT/ACT, A,B in Math 140
# pc = pass MAX SAT/ACT, C in Math 140
# pf = pass MAX SAT/ACT, fail Math 140 
# fa = fail MAX SAT/ACT, A,B in Math 140
# fc = fail MAX SAT/ACT, C in Math 140
# ff = fail MAX SAT/ACT, fail Math 140

# MAX cutoff = 24
pa = sum( (GPA140 >= 2.7 & GPA140 <= 4.0 & MAX >= 24) == TRUE)
pc = sum( (GPA140 >= 1.7 & GPA140 < 2.7 & MAX >= 24) == TRUE)
pf = sum( (GPA140 < 1.7 & MAX >= 24) == TRUE)
fa = sum( (GPA140 >= 2.7 & GPA140 <= 4.0 & MAX < 24) == TRUE)
fc = sum( (GPA140 >= 1.7 & GPA140 < 2.7 & MAX < 24) == TRUE)
ff=  sum( (GPA140 < 1.7 & MAX < 24) == TRUE)
m = matrix( c(pa, pc, pf, fa, fc, ff), nrow=3, ncol=2)
m
chisq.test(m, correct=FALSE)
#p-value = 3.91e-07

#----------------------------------------------------

# Score25 vs Math 140 GPA
library("ggplot2")
Score25_140 <- read_excel("Score25_140.xlsx", sheet = "GPA140 vs Score25")

# DEFINE COLUMNS
Score25 = Score25_140[[1]]
GPA140 = Score25_140[[2]]

#linear regression
fit = lm(GPA140 ~ Score25, data=Score25_140)
summary(fit)

ggplot(Score25_140, aes(Score25, GPA140)) + ggtitle("Score25 vs GPA140")  + theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(name="Score25", limits=c(4, 26)) + scale_y_continuous(name="GPA140", limits=c(0, 4)) + geom_point() + geom_smooth(method = "lm", se=FALSE, colour="yellow") 

#logistic regression for passing
pass140 = as.integer(GPA140 >= 1.7)
Score25_140$pass140=pass140

mylogit <- glm(pass140 ~ Score25, data = Score25_140, family = "binomial")
summary(mylogit)

#prediction for passing
x <- data.frame(Score25 = 18)
p <- predict(mylogit, x)
p

#logistic regression for B- and above
succeed140 = as.integer(GPA140 >= 2.7)
Score25_140$succeed140=succeed140

mylogit_b <- glm(succeed140 ~ Score25, data = Score25_140, family = "binomial")
summary(mylogit_b)

#prediction for B- and above
x <- data.frame(Score25 = 18)
s <- predict(mylogit_b, x)
s

# 2x2 Contingency Tables


# pp = pass Math 140, pass Score25
# pf = pass Math 140, fail Score25
# fp = fail Math 140, pass Score25
# ff = fail Math 140, fail Score25

# Cutoff Score25 >= 18 
pp_18 = sum( (GPA140 >= 1.7 & Score25 >= 18) == TRUE)
pf_18 = sum( (GPA140 >= 1.7 & Score25 < 18) == TRUE)
fp_18 = sum( (GPA140 < 1.7 & Score25 >= 18) == TRUE)
ff_18 =  sum( (GPA140 < 1.7 & Score25 < 18) == TRUE)
m_18 = matrix( c(pp_18, pf_18, fp_18, ff_18), nrow=2, ncol=2)
m_18
chisq.test(m_18, correct=FALSE)
#p = 1.974e-06 
# 
# # Cutoff Score25 >= 17
# pp_17 = sum( (GPA140 >= 1.7 & Score25 >= 17) == TRUE)
# pf_17 = sum( (GPA140 >= 1.7 & Score25 < 17) == TRUE)
# fp_17 = sum( (GPA140 < 1.7 & Score25 >= 17) == TRUE)
# ff_17 =  sum( (GPA140 < 1.7 & Score25 < 17) == TRUE)
# m_17 = matrix( c(pp_17, pf_17, fp_17, ff_17), nrow=2, ncol=2)
# m_17
# chisq.test(m_17, correct=FALSE)
# #p = 0.0002123
# 
# # Cutoff Score25 >= 19
# pp_19 = sum( (GPA140 >= 1.7 & Score25 >= 19) == TRUE)
# pf_19 = sum( (GPA140 >= 1.7 & Score25 < 19) == TRUE)
# fp_19 = sum( (GPA140 < 1.7 & Score25 >= 19) == TRUE)
# ff_19 =  sum( (GPA140 < 1.7 & Score25 < 19) == TRUE)
# m_19 = matrix( c(pp_19, pf_19, fp_19, ff_19), nrow=2, ncol=2)
# m_19
# chisq.test(m_19, correct=FALSE)
# #p = 0.0001437

# 3x2 Contingency Table

# pa = pass Score25, A,B in Math 140
# pc = pass Score25, C in Math 140
# pf = pass Score25, fail Math 140 
# fa = fail Score25, A,B in Math 140
# fc = fail Score25, C in Math 140
# ff = fail Score25, fail Math 140

# Score25 cutoff = 18
pa = sum( (GPA140 >= 2.7 & GPA140 <= 4.0 & Score25 >= 18) == TRUE)
pc = sum( (GPA140 >= 1.7 & GPA140 < 2.7 & Score25 >= 18) == TRUE)
pf = sum( (GPA140 < 1.7 & Score25 >= 18) == TRUE)
fa = sum( (GPA140 >= 2.7 & GPA140 <= 4.0 & Score25 < 18) == TRUE)
fc = sum( (GPA140 >= 1.7 & GPA140 < 2.7 & Score25 < 18) == TRUE)
ff=  sum( (GPA140 < 1.7 & Score25 < 18) == TRUE)
m = matrix( c(pa, pc, pf, fa, fc, ff), nrow=3, ncol=2)
m
chisq.test(m, correct=FALSE)
#p-value = 2.369e-07


#------------------------------------

# College Algebra GPA vs Math 140 GPA
library("ggplot2")
CA140 <- read_excel("CA140.xlsx", sheet = "GPA140 vs GPAca")

# DEFINE COLUMNS
GPAca = CA140[[1]]
GPA140 = CA140[[2]]

#linear regression
fit = lm(GPA140 ~ GPAca, data=CA140)
summary(fit)

ggplot(CA140, aes(GPAca, GPA140)) + ggtitle("GPAca vs GPA140")  + theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(name="GPAca", limits=c(0, 4)) + scale_y_continuous(name="GPA140", limits=c(0, 4)) + geom_point() + geom_smooth(method = "lm", se=FALSE, colour="green") 

#ca_p is those who passed college algebra and passed math 140
#ca_t is those who passed college algebra and took math 140
ca_p = sum( (GPAca >= 1.7 & GPA140 >= 1.7) == TRUE)
ca_p
ca_t = sum( (GPAca >= 1.7 & GPA140 >= 0) == TRUE)
ca_t
p = ca_p / ca_t
p

#ca_b is those who passed college algebra and >= B- in math 140
#ca_t is those who passed college algebra and took math 140
ca_b = sum( (GPAca >= 1.7 & GPA140 >= 2.7) == TRUE)
ca_b
ca_t = sum( (GPAca >= 1.7 & GPA140 >= 0) == TRUE)
ca_t
b = ca_b / ca_t
b

#logistic regression for passing
pass140 = as.integer(GPA140 >= 1.7)
CA140$pass140=pass140

mylogit <- glm(pass140 ~ GPAca, data = CA140, family = "binomial")
summary(mylogit)

#prediction for passing
x <- data.frame(GPAca = 1.7)
p <- predict(mylogit, x)
p

#logistic regression for B- and above
succeed140 = as.integer(GPA140 >= 2.7)
CA140$succeed140=succeed140

mylogit_b <- glm(succeed140 ~ GPAca, data = CA140, family = "binomial")
summary(mylogit_b)

#prediction for B- and above
x <- data.frame(GPAca = 1.7)
s <- predict(mylogit_b, x)
s



