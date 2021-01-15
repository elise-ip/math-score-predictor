# M150 vs Science GPA
library("ggplot2")
ScienceM150 <- read_excel("C:/Users/ebrad/Desktop/Stats Research/Science/ScienceM150.xlsx")


# DEFINE COLUMNS
M150 = ScienceM150[[1]]
GPA120 = ScienceM150[[2]]

#linear regression
fit = lm(GPA120 ~ M150, data=ScienceM150)
summary(fit)
ggplot(ScienceM150, aes(M150, GPA120)) + ggtitle("M150 vs GPA120") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(name="M150", limits=c(30, 68)) + scale_y_continuous(name="GPA120", limits=c(0, 4)) + geom_point() + geom_smooth(method = "lm", se=FALSE, colour="maroon1") 

#logistic regression for passing
pass150 = as.integer(GPA120 >= 1.7)
ScienceM150$pass150=pass150

mylogit <- glm(pass150 ~ M150, data = ScienceM150, family = "binomial")
summary(mylogit)

#prediction for passing
x <- data.frame(M150 = 49)
p <- predict(mylogit, x)
p

#logistic regression for B- and above
succeed150 = as.integer(GPA120 >= 2.7)
ScienceM150$succeed150=succeed150

mylogit_b <- glm(succeed150 ~ M150, data = ScienceM150, family = "binomial")
summary(mylogit_b)

#prediction for B- and above
x <- data.frame(M150 = 49)
s <- predict(mylogit_b, x)
s


# 3x2 Contingency Table

# pa = pass M150, A,B in Chem 120
# pc = pass M150, C in Chem 120
# pf = pass M150, fail Chem 120 
# fa = fail M150, A,B in Chem 120
# fc = fail M150, C in Chem 120
# ff = fail M150, fail Chem 120

# M150 cutoff = 49
pa = sum( (GPA120 >= 2.7 & GPA120 <= 4.0 & M150 >= 49) == TRUE)
pc = sum( (GPA120 >= 1.7 & GPA120 < 2.7 & M150 >= 49) == TRUE)
pf = sum( (GPA120 < 1.7 & M150 >= 49) == TRUE)
fa = sum( (GPA120 >= 2.7 & GPA120 <= 4.0 & M150 < 49) == TRUE)
fc = sum( (GPA120 >= 1.7 & GPA120 < 2.7 & M150 < 49) == TRUE)
ff=  sum( (GPA120 < 1.7 & M150 < 49) == TRUE)
m = matrix( c(pa, pc, pf, fa, fc, ff), nrow=3, ncol=2)
m
chisq.test(m, correct=FALSE)
#p-value = 1.046e-07


# 2x2 Contingency Tables

# pp = pass Chem 120, pass M150
# pf = pass Chem 120, fail M150
# fp = fail Chem 120, pass M150
# ff = fail Chem 120, fail M150

# # #M150 cutoff score = 47
# pp = sum( (GPA120 >= 1.7 & M150 >= 47) == TRUE)
# pf = sum( (GPA120 >= 1.7 & M150 < 47) == TRUE)
# fp = sum( (GPA120 < 1.7 & M150 >= 47) == TRUE)
# ff=  sum( (GPA120 < 1.7 & M150 < 47) == TRUE)
# m = matrix( c(pp, pf, fp, ff), nrow=2, ncol=2)
# m
# chisq.test(m, correct=FALSE)
# #p = 0.0002534
# 
# # Testing:
# #M150 cutoff score = 46
# pp = sum( (GPA120 >= 1.7 & M150 >= 46) == TRUE)
# pf = sum( (GPA120 >= 1.7 & M150 < 46) == TRUE)
# fp = sum( (GPA120 < 1.7 & M150 >= 46) == TRUE)
# ff=  sum( (GPA120 < 1.7 & M150 < 46) == TRUE)
# m = matrix( c(pp, pf, fp, ff), nrow=2, ncol=2)
# m
# chisq.test(m, correct=FALSE)
# #p = 0.007164

#M150 cutoff score = 49 BEST
pp = sum( (GPA120 >= 1.7 & M150 >= 49) == TRUE)
pf = sum( (GPA120 >= 1.7 & M150 < 49) == TRUE)
fp = sum( (GPA120 < 1.7 & M150 >= 49) == TRUE)
ff=  sum( (GPA120 < 1.7 & M150 < 49) == TRUE)
m = matrix( c(pp, pf, fp, ff), nrow=2, ncol=2)
m
chisq.test(m, correct=FALSE)
# p = 4.009e-06


#--------------------------------------------

# MAX SAT/ACT vs Science GPA
library("ggplot2")
ScienceMAX <- read_excel("C:/Users/ebrad/Desktop/Stats Research/Science/ScienceMAX.xlsx")

# DEFINE COLUMNS
MAX = ScienceMAX[[1]]
GPA120 = ScienceMAX[[2]]

#linear regression
fit = lm(GPA120 ~ MAX, data=ScienceMAX)
summary(fit)
ggplot(ScienceMAX, aes(MAX, GPA120)) + ggtitle("MAX vs GPA120") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(name="MAX", limits=c(17, 36)) + scale_y_continuous(name="GPA120", limits=c(0, 4)) + geom_point() + geom_smooth(method = "lm", se=FALSE, colour="deepskyblue") 

#logistic regression for passing
pass120 = as.integer(GPA120 >= 1.7)
ScienceMAX$pass120=pass120

mylogit <- glm(pass120 ~ MAX, data = ScienceMAX, family = "binomial")
summary(mylogit)

#prediction for passing
x <- data.frame(MAX = 27)
p <- predict(mylogit, x)
p

#logistic regression for B- and above
succeed120 = as.integer(GPA120 >= 2.7)
ScienceMAX$succeed120=succeed120

mylogit_b <- glm(succeed120 ~ MAX, data = ScienceMAX, family = "binomial")
summary(mylogit_b)

#prediction for B- and above
x <- data.frame(MAX = 27)
s <- predict(mylogit_b, x)
s


# 3x2 Contingency Tables


# pa = pass MAX SAT/ACT, A,B in Math 140
# pc = pass MAX SAT/ACT, C in Math 140
# pf = pass MAX SAT/ACT, fail Math 140 
# fa = fail MAX SAT/ACT, A,B in Math 140
# fc = fail MAX SAT/ACT, C in Math 140
# ff = fail MAX SAT/ACT, fail Math 140

# MAX cutoff = 27
pa = sum( (GPA120 >= 2.7 & GPA120 <= 4.0 & MAX >= 27) == TRUE)
pc = sum( (GPA120 >= 1.7 & GPA120 < 2.7 & MAX >= 27) == TRUE)
pf = sum( (GPA120 < 1.7 & MAX >= 27) == TRUE)
fa = sum( (GPA120 >= 2.7 & GPA120 <= 4.0 & MAX < 27) == TRUE)
fc = sum( (GPA120 >= 1.7 & GPA120 < 2.7 & MAX < 27) == TRUE)
ff=  sum( (GPA120 < 1.7 & MAX < 27) == TRUE)
m = matrix( c(pa, pc, pf, fa, fc, ff), nrow=3, ncol=2)
m
chisq.test(m, correct=FALSE)
#p-value < 2.2e-16


# 2x2 Contingency Tables

# pp = pass Chem 120, pass MAX
# pf = pass Chem 120, fail MAX
# fp = fail Chem 120, pass MAX
# ff = fail Chem 120, fail MAX

# #MAX cutoff score = 27
pp = sum( (GPA120 >= 1.7 & MAX >= 27) == TRUE)
pf = sum( (GPA120 >= 1.7 & MAX < 27) == TRUE)
fp = sum( (GPA120 < 1.7 & MAX >= 27) == TRUE)
ff=  sum( (GPA120 < 1.7 & MAX < 27) == TRUE)
m = matrix( c(pp, pf, fp, ff), nrow=2, ncol=2)
m
chisq.test(m, correct=FALSE)
# diag = pf + fp
# diag
# #p = 0.001092

# 
# #M150 cutoff score = 32
# pp = sum( (GPA120 >= 1.7 & MAX >= 32) == TRUE)
# pf = sum( (GPA120 >= 1.7 & MAX < 32) == TRUE)
# fp = sum( (GPA120 < 1.7 & MAX >= 32) == TRUE)
# ff=  sum( (GPA120 < 1.7 & MAX < 32) == TRUE)
# m = matrix( c(pp, pf, fp, ff), nrow=2, ncol=2)
# m
# chisq.test(m, correct=FALSE)
#p = 1.472e-09


#----------------------------------------------------

# Score vs Science GPA
library("ggplot2")
ScienceScore <- read_excel("C:/Users/ebrad/Desktop/Stats Research/Science/ScienceScore.xlsx")

# DEFINE COLUMNS
Score = ScienceScore[[1]]
GPA120 = ScienceScore[[2]]

#linear regression
fit = lm(GPA120 ~ Score, data=ScienceScore)
summary(fit)
ggplot(ScienceScore, aes(Score, GPA120)) + ggtitle("Score vs GPA120") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(name="Score", limits=c(5, 30)) + scale_y_continuous(name="GPA120", limits=c(0, 4)) + geom_point() + geom_smooth(method = "lm", se=FALSE, colour="seagreen2") 

#logistic regression for passing
pass120 = as.integer(GPA120 >= 1.7)
ScienceScore$pass120=pass120

mylogit <- glm(pass120 ~ Score, data = ScienceScore, family = "binomial")
summary(mylogit)

#prediction for passing
x <- data.frame(Score = 23)
p <- predict(mylogit, x)
p

#logistic regression for B- and above
succeed120 = as.integer(GPA120 >= 2.7)
ScienceScore$succeed120=succeed120

mylogit_b <- glm(succeed120 ~ Score, data = ScienceScore, family = "binomial")
summary(mylogit_b)

#prediction for B- and above
x <- data.frame(Score = 23)
s <- predict(mylogit_b, x)
s


# 3x2 Contingency Tables

# Score cutoff = 23
pa = sum( (GPA120 >= 2.7 & GPA120 <= 4.0 & Score >= 23) == TRUE)
pc = sum( (GPA120 >= 1.7 & GPA120 < 2.7 & Score >= 23) == TRUE)
pf = sum( (GPA120 < 1.7 & Score >= 23) == TRUE)
fa = sum( (GPA120 >= 2.7 & GPA120 <= 4.0 & Score < 23) == TRUE)
fc = sum( (GPA120 >= 1.7 & GPA120 < 2.7 & Score < 23) == TRUE)
ff=  sum( (GPA120 < 1.7 & Score < 23) == TRUE)
m = matrix( c(pa, pc, pf, fa, fc, ff), nrow=3, ncol=2)
m
chisq.test(m, correct=FALSE)
#p-value < 2.2e-16


# 2x2 Contingency Tables

# pp = pass Chem 120, pass Score
# pf = pass Chem 120, fail Score
# fp = fail Chem 120, pass Score
# ff = fail Chem 120, fail Score

#Score cutoff score = 23 BEST
pp = sum( (GPA120 >= 1.7 & Score >= 23) == TRUE)
pf = sum( (GPA120 >= 1.7 & Score < 23) == TRUE)
fp = sum( (GPA120 < 1.7 & Score >= 23) == TRUE)
ff=  sum( (GPA120 < 1.7 & Score < 23) == TRUE)
m = matrix( c(pp, pf, fp, ff), nrow=2, ncol=2)
m
chisq.test(m, correct=FALSE)
#p = 3.52e-09

# #Score cutoff score = 22
# pp = sum( (GPA120 >= 1.7 & Score >= 22) == TRUE)
# pf = sum( (GPA120 >= 1.7 & Score < 22) == TRUE)
# fp = sum( (GPA120 < 1.7 & Score >= 22) == TRUE)
# ff=  sum( (GPA120 < 1.7 & Score < 22) == TRUE)
# m = matrix( c(pp, pf, fp, ff), nrow=2, ncol=2)
# m
# chisq.test(m, correct=FALSE)
# diag = pf + fp
# diag
# #p = 5.16e-09
# 
# #Score cutoff score = 24
# pp = sum( (GPA120 >= 1.7 & Score >= 24) == TRUE)
# pf = sum( (GPA120 >= 1.7 & Score < 24) == TRUE)
# fp = sum( (GPA120 < 1.7 & Score >= 24) == TRUE)
# ff=  sum( (GPA120 < 1.7 & Score < 24) == TRUE)
# m = matrix( c(pp, pf, fp, ff), nrow=2, ncol=2)
# m
# chisq.test(m, correct=FALSE)
# diag = pf + fp
# diag
# #p = 1.28e-08

#------------------------------------

# College Algebra GPA vs Science GPA
library("ggplot2")
ScienceCA <- read_excel("C:/Users/ebrad/Desktop/Stats Research/Science/ScienceCA.xlsx")

# DEFINE COLUMNS
GPAca = ScienceCA[[1]]
GPA120 = ScienceCA[[2]]

#linear regression
fit = lm(GPA120 ~ GPAca, data=ScienceCA)
summary(fit)

ggplot(ScienceCA, aes(GPAca, GPA120)) + ggtitle("GPAca vs GPA120")  + theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(name="GPA College Algebra", limits=c(0, 4)) + scale_y_continuous(name="GPA120", limits=c(0, 4)) + geom_point() + geom_smooth(method = "lm", se=FALSE, colour="cyan1") 

#logistic regression for passing
pass120 = as.integer(GPA120 >= 1.7)
SciencCA$pass120=pass120

mylogit <- glm(pass120 ~ GPAca, data = ScienceCA, family = "binomial")
summary(mylogit)

#prediction for passing
x <- data.frame(GPAca = 1.7)
p <- predict(mylogit, x)
p

#logistic regression for B- and above
succeed120 = as.integer(GPA120 >= 2.7)
ScienceCA$succeed120=succeed120

mylogit_b <- glm(succeed120 ~ GPAca, data = ScienceCA, family = "binomial")
summary(mylogit_b)

#prediction for B- and above
x <- data.frame(GPAca = 1.7)
s <- predict(mylogit_b, x)
s

#ca_p is those who passed college algebra and passed chem 120
#ca_t is those who passed college algebra and took chem 120
ca_p = sum( (GPAca >= 1.7 & GPA120 >= 1.7) == TRUE)
ca_p
ca_t = sum( (GPAca >= 1.7 & GPA120 >= 0) == TRUE)
ca_t
p = ca_p / ca_t
p

#ca_b is those who passed college algebra and >= B- in chem 120
#ca_t is those who passed college algebra and took chem 120
ca_b = sum( (GPAca >= 1.7 & GPA120 >= 2.7) == TRUE)
ca_b
ca_t = sum( (GPAca >= 1.7 & GPA120 >= 0) == TRUE)
ca_t
b = ca_b / ca_t
b
