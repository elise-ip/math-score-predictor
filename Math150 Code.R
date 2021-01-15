# M150 vs Math 150 GPA
install.packages("ggplot2")
library("ggplot2")
M_150 <- read_excel("C:/Users/ebrad/Desktop/Stats Research/Math 150/M_150.xlsx")

# DEFINE COLUMNS
M150 = M_150[[1]]
GPA150 = M_150[[2]]

#linear regression
fit = lm(GPA150 ~ M150, data=M_150)
summary(fit)
ggplot(M_150, aes(M150, GPA150)) + ggtitle("M150 vs GPA150") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(name="M150", limits=c(30, 65)) + scale_y_continuous(name="GPA150", limits=c(0, 4)) + geom_point() + geom_smooth(method = "lm", se=FALSE, colour="red") 

#logistic regression for passing
pass150 = as.integer(GPA150 >= 1.7)
M_150$pass150=pass150

mylogit <- glm(pass150 ~ M150, data = M_150, family = "binomial")
summary(mylogit)

#prediction for passing
x <- data.frame(M150 = 47)
p <- predict(mylogit, x)
p

#logistic regression for B- and above
succeed150 = as.integer(GPA150 >= 2.7)
M_150$succeed150=succeed150

mylogit_b <- glm(succeed150 ~ M150, data = M_150, family = "binomial")
summary(mylogit_b)

#prediction for B- and above
x <- data.frame(M150 = 47)
s <- predict(mylogit_b, x)
s


# 3x2 Contingency Table

# pa = pass M150, A,B in Math 150
# pc = pass M150, C in Math 150
# pf = pass M150, fail Math 150 
# fa = fail M150, A,B in Math 150
# fc = fail M150, C in Math 150
# ff = fail M150, fail Math 150

# M150 cutoff = 47
pa = sum( (GPA150 >= 2.7 & GPA150 <= 4.0 & M150 >= 47) == TRUE)
pc = sum( (GPA150 >= 1.7 & GPA150 < 2.7 & M150 >= 47) == TRUE)
pf = sum( (GPA150 < 1.7 & M150 >= 47) == TRUE)
fa = sum( (GPA150 >= 2.7 & GPA150 <= 4.0 & M150 < 47) == TRUE)
fc = sum( (GPA150 >= 1.7 & GPA150 < 2.7 & M150 < 47) == TRUE)
ff=  sum( (GPA150 < 1.7 & M150 < 47) == TRUE)
m = matrix( c(pa, pc, pf, fa, fc, ff), nrow=3, ncol=2)
m
chisq.test(m, correct=FALSE)
#p-value = 0.6198


# 2x2 Contingency Tables

# pp = pass Math 150, pass M150
# pf = pass Math 150, fail M150
# fp = fail Math 150, pass M150
# ff = fail Math 150, fail M150

#M150 cutoff score = 47
pp = sum( (GPA150 >= 1.7 & M150 >= 47) == TRUE)
pf = sum( (GPA150 >= 1.7 & M150 < 47) == TRUE)
fp = sum( (GPA150 < 1.7 & M150 >= 47) == TRUE)
ff=  sum( (GPA150 < 1.7 & M150 < 47) == TRUE)
m = matrix( c(pp, pf, fp, ff), nrow=2, ncol=2)
m
m_47 = m
m_47
chisq.test(m_47, correct=FALSE)
#p = 0.414


#--------------------------------------------

# MAX SAT/ACT vs Math 150 GPA
library("ggplot2")
MAX150 <- read_excel("C:/Users/ebrad/Desktop/Stats Research/Math 150/MAX150.xlsx")

# DEFINE COLUMNS
MAX = MAX150[[1]]
GPA150 = MAX150[[2]]

#linear regression
fit = lm(GPA150 ~ MAX, data=MAX150)
summary(fit)
ggplot(MAX150, aes(MAX, GPA150)) + ggtitle("MAX SAT/ACT vs GPA150") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(name="MAX", limits=c(15, 36)) + scale_y_continuous(name="GPA150", limits=c(0, 4)) + geom_point() + geom_smooth(method = "lm", se=FALSE, colour="blue") 

#logistic regression for passing
pass150 = as.integer(GPA150 >= 1.7)
MAX150$pass150=pass150

mylogit <- glm(pass150 ~ MAX, data = MAX150, family = "binomial")
summary(mylogit)

#prediction for passing
x <- data.frame(MAX = 27)
p <- predict(mylogit, x)
p

#logistic regression for B- and above
succeed150 = as.integer(GPA150 >= 2.7)
MAX150$succeed150=succeed150

mylogit_b <- glm(succeed150 ~ MAX, data = MAX150, family = "binomial")
summary(mylogit_b)

#prediction for B- and above
x <- data.frame(MAX = 27)
s <- predict(mylogit_b, x)
s


# 3x2 Contingency Table

# pa = pass MAX, A,B in Math 150
# pc = pass MAX, C in Math 150
# pf = pass MAX, fail Math 150 
# fa = fail MAX, A,B in Math 150
# fc = fail MAX, C in Math 150
# ff = fail MAX, fail Math 150

# MAX cutoff = 27
pa = sum( (GPA150 >= 2.7 & GPA150 <= 4.0 & MAX >= 27) == TRUE)
pc = sum( (GPA150 >= 1.7 & GPA150 < 2.7 & MAX >= 27) == TRUE)
pf = sum( (GPA150 < 1.7 & MAX >= 27) == TRUE)
fa = sum( (GPA150 >= 2.7 & GPA150 <= 4.0 & MAX < 27) == TRUE)
fc = sum( (GPA150 >= 1.7 & GPA150 < 2.7 & MAX < 27) == TRUE)
ff=  sum( (GPA150 < 1.7 & MAX < 27) == TRUE)
m = matrix( c(pa, pc, pf, fa, fc, ff), nrow=3, ncol=2)
m
chisq.test(m, correct=FALSE)
#p-value = 2.706e-05


# 2x2 Contingency Tables

# pp = pass Math 150, pass MAX SAT/ACT
# pf = pass Math 150, fail MAX SAT/ACT
# fp = fail Math 150, pass MAX SAT/ACT
# ff = fail Math 150, fail MAX SAT/ACT

#MAX SAT/ACT cutoff score = 27
pp = sum( (GPA150 >= 1.7 & MAX >= 27) == TRUE)
pf = sum( (GPA150 >= 1.7 & MAX < 27) == TRUE)
fp = sum( (GPA150 < 1.7 & MAX >= 27) == TRUE)
ff=  sum( (GPA150 < 1.7 & MAX < 27) == TRUE)
m_27 = matrix( c(pp, pf, fp, ff), nrow=2, ncol=2)
m_27
x = chisq.test(m_27, correct=FALSE)
x
#p = .008795


#----------------------------------------------------

# Score vs Math 150 GPA
library("ggplot2")
Score150 <- read_excel("C:/Users/ebrad/Desktop/Stats Research/Math 150/Score150.xlsx")

# DEFINE COLUMNS
Score = Score150[[1]]
GPA150 = Score150[[2]]

#linear regression
fit = lm(GPA150 ~ Score, data=Score150)
summary(fit)
ggplot(Score150, aes(Score, GPA150)) + ggtitle("Score vs GPA150") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(name="Score", limits=c(5, 30)) + scale_y_continuous(name="GPA150", limits=c(0, 4)) + geom_point() + geom_smooth(method = "lm", se=FALSE, colour="magenta") 

#logistic regression for passing
pass150 = as.integer(GPA150 >= 1.7)
Score150$pass150=pass150

mylogit <- glm(pass150 ~ Score, data = Score150, family = "binomial")
summary(mylogit)

#prediction for passing
x <- data.frame(Score = 23)
p <- predict(mylogit, x)
p

#logistic regression for B- and above
succeed150 = as.integer(GPA150 >= 2.7)
Score150$succeed150=succeed150

mylogit_b <- glm(succeed150 ~ Score, data = Score150, family = "binomial")
summary(mylogit_b)

#prediction for B- and above
x <- data.frame(Score = 23)
s <- predict(mylogit_b, x)
s


# 3x2 Contingency Table

# pa = pass Score, A,B in Math 150
# pc = pass Score, C in Math 150
# pf = pass Score, fail Math 150 
# fa = fail Score, A,B in Math 150
# fc = fail Score, C in Math 150
# ff = fail Score, fail Math 150

# Score cutoff = 23
pa = sum( (GPA150 >= 2.7 & GPA150 <= 4.0 & Score >= 23) == TRUE)
pc = sum( (GPA150 >= 1.7 & GPA150 < 2.7 & Score >= 23) == TRUE)
pf = sum( (GPA150 < 1.7 & Score >= 23) == TRUE)
fa = sum( (GPA150 >= 2.7 & GPA150 <= 4.0 & Score < 23) == TRUE)
fc = sum( (GPA150 >= 1.7 & GPA150 < 2.7 & Score < 23) == TRUE)
ff=  sum( (GPA150 < 1.7 & Score < 23) == TRUE)
m = matrix( c(pa, pc, pf, fa, fc, ff), nrow=3, ncol=2)
m
chisq.test(m, correct=FALSE)
#p-value = .02044


# 2x2 Contingency Tables

# pp = pass Math 150, pass Score
# pf = pass Math 150, fail Score
# fp = fail Math 150, pass Score
# ff = fail Math 150, fail Score

#Score cutoff score = 23
pp = sum( (GPA150 >= 1.7 & Score >= 23) == TRUE)
pf = sum( (GPA150 >= 1.7 & Score < 23) == TRUE)
fp = sum( (GPA150 < 1.7 & Score >= 23) == TRUE)
ff=  sum( (GPA150 < 1.7 & Score < 23) == TRUE)
m_23 = matrix( c(pp, pf, fp, ff), nrow=2, ncol=2)
m_23
chisq.test(m_23, correct=FALSE)
#p = 0.163


# #Score cutoff score = 24
# pp = sum( (GPA150 >= 1.7 & Score >= 24) == TRUE)
# pf = sum( (GPA150 >= 1.7 & Score < 24) == TRUE)
# fp = sum( (GPA150 < 1.7 & Score >= 24) == TRUE)
# ff=  sum( (GPA150 < 1.7 & Score < 24) == TRUE)
# m = matrix( c(pp, pf, fp, ff), nrow=2, ncol=2)
# m
# chisq.test(m, correct=FALSE)
# diag = pf + fp
# diag
# #p = 0.4324


#------------------------------------

# College Algebra GPA vs Math 150 GPA
library("ggplot2")
CA150 <- read_excel("C:/Users/ebrad/Desktop/Stats Research/Math 150/CA150.xlsx")

# DEFINE COLUMNS
GPAca = CA150[[1]]
GPA150 = CA150[[2]]

#linear regression
fit = lm(GPA150 ~ GPAca, data=CA150)
summary(fit)

ggplot(CA150, aes(GPAca, GPA150)) + ggtitle("GPA College Algebra vs GPA Math 150")  + theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(name="GPAca", limits=c(0, 4)) + scale_y_continuous(name="GPA150", limits=c(0, 4)) + geom_point() + geom_smooth(method = "lm", se=FALSE, colour="orange") 

#ca_p is those who passed college algebra and passed math 150
#ca_t is those who passed college algebra and took math 150
ca_p = sum( (GPAca >= 1.7 & GPA150 >= 1.7) == TRUE)
ca_p
ca_t = sum( (GPAca >= 1.7 & GPA150 >= 0) == TRUE)
ca_t
p = ca_p / ca_t
p

#ca_b is those who passed college algebra and >= B- in math 150
#ca_t is those who passed college algebra and took math 150
ca_b = sum( (GPAca >= 1.7 & GPA150 >= 2.7) == TRUE)
ca_b
ca_t = sum( (GPAca >= 1.7 & GPA150 >= 0) == TRUE)
ca_t
b = ca_b / ca_t
b

#logistic regression for passing
pass150 = as.integer(GPA150 >= 1.7)
CA150$pass150=pass150

mylogit <- glm(pass150 ~ GPAca, data = CA150, family = "binomial")
summary(mylogit)

#prediction for passing
x <- data.frame(GPAca = 1.7)
p <- predict(mylogit, x)
p

#logistic regression for B- and above
succeed150 = as.integer(GPA150 >= 2.7)
Score150$succeed150=succeed150

mylogit_b <- glm(succeed150 ~ GPAca, data = CA150, family = "binomial")
summary(mylogit_b)

#prediction for B- and above
x <- data.frame(GPAca = 1.7)
s <- predict(mylogit_b, x)
s