#library(icda)
####data from a study of nesting horseshoe rabs
####investigate factors that affect a female crab has at least one satellite
#data(horseshoecrabs)
horseshoecrabs=read.csv("/Users/jason13nn/Desktop/SMU/Spring 2020/STAT 6395   (CDA)/files/horseshoecrabs.csv")
horseshoecrabs

####fit a simple logistic regression model
crabs.logit = glm((Satellites > 0) ~ Weight, family=binomial, data=horseshoecrabs)
summary(crabs.logit)

#####multiple regression with weight and color
horseshoecrabs = transform(horseshoecrabs, C = as.factor(Color))
levels(horseshoecrabs$C)
crabs.fit1 = glm((Satellites > 0) ~ C + Weight, family=binomial, data=horseshoecrabs)
summary(crabs.fit1)
predict(crabs.fit1, data.frame(Weight=c(2.44,2.44),C=c("1","2")), type="response")

#### test whether color can be dropped
#obtain log likelihood under M0 (with Weight only)
logLik(crabs.logit)
#obtain log likelihood under M1(with both Weight and color)
logLik(crabs.fit1)
#obtain deviance for M0 (with Weight only)
deviance(crabs.logit)
#obtain deviance for M1 (with both Color and Weight)
deviance(crabs.fit1)
anova(crabs.logit, crabs.fit1, test="Chisq")
#### test whether color or weight can be dropped from the model being fitted
drop1(crabs.fit1, test="Chisq")

#### Group color levels to dark or nondark to improve the fit
crabs.fit2 = glm((Satellites > 0) ~ I(Color == 4) + Weight, family=binomial, data=horseshoecrabs)
summary(crabs.fit2)
###comparing two models, one with 1 dummy, the other with three dummies
anova(crabs.fit2, crabs.fit1, test="Chisq")

#### Interaction between weight and color
crabs.fit3 = update(crabs.fit1, . ~ C*Weight)
summary(crabs.fit3)
#Get deviance of the model with interation terms
deviance(crabs.fit3)
#Get deviance of the model without interation terms
deviance(crabs.fit1)

#### Test whether interaction terms can be dropped
anova(crabs.fit1, crabs.fit3, test="Chisq")
drop1(crabs.fit3, test="Chisq")

####Treat color as an ordinal var.
crabs.fit4 = glm((Satellites > 0) ~ Weight + Color, family=binomial, data=horseshoecrabs)
summary(crabs.fit4)

####comparing two models, one treating color as nominal and the other treating color as ordinal
anova(crabs.fit4, crabs.fit1, test="Chisq")

#SAS
data d1;
input d t y @@;
cards;
45 0 0 15 0 0 40 0 1 83 1 1
90 1 1 25 1 1 35 0 1 65 0 1
95 0 1 35 0 1 75 0 1 45 1 1
50 1 0 75 1 1 30 0 0 25 0 1
20 1 0 60 1 1 70 1 1 30 0 1
60 0 1 61 0 0 65 0 1 15 1 0
20 1 0 45 0 1 15 1 0 25 0 1
15 1 0 30 0 1 40 0 1 15 1 0
135 1 1 20 1 0 40 1 0
;
run;
proc genmod order=data;
class t;
model y = d t / dist = bin link = logit type3;
run;