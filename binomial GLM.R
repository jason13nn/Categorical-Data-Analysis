####Read Data from the package "icda"
#install.packages("icda", repos="http://www.stat.ufl.edu/~presnell/R", type="source")
#library(icda)
#data(malformation)

malformation=read.csv("Data/malformation.csv")
malformation

####Get a table
malform.tab=xtabs(Freq ~ Alcohol + Malformation, data=malformation)
malform.tab
round(100*prop.table(malform.tab, 1), 2)

####Recast data frame into a wide format
library(reshape2)
malformwide=dcast(malformation, Alcohol ~ Malformation, value.var="Freq")
malformwide

#####################################################
####Two ways to fit linear probability model
#####################################################
#1st way:
malform.lin=glm(cbind(Present,Absent) ~ Alcohol, family=binomial(link=make.link("identity")), data=malformwide)
#2nd way
malformwide=transform(malformwide, Total = Present + Absent)
malform.lin.alt=glm(Present/Total ~ Alcohol, weights=Total, family=binomial(link=make.link("identity")), data=malformwide)
#Print regression coefficients
coef(malform.lin)
#Summary of fitted linear probability model
summary(malform.lin)

####Compare fitted prob. to sample prop.
lin.fit=round(fitted.values(malform.lin),4)
sample.prop=with(malformwide, Present/Total)
sample.prop=round(sample.prop,4)
cbind(malformwide, sample.prop*100, lin.fit*100)

###########################################################
###Test independence in contingency table (chap.2)
###########################################################
####Use Pearson X2 to test independence in contingency table
result=chisq.test(malform.tab)
result

####Use LR G2 to test independence in contingency table
library(DescTools)
GTest(malform.tab) 
#both X2 and G2 ignore the ordering information

###########################################################
###Test Goodness of fit using X2 or G2
###########################################################
#Pearson X2 goodness-of-fit Statistic
sum(residuals(malform.lin, type="pearson")^2)
1-pchisq(3.3551,3)
#Likelihood Ratio G2 Statistic
deviance(malform.lin)
df.residual(malform.lin)
1-pchisq(2.9795,3)

#####################################################
####Fit logistic regression model for binary data
#####################################################
malform.logit <- glm(cbind(Present,Absent) ~ Alcohol, family=binomial, data=malformwide)
summary(malform.logit)

#Pearson X2 goodness-of-fit Statistic
sum(residuals(malform.logit, type="pearson")^2)
1-pchisq(2.0523,3)
#Likelihood Ratio G2 Statistic
deviance(malform.logit)
df.residual(malform.logit)
1-pchisq(1.9487,3)

####Compare fitted prob. to sample prop.
logit.fit=round(fitted.values(malform.logit),4)
logit.fit
cbind(malformwide, sample.prop*100, lin.fit*100, logit.fit*100)

#####################################################
####Fit probit regression model for binary data
#####################################################
malform.probit <- glm(cbind(Present,Absent) ~ Alcohol, family=binomial(link = "probit"), data=malformwide)
summary(malform.probit)

####Compare fitted prob. between logit and probit
probit.fit=round(fitted.values(malform.probit),4)
probit.fit
cbind(malformwide, sample.prop*100, logit.fit*100, probit.fit*100)

########################################################
####Comparing fitted prob. curves using different GLMs
#######################################################
x <- seq(0, 7.5, 0.01)
linfit <- predict(malform.lin, list(Alcohol = x), type="response")
logitfit <- predict(malform.logit, list(Alcohol = x), type="response")
plot(malformwide$Alcohol,sample.prop)
lines(x,linfit,col="red",lty=2)
lines(x,logitfit,col="blue",lty=3)
legend("topleft", legend=c("linear", "logit"), col=c("red", "blue"), lty=2:3, cex=0.8)
