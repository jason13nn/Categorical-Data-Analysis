####data from a study of nesting horseshoe rabs
####investigate factors that affect a female crab has at least one satellite
#library(icda)
#data(horseshoecrabs)
horseshoecrabs=read.csv("/Users/jason13nn/Desktop/SMU/Spring 2020/STAT 6395   (CDA)/files/horseshoecrabs.csv")
horseshoecrabs

####fit a simple logistic regression model
crabs.logit = glm((Satellites > 0) ~ Weight, family=binomial, data=horseshoecrabs)
summary(crabs.logit)

####estimate the prob. of having satellites at the mean weight
xbar <- mean(horseshoecrabs$Weight)
xbar
sd(horseshoecrabs$Weight)
##estimate the linear componet logit pi
predict(crabs.logit, data.frame(Weight=xbar), type="link")
##estimate prob. directly
predict(crabs.logit, data.frame(Weight=xbar), type="response")

####visualize the estimated prob. curve
attach(horseshoecrabs)
plot(Weight, (Satellites > 0), xlim=c(0,30), ylim=c(0,1), xlab="Weight", ylab="Has Satellites")
curve(plogis(-3.69+1.82*x), add=TRUE)
curve(plogis(-3.69+1.2*x), add=TRUE)
curve(plogis(-3.69+0.6*x), add=TRUE)
curve(plogis(-3.69+0.2*x), add=TRUE)
detach(horseshoecrabs)

####try linear probability model
crabs.lin = glm((Satellites > 0) ~ Weight, family=binomial(link=make.link("identity")), data=horseshoecrabs)
##try again by pretending normal data
crabs.lin = glm((Satellites > 0) ~ Weight, family=gaussian, data=horseshoecrabs)
crabs.lin 

####CI for beta
##Wald CI
confint.default(crabs.logit)
exp(confint.default(crabs.logit))
exp(0.1*confint.default(crabs.logit))

##LRT CI
confint(crabs.logit)
exp(confint(crabs.logit))
exp(0.1*confint(crabs.logit))

####Model-based CI for estimated prob
#library(icda)
#crabs.predCI <- predCI(crabs.logit)
#crabs.predCI[1,]
preddat=predict(crabs.logit, data.frame(Weight=c(2.44,3.05,4)), type='link', se.fit=TRUE)
preddat
#fitted prob
with(preddat,  exp(fit)/(1+exp(fit)))
#lower bound
with(preddat,  exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)))
#upper bound
with(preddat,  exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)))

####plot model-based C.I curves

preddat <- predict(crabs.logit, data.frame(Weight=(0:600)/100), type='link', se.fit=TRUE)
with(horseshoecrabs, plot(Weight, (Satellites > 0), type="n", 
                 ylim=c(0, 1), ylab="Have Satellites", xlab="Wight"))
with(preddat, lines((0:600)/100, exp(fit)/(1+exp(fit)), col="blue"))
with(preddat, lines((0:600)/100, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2, col="red"))
with(preddat, lines((0:600)/100, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2, col="red"))


###Sample-based CI for estimated prob
#6 crabs have weight=3.05, and 5 of them have satellites
binom.test(5,6)

### test association H0: beta=0
##get loglikelihood for the null model
crabs.null = glm((Satellites > 0) ~ 1, family=binomial, data=horseshoecrabs)
logLik(crabs.null)
##get loglikelihood for the fitted model
logLik(crabs.logit)
#LRT test
drop1(crabs.logit, test="Chisq")
#anova(crabs.null, crabs.logit,test="Chisq")


