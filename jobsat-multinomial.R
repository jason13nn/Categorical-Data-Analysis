####enter data
jobsatw=data.frame (Income=c(3,10,20,35),Diss=c(2,2,0,0), Little=c(4,6,1,3), Mod=c(13,22,15,13), Very=c(3,4,8,8))
jobsatw

##############################################
####Baseline-Category Logit (BCL) Models
##############################################
library(VGAM)
jobsat.fit1 <-vglm(cbind(Diss, Little, Mod, Very) ~ Income, family=multinomial, data=jobsatw)
coef(jobsat.fit1)
summary(jobsat.fit1)

####Goodness-of-fit test
dev.fit1=deviance(jobsat.fit1)
df.fit1=df.residual(jobsat.fit1)
dev.fit1
df.fit1
1-pchisq(dev.fit1,df.fit1)

###Test the income effect
##should test all beta=0 together
jobsat.fit2 = vglm(cbind(Diss,Little,Mod,Very) ~ 1,family=multinomial, data=jobsatw)
dev.fit2=deviance(jobsat.fit2)
df.fit2=df.residual(jobsat.fit2)
dev.fit2
df.fit2
1-pchisq(dev.fit2-dev.fit1,df.fit2-df.fit1)

##############################################
####Cumulative Logit (Proportional Odds) Models
##############################################
jobsat.cl1=vglm(cbind(Diss,Little,Mod,Very) ~ Income, family=cumulative(parallel=TRUE), data=jobsatw)
summary(jobsat.cl1)
jobsat.cl1r=vglm(cbind(Very,Mod,Little,Diss) ~ Income, family=cumulative(parallel=TRUE), data=jobsatw)
coef(jobsat.cl1r)

###Test the income effect
jobsat.cl0=vglm(cbind(Diss,Little,Mod,Very) ~ 1, family=cumulative(parallel=TRUE), data=jobsatw)
dev.cl0=deviance(jobsat.cl0)
df.cl0=df.residual(jobsat.cl0)
dev.cl0
df.cl0
dev.cl1=deviance(jobsat.cl1)
df.cl1=df.residual(jobsat.cl1)
dev.cl1
df.cl1
1-pchisq(dev.cl0-dev.cl1,df.cl0-df.cl1)
