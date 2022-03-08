##################################
#### Chinese lung cancer data ####
##################################

#####################CMH Test##################
lung = read.table("Data/chinese.txt",header=T)
lung
lung.l= reshape(lung, varying=list(3:4),timevar="lungcancer", v.names="freq", direction="long")
lung.l

lung.l=transform(lung.l, lungcancer=abs(lungcancer-2))
##### CMH test for conditional independence in a 2X2XK table
lungtab= xtabs(freq ~ smoker + lungcancer + city, data = lung.l)
mantelhaen.test(lungtab,correct=F,  conf.level = 0.95)

####Breslow-Day test for homogeneous association
library(DescTools)
BreslowDayTest(lungtab)

#############Model Based Approach ##################
####logistic reg for testing cond. indep.
lung = transform(lung, C = as.factor(city))
lung.logit = glm(cbind(lcY,lcN) ~ C + smoker, family=binomial, data=lung)

####Wald test beta (smoke)=0 for testing cond. indep.
summary(lung.logit)
####Goodness of fit test for homogeneous association
1-pchisq(5.20,7)
####LR test beta (smoke)=0 for testing cond. indep.
drop1(lung.logit, test="Chisq")