###read data
homicide=data.frame(nvics=rep(0:6, 2), race=rep(c("Black","White"), each=7), freq=c(119,16,12,7,3,2,0,1070,60,14,4,0,0,1))
homicide

###exploratory analysis for overdispersion
xtabs(freq ~ race + nvics, data=homicide)
n=with(homicide, tapply(freq, race, sum))
ybar=by(homicide, homicide$race, function(x) weighted.mean(x$nvics, x$freq))
homicide$ybar=rep(ybar, each=7)
s2=by(homicide, homicide$race, function(x) weighted.mean((x$nvics - x$ybar)^2, x$freq))
cbind(n, ybar,s2)

###Poisson regression
homicide=transform(homicide, race = relevel(race, "White"))
hom.poi=glm(nvics ~ race, data=homicide, weights=freq, family=poisson)
summary(hom.poi)
#Wald interval --Poisson
confint.default(hom.poi)
exp(confint.default(hom.poi))
#LR interval --Poisson
confint(hom.poi)
exp(confint(hom.poi))


###Negative binomial regression
library(MASS)
hom.nb=glm.nb(nvics ~ race, data=homicide, weights=freq)
summary(hom.nb)
#Wald interval --negative binomial
confint.default(hom.nb)
exp(confint.default(hom.nb))
#LR interval --negative binomial
confint(hom.nb)
exp(confint(hom.nb))

###unweighted version
#one obs per line
hom2=homicide[rep(1:14, homicide$freq),-3]
hom2=transform(hom2, race = relevel(race, "White"))
hom.poi2=glm(nvics ~ race, data=hom2, family=poisson)
summary(hom.poi2)


