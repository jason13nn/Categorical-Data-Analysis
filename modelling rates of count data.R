####Read data from the icda package
#library(icda)
#data(traincollisions)
traincollisions=read.csv("Data/traincollisions.csv")
traincollisions

####Poisson loglinear model
trains.loglin = glm(TrRd ~ I(Year-1975), offset = log(KM), family=poisson, data=traincollisions)
summary(trains.loglin)

###Get Pearson X2 goodness of fit
sum(residuals(trains.loglin, type="pearson")^2)
#X2=42.19, larger than df=27

####Negative binomial GLM
trains.nb = glm.nb(TrRd ~ I(Year-1975) + offset(log(KM)), data=traincollisions)
summary(trains.nb)

####Plot the fitted curve from Poisson GLM
attach(traincollisions)
plot(Year, 1000*TrRd/KM, ylim=c(0,1000*max(TrRd/KM)), ylab="Collisions per Billion Train-Kilometers")
curve(1000*exp(-4.21 - 0.0329*(x-1975)), add=TRUE)
detach(traincollisions)
