#read data
cardiodata<-read.csv("/Users/jason13nn/Desktop/SMU/Spring 2020/STAT 6395   (CDA)/Final project/cardio_train.csv",sep = ";")
head(cardiodata)

# remove ID
cardiodata$id<-NULL

#transform days into years for Age
cardiodata$age<-cardiodata$age/365

str(cardiodata)

# factorize variables
cardiodata$gender<-factor(cardiodata$gender,labels=c('women','men'))
cardiodata$cholesterol<-factor(cardiodata$cholesterol,labels=c("normal","above normal","well above normal"))
cardiodata$gluc<-factor(cardiodata$gluc,labels=c("normal","above normal","well above normal"))
cardiodata$smoke<-factor(cardiodata$smoke,labels = c("No","Yes"))
cardiodata$alco<-factor(cardiodata$alco,labels = c("No","Yes"))
cardiodata$active<-factor(cardiodata$active,labels = c("No","Yes"))

# Target Variable
cardiodata$cardio<-factor(cardiodata$cardio,labels = c("Negative","Positive"))
str(cardiodata)

# Feature enginering 
library(ggcorrplot)

#add new varable BMI
cardiodata$bmi<-(cardiodata$weight)/((cardiodata$height/100)^2)

#check missing value
sapply(cardiodata, function(x) sum(is.na(x)))
# No missing values

# Looking into factor variables first
ggplot(cardiodata, aes(x=cardiodata$age, fill=cardiodata$cardio)) + geom_density(alpha=.3)+
  labs(title="Age distribution on test result",
       x="Age",y = "Density") +
  scale_fill_discrete(name = "Test Result",labels = c("Negative", "Positive"))+
  scale_x_continuous(breaks=seq(35, 70, 5))
# older people easier to have cardio

#cholesterol
ggplot(data = cardiodata, mapping = aes(x = cardio, fill = cardiodata$cholesterol)) +
  geom_bar(stat = 'count', position = 'dodge')+
  labs(title="Cholesterol Level on test result",
       x="Test Result",y = "Count",fill="Cholesterol Level")+
  scale_fill_manual(values=c("skyblue2","brown2","green2"))

# BMI
ggplot(data=cardiodata, aes(x=cardio,y=bmi)) + 
  geom_boxplot(width=0.5,fill = "skyblue2")  +ylim(10,50)+
  labs(title="BMi on test result",
       x="Test Result",y = "Body Mass Index")
  theme(plot.background = element_blank(),
         panel.background = element_blank(), axis.line = element_line(size=3, colour = "black"),
         axis.text.x=element_text(colour="black", size = , angle = 40, hjust=1),
         axis.text.y=element_text(colour="black", size = 6))

#logistic regression
cardio.lg <- glm((cardio>0) ~., cardiodata[,-13], family = "binomial")
summary(cardio.lg)

#check multicollinearity
library(car)
vif(cardio.lg)
#no VIF larger than 4.

#Automated backward selection using AIC
#specify the full model crabs.fit1 to start
step(cardio.lg, direction="backward")

#Automated forward selection using AIC
#specify the null model to start
cardio.null <- glm(cardio ~1, family=binomial, data=cardiodata[,-13])
#specify the full model crabs.fit1 to stop
step(cardio.null, scope=list(lower=cardio.null, upper=cardio.lg), direction="forward")

#Automated stepwise selection using AIC
step(cardiodata[,-13], scope=list(upper=cardio.lg), direction="both")

#ROC
library(InformationValue)
cardio <- ifelse(cardiodata$cardio=="Negative",0,1)
cardio <- as.factor(cardio)
lg.predict <- predict(cardio.lg, cardiodata, type="response") 
ROC <- plotROC(actuals = cardio, predictedScores = lg.predict)
ROC

#sensitivity
sensitivity(cardio, lg.predict)

#specificity
specificity(cardio, lg.predict)

#Residuals plot
Index <- 1:dim(cardiodata)[1]
# deviance residuals
Deviance_Residuals <- residuals(cardio.lg)
dff <- data.frame(Index,Deviance_Residuals,cardiodata$cardio)

ggplot(data = dff, mapping = aes(x = Index,y = Deviance_Residuals,color = cardiodata$cardio)) +
  geom_point() +
  geom_hline(yintercept = 3,linetype = "dashed", color = "blue") +
  geom_hline(yintercept = -3,linetype = "dashed", color = "blue") +
  labs(title = "Plot of Deviance Residuals") +
  theme(plot.title = element_text(hjust = 0.5))
#Most of the residuals falls with (-3, 3).

#Hosmer-Lemeshow test
#library(ResourceSelection)
#hoslem.test(cardiodata$cardio, fitted(cardio.lg), g = 10)

#interaction term
#cardio.lg.2 <- glm(cardio ~ (.)^2, cardiodata[,-13], family = "binomial")
#summary(cardio.lg.2)
#anova(cardio.lg.2, cardio.lg, test="Chisq")

#See whether we can drop any main effect
#drop1(cardio.lg, test="Chisq")

summary(cardio.lg)
