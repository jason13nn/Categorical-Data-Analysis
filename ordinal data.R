#######################################################
### R Function pears.cor()#############################
#######################################################
pears.cor=function(table, rscore, cscore)
{ 
  dim=dim(table) 
  rbar=sum(margin.table(table,1)*rscore)/sum(table) 
  rdif=rscore-rbar 
  cbar=sum(margin.table(table,2)*cscore)/sum(table) 
  cdif=cscore-cbar 
  ssr=sum(margin.table(table,1)*(rdif^2)) 
  ssc=sum(margin.table(table,2)*(cdif^2)) 
  ssrc=sum(t(table*rdif)*cdif) 
  pcor=ssrc/(sqrt(ssr*ssc)) 
  pcor 
  M2=(sum(table)-1)*pcor^2
  M2
  result=c(pcor, M2)
  result
} 

#######################################################
### Example: Job Satisfaction##########################
#######################################################
## enter data
sattab= matrix(c(2,2,0,0,4,6,1,3,13,22,15,13,3,4,8,8), nrow = 4, dimnames = list(Income = c("<5K", "5K-15K", "15K-25K", ">25K"), Sat = c("Dissat", "Little", "Moderate", "Very")))
sattab = as.table(sattab)

## run the Pearson chi-squared test of independence based on X2
chisq.test(sattab) 

### Likelihood Ratio Test based on G2
##G2
library(DescTools)
GTest(sattab) 

### Calculate r and M2
pears.cor(sattab, c(1,2,3,4),c(1,2,3,4)) 
### this should give you, r=0.27, M2=7.63

pears.cor(sattab, c(2.5,10,20,30),c(1,2,3,4)) 
### this should give you, r=0.27, M2=7.73
