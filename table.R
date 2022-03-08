####Get Tabled Data in R
pag.tab = matrix(c(762, 484, 327, 239, 468, 477), nrow=2)
dimnames(pag.tab) = list(Gender=c("Female","Male"), Party=c("Democrat","Independent","Republican"))
pag.tab = as.table(pag.tab)
pag.tab

####Convert a table to a data frame
pag.df <- as.data.frame(pag.tab)
pag.df

####Create a data frame first
pag.df = expand.grid(Gender=c("Female","Male"), Party=c("Democrat","Independent","Republican"))
pag.df
pag.df$Freq = c(762, 484, 327, 239, 468, 477)
pag.df

####Generate a table from the data frame
xtabs(Freq ~ Gender + Party, data=pag.df)

####Read from a .txt file
pag.df = read.table("Data/pag.txt", header=TRUE)
####Read from a .csv file
pag.df = read.csv("Data/pag.csv")

####Get marginal table
pag.tab
margin.table(pag.tab, 1)
margin.table(pag.tab, 2)
addmargins(pag.tab)

####Get overall sample proportions
prop.table(pag.tab)
round(prop.table(pag.tab), 3)

####Get row & column proportions
prop.table(pag.tab, 1)
prop.table(pag.tab, 2)

####Pearson Chi-Square Test for Independence
chisq.test(pag.tab)
pag.chisq = chisq.test(pag.tab)
names(pag.chisq)
pag.chisq$statistic
pag.chisq$parameter
pag.chisq$p.value
pag.chisq$observed
pag.chisq$expected
with(pag.chisq, sum((observed - expected)^2/expected))

####Likelihood ratio Chi-Square Test for Independence
with(pag.chisq, 2*sum(observed*log(observed/expected)))

####Unadjusted (or raw) Pearson residuals
pag.chisq$residuals
####Standardized (or adjusted) Pearson residuals
pag.chisq$stdres



