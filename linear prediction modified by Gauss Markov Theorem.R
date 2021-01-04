### examine potential Multicollinearity by scatterplot and VIF
library(readxl)
mpow = read_excel("MarketPower.xlsx")

#create an OLS linear regression
OLSbase = lm(Markup~RevGR+eqshare+FCR+age+TotalassetsthEUR, dat=mpow)
summary(OLSbase)

#Calculate the pairwise correlation coefficients
indepvar = cbind(mpow[,5], mpow[,18], mpow[,20], mpow[,22:23])
cor(indepvar)
#all correlations are far from 0.8 or -0.8, so multicollinearity might not exist

#to double check it, let us use VIF indicator to check if inflated exploratory power
library(car)
vif(OLSbase)
#confirm no multicollinearity because all VIFs are below 5


### examine Heteroskedasticity by Breusch-Pagan test & White-test

#Start with the Breusch-Pagan test
#Extract squared residuals
mpow$sqres = OLSbase$residuals^2
BPreg = lm(sqres~RevGR+eqshare+FCR+age+TotalassetsthEUR, dat=mpow)
BP = nrow(mpow)*summary(BPreg)$r.squared
BP
#the Breusch-Pagan test is based on chi-square distribution to test p value
BPpv = pchisq(BP, length(BPreg$coefficients)-1,lower.tail=FALSE)
BPpv

#There is a predefined function in R. So let us cross-check our results
library(lmtest)
bptest(OLSbase)

#Now we are going to conduct a White-test
WTreg = lm(sqres~RevGR+eqshare+FCR+age+TotalassetsthEUR+I(RevGR*RevGR)+RevGR*eqshare+RevGR*FCR
                  +RevGR*age+RevGR*TotalassetsthEUR+I(eqshare*eqshare)+eqshare*FCR+eqshare*age
                  +eqshare*TotalassetsthEUR+I(FCR*FCR)+FCR*age+FCR*TotalassetsthEUR
                  +I(age*age)+age*TotalassetsthEUR+I(TotalassetsthEUR*TotalassetsthEUR), dat=mpow)
summary(WTreg)
WT =  nrow(mpow)*summary(WTreg)$r.squared
WT
#the more variables the model has, the higher R^3 is
#the White Test is based on chi-square distribution to test p value
WTpv = pchisq(WT, length(WTreg$coefficients)-1,lower.tail=FALSE)
WTpv

#Again, we can cross check using predefined commands in R
install.packages("skedastic")
library(skedastic)
white_lm(OLSbase, interactions=TRUE)

#Let us solve the issue of heteroskedastic errors now
library(sandwich)
coeftest(OLSbase, vcov=vcovHC(OLSbase, "HC1"))
summary(OLSbase)

#correct heteroskedasticity with heteroskedasticity-robus standdard errors
install.packages("estimatr")
library(estimatr)
OLS_robust <- lm_robust(Markup~RevGR+eqshare+FCR+age+TotalassetsthEUR, dat=mpow)
summary(OLS_robust)
# then we use adjusted t-values, which are lower and without the issue of heteroskedasticity


### examine Autocorrelation by Durbin–Watson d test

#time-series dataset
soldat = read.csv("solardat.csv", sep=";")

#Run a linear model
OLSsol = lm(CV_daily~PV_daily_MWh, dat=soldat)
summary(OLSsol)

#obtain residuals
#manually calculate Durbin–Watson d test
library(Hmisc)
soldat$solres= OLSsol$residuals
soldat$lagsolres = Lag(soldat$solres)

#generate the test statistic of Durbin–Watson d test
soldat$difres = (soldat$solres-soldat$lagsolres)^2
soldat$sqres = soldat$solres^2
dtest = sum(soldat$difres, na.rm=TRUE)/sum(soldat$sqres, na.rm=TRUE)
dtest


#Use predefined function in R
dwtest(OLSsol)
#p-value < 0.05 -> autocorrelation exists

#solution to autocorrelation: add lagged residuals 
soldat$lagcv = Lag(soldat$CV_daily)
autoco = lm(CV_daily~PV_daily_MWh+lagcv,dat=soldat)
summary(autoco)
dwtest(autoco)
#p-value > 0.05 -> no autocorrelation