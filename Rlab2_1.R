# SDA chapter 12

if (!require(AER)){
  library(AER)  
}

data("USMacroG")
MacroDiff= apply(USMacroG,2,diff)

consumption = MacroDiff[,2]
dpi= MacroDiff[,5]
cpi= MacroDiff[,6]
government= MacroDiff[,4]
unemp= MacroDiff[,9]
# scatter plot
pairs(cbind(consumption,dpi,cpi,government,unemp))

fitLm1 = lm(consumption~dpi+cpi+government+unemp)
summary(fitLm1)
confint(fitLm1)
anova(fitLm1)

library(MASS)
fitLm2 = stepAIC(fitLm1)
summary(fitLm2)
AIC(fitLm1)
AIC(fitLm2)
AIC(fitLm1)-AIC(fitLm2)

install.packages("car")
library(car)
vif(fitLm1)
vif(fitLm2)


par(mfrow=c(2,2))
sp = 0.8
crPlot(fitLm1,dpi,span=sp,col="black")
crPlot(fitLm1,cpi,span=sp,col="black")
crPlot(fitLm1,government,span=sp,col="black")
crPlot(fitLm1,unemp,span=sp,col="black")






