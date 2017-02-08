# Read in 10 mile race data
library(mosaicData)
data(TenMileRace)

# The model aggregrating men and women
plot(net~age,data=TenMileRace, col='grey')
lm1 = lm(net~age,data=TenMileRace)
abline(lm1)
summary(lm1)

# Now disaggregating
lmM = lm(net~age,data=subset(TenMileRace,sex=="M"))
lmF = lm(net~age,data=subset(TenMileRace,sex=="F"))
summary(lmM)
summary(lmF)

# Clearly we get different effects due to age when we disaggregate
plot(net~age,data=TenMileRace, col='grey', pch=19, cex=0.5)
abline(lm1, col='black')
abline(lmM, col='red')
abline(lmF, col='blue')
