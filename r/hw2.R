# 1) Stock returns

# Part A: create models
lmAAPL = lm(AAPL~SP500,data=marketmodel)
lmGOOG = lm(GOOG~SP500,data=marketmodel)
lmMRK = lm(MRK~SP500,data=marketmodel)
lmJNJ = lm(JNJ~SP500,data=marketmodel)
lmWMT = lm(WMT~SP500,data=marketmodel)
lmTGT = lm(TGT~SP500,data=marketmodel)

# Extract information from each model
# E.g. for Apple:
coef(lmAAPL)
sd(resid(lmAAPL))

# Beta for Wal-Mart
coef(lmWMT)

# And so on for the remaining stocks
# Part D: look at Wal-Mart residuals versus other residuals
plot(resid(lmWMT) ~ resid(lmTGT))
plot(resid(lmWMT) ~ resid(lmAAPL))
plot(resid(lmWMT) ~ resid(lmGOOG))
plot(resid(lmWMT) ~ resid(lmMRK))
plot(resid(lmWMT) ~ resid(lmJNJ))

# WMT residuals look nearly uncorrelated with AAPL and GOOG residuals
# To compare the other three, we could compute correlations
cor(resid(lmWMT), resid(lmTGT))
cor(resid(lmWMT), resid(lmMRK))
cor(resid(lmWMT), resid(lmJNJ))



# Problem 3: read in 10 mile race data
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

