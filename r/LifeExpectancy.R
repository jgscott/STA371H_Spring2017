library(mosaic)

plot(LifeExp ~ PPGDP, data=LifeExpectancy)

# A log transformation of GDP?
hist(LifeExpectancy$PPGDP)

# Still looks nonlinear
plot(LifeExp ~ log(PPGDP), data=LifeExpectancy)

# What about stratifying by group?
xyplot(LifeExp ~ log(PPGDP) | Group, data=LifeExpectancy)

# Use a model with dummy variables
lm1 = lm(LifeExp ~ log(PPGDP) + Group, data=LifeExpectancy)

plot(LifeExp ~ log(PPGDP), data=LifeExpectancy)
points(fitted(lm1) ~ log(PPGDP), data=LifeExpectancy,
      col='blue', pch=19)

sigmae = sd(resid(lm1))

coef(lm1)

# prediction for OECD country with GDP = 20000
yhat = 37.35888 + log(20000)* 3.17732 + 12.17037
yhat + sigmae
yhat - sigmae

# coverage
sum(abs(resid(lm1)) > sigmae)
1 - 43/199
