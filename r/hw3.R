# Polynomial regression

# Define daily average gas gill
utilities$daily.average.gasbill = utilities$gasbill/utilities$billingDays

# Fit a model with a linear term:
lm1=lm(daily.average.gasbill ~ temp , data=utilities)
plot(daily.average.gasbill ~ temp, data=utilities)
points(fitted(lm1)~temp, data=utilities, col='red', pch=19)

# Fit a model with a quadratic term:
lm2=lm(daily.average.gasbill ~ temp + I(temp^2), data=utilities)
plot(daily.average.gasbill ~ temp, data=utilities)
points(fitted(lm2)~temp, data=utilities, col='blue', pch=19)

# Fit a model with a cubic term:
lm3=lm(daily.average.gasbill ~ temp + I(temp^2) + I(temp^3), data=utilities)
plot(daily.average.gasbill ~ temp, data=utilities)
points(fitted(lm3)~temp, data=utilities, col='green', pch=19)

# Prediction interval for the quadratic model:
sigma_lm2 = sd(resid(lm2))
new_temp = 50
coef(lm2)
yhat_2 = 9.472288520 - 0.211555282*new_temp + 0.001247559 * new_temp^2

# Two standard deviations to either side of 50
yhat_2 - 2*sigma_lm2
yhat_2 + 2*sigma_lm2

# Calculate empirical coverage
sum(resid(lm2) > 2*sigma_lm2) # misses low
sum(resid(lm2) < -2*sigma_lm2) # misses high
12/117 # fraction of misses out of 117 data points

# Show the upper and lower bounds of the interval:
lm2=lm(daily.average.gasbill ~ temp + I(temp^2), data=utilities)
plot(daily.average.gasbill ~ temp, data=utilities)
points(fitted(lm2)~temp, data=utilities, col='blue', pch=19)
points((fitted(lm2) + 2*sigma_lm2) ~ temp, data=utilities, col='red', pch=19)
points((fitted(lm2) - 2*sigma_lm2) ~ temp, data=utilities, col='red', pch=19)


### Soldering

# Starting with main effects only
lm1 = lm(skips ~ Opening + Solder + Mask, data=solder)
summary(lm1)

# All interactions
lm2 = lm(skips ~ Opening + Solder + Mask + Solder:Mask + Opening:Solder + Opening:Mask, data=solder)
summary(lm2)

# Looks like a noticeable jump in R^2 when we add the interaction terms
# An analysis of variance will help
anova(lm2)

# Solder:Mask has the lowest contribution to sums of squares
# Let's try dropping it
lm3 = lm(skips ~ Opening + Solder + Mask + Opening:Solder + Opening:Mask, data=solder)
summary(lm3)

# R-squared drops by 2%... up to you whether this is practically significant or not!
