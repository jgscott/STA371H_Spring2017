## Method 1: best approach
# fit a demand curve and use this in the equation for profit
plot(sales ~ price, data=milk)
plot(log(sales) ~ log(price), data=milk)
lm1 = lm(log(sales) ~ log(price), data=milk)
abline(lm1)

# Model coefficients
coef(lm1)
betahat = coef(lm1)

# The fitted demand curve on the original scale
plot(sales~price, data=milk)
curve(exp(betahat[1])*x^betahat[2], add=TRUE, col='blue')

# Now use R just like a graphing calculator
# Plot the equation for profit
unitcost = 1.5
curve( (x-unitcost)*exp(betahat[1])*x^betahat[2], from=1, to=10)
curve( (x-unitcost)*exp(betahat[1])*x^betahat[2], from=3, to=5)

# If you need the exact price, form a very dense sequence of point predictions
# Figure out which one leads to the largest profit
curve( (x-unitcost)*exp(betahat[1])*x^betahat[2], from=2, to=4)
xgrid = seq(1,10,length=1000)
profitgrid = (xgrid-unitcost)*exp(betahat[1])*xgrid^betahat[2]
which.max(profitgrid)
best_price = xgrid[which.max(profitgrid)]
abline(v=best_price)



### Method 2: plot realized profit vs price and fit a quadratic
# Not informed by economic theory but still reasonable
unitcost = 1
profit = milk$sales * (milk$price - unitcost)
plot(profit ~ price, data=milk)

lm2 = lm(profit ~ price + I(price^2), data=milk)
points(fitted(lm2) ~ price, data=milk, col='blue')

plot(profit ~ price, data=milk)

lm3 = lm(profit ~ price + splines::bs(price, knots=5), data=milk)
points(fitted(lm3) ~ price, data=milk, col='blue')

