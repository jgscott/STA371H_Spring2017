library(mosaic)
library(fImport)

# Import helper function to get returns from prices
source("http://jgscott.github.io/teaching/r/mvnorm/computereturns.R")

# Import 10 years of data on assets
mystocks = c("SPY", "TLT", "LQD", "DBC", "VNQ")
myprices = yahooSeries(mystocks, from='2006-04-25', to='2016-04-24')

# Compute the returns from the closing prices
myreturns = computereturns(myprices)
head(myreturns)

# These returns can be viewed as draws from the joint distribution
pairs(myreturns)
plot(myreturns[,1], type='l')

# Simulate a one-day change in your portfolio
# We'll allocate 20% to each asset.
totalwealth = 10000
weights = c(0.2, 0.2, 0.2, 0.2, 0.2)	# What percentage of your wealth will you put in each stock?

# How much money do we have in each stock?
holdings = weights * totalwealth

# Sample a random return from the empirical joint distribution
# This simulates a random day
return.today = resample(myreturns, 1, orig.ids=FALSE)

# Update the value of your holdings
holdings = holdings + holdings*return.today

# Now loop over 20 trading days (4 weeks)
totalwealth = 10000
horizon = 20
weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
holdings = weights * totalwealth
wealthtracker = rep(0, horizon) # Placeholder
for(today in 1:horizon) {
	return.today = resample(myreturns, 1, orig.ids=FALSE)
	holdings = holdings + holdings*return.today
	totalwealth = sum(holdings)
	wealthtracker[today] = totalwealth
}
totalwealth
plot(wealthtracker)

# Now simulate many different possible 4-week trading periods.
# Here we're not bothering to track wealth along the way.
# Just output total wealth after 4 weeks from each for-loop.
sim1 = do(1000)*{
	totalwealth = 10000
	weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
	holdings = weights * totalwealth
	for(today in 1:horizon) {
	  return.today = resample(myreturns, 1, orig.ids=FALSE)
	  holdings = holdings + holdings*return.today
	  totalwealth = sum(holdings)
	}
	totalwealth
}

# Visualize and summarize the result
hist(sim1$result, 50)
mean(sim1$result)
sd(sim1$result)

# If we wanted, we could calculate an expected utility.
# If u(w) = log(w), then:
mu_utility = mean(log(sim1$result))


###
# VAR
###

# Turn the simulation into a profit/loss distribution
profit = sim1$result - 10000
hist(profit, 50)

# Now calculate 5% value at risk.
# This is the 5% quantile of the profit-loss distribution
qdata(profit, 0.05)

var05 = qdata(profit, 0.05)[2]
abline(v=var05, col='red', lwd=3)
abline(v=mean(profit), col='blue', lwd=3)
