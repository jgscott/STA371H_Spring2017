library(mosaic)
library(fImport) # need to install this the first time you use it

# Import helper function
# Put this line at the top of any script where you need to simulate
# from past returns using bootstrapping.
source("http://jgscott.github.io/teaching/r/mvnorm/computereturns.R")

# Download data for a few stocks
mystocks = c("ORCL", "JNJ", "WMT", "XOM", "MRK")
myprices = yahooSeries(mystocks, from='2007-04-16', to='2017-04-25', frequency = 'daily')

# The first few rows
head(myprices)

# Compute the returns from the closing prices
myreturns = computereturns(myprices)
head(myreturns)

# These don't look normal
ind = 3
hist(myreturns[,ind], 100, prob=TRUE)
curve(dnorm(x, mean(myreturns[,ind]), sd(myreturns[,ind])), add=TRUE)


# How to proceed?
# One way: bootstrapping.
# That is: sample from the empirical joint distribution rather a theoretical model.

# How much money do we have in each stock?
totalwealth = 10000
pweights = c(0.2, 0.2, 0.2, 0.2, 0.2)
holdings = pweights * totalwealth


#### Simulate a single day

# Bootstrap: sample a random return from the empirical joint distribution
# This simulates a random day.
return.today = resample(myreturns, 1, orig.ids=FALSE)

# Update the value of your holdings
holdings = holdings*(1+return.today)

# Compute your new total wealth
totalwealth = sum(holdings)

#### Now simulate a single trajectory over 20 trading days
horizon = 20
totalwealth = 10000
pweights = c(0.2, 0.2, 0.2, 0.2, 0.2)
holdings = pweights * totalwealth
wealthtracker = rep(0, horizon) # Set up a placeholder to track total wealth
for(today in 1:horizon) {
	return.today = resample(myreturns, 1, orig.ids=FALSE)
	holdings = holdings*(1+return.today)
	totalwealth = sum(holdings)
	wealthtracker[today] = totalwealth
}
plot(wealthtracker, type='l')

