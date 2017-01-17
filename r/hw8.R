library(mosaic)
library(mvtnorm)  # need to install this the first time you use it

# Import code for bivariate normal
# Put this line at the top of any script where you need to simulate
# from a bivariate normal distribution.
source("http://jgscott.github.io/teaching/r/mvnorm/rbvnorm.R")

# Modeling a risky asset with a positive expected return
mu1 = 0.065
mu2 = 0.015
sigma1 = 0.2
sigma2 = 0.1
rho = -0.1

# 1000 samples from this bivariate normal
returns = rbvnorm(1000, mu1, mu2, sigma1, sigma2, rho)
plot(returns)


#### Calculate expected value of E(log(1+x_t))

# 50/50 split
port1 = do(10000)*{
	returns = rbvnorm(1, mu1, mu2, sigma1, sigma2, rho)
	portfolio_return = 0.5*returns[1] + 0.5*returns[2]
	log(1 + portfolio_return)
}
mean(port1$result)

port2 = do(10000)*{
	returns = rbvnorm(1, mu1, mu2, sigma1, sigma2, rho)
	portfolio_return = 0.8*returns[1] + 0.2*returns[2]
	log(1 + portfolio_return)
}
mean(port2$result)

# Looks like this one is the highest
port3 = do(10000)*{
	returns = rbvnorm(1, mu1, mu2, sigma1, sigma2, rho)
	portfolio_return = returns[1]
	log(1 + portfolio_return)
}
mean(port3$result)

# Long-term growth rates: R = e^{E(log(1+x_t))} - 1
exp( mean(port1$result) ) - 1 
exp( mean(port2$result) ) - 1 
exp( mean(port3$result) ) - 1 



#### Alternate way: just simulate very long portfolios
#### to estimate long-term growth rate
horizon = 100

sim1 = do(250)*{
	# Invest $1
	total_wealth = 1
	pweights = c(0.5, 0.5)
	holdings = pweights*total_wealth
	for(year in 1:horizon) {
		returns = rbvnorm(1, mu1, mu2, sigma1, sigma2, rho)
		holdings = holdings*(1+returns)
		total_wealth = sum(holdings)
		# rebalance portfolio
		holdings = pweights*total_wealth
	}
	total_wealth
}
mean(sim1$result)

