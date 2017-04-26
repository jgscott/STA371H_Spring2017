library(mosaic)
library(mvtnorm)

# Read in helper function
source("http://jgscott.github.io/teaching/r/mvnorm/rbvnorm.R")

# Statistical assumptions
mu_stocks = 0.065
mu_bonds = 0.017
sd_stocks = 0.195
sd_bonds = 0.075
rho = 0.3

out1 = do(1000)*{
  total_wealth = 10000
  weights = c(0.7, 0.3) # how much of your wealth in each asset?
  wealth_by_asset = total_wealth * weights
  Horizon = 40
  for(year in 1:Horizon) {
    # Simulate a bivariate normal set of returns
    returns = rbvnorm(1, mu_stocks, mu_bonds, sd_stocks, sd_bonds, rho)
    # Update wealth in each asset
    wealth_by_asset = wealth_by_asset * (1 + returns)
    # # rebalance
    total_wealth = sum(wealth_by_asset)
    wealth_by_asset = total_wealth * weights
  }
  total_wealth
}

mean(out1$result)
sd(out1$result)



### Advanced optional topic
### Do loops are slow!  Using nested for loops will be faster...

NMC = 5000
out2 = rep(0, NMC)
for(sim in 1:NMC) {
  total_wealth = 10000
  weights = c(0.7, 0.3) # how much of your wealth in each asset?
  wealth_by_asset = total_wealth * weights
  Horizon = 40
  for(year in 1:Horizon) {
    # Simulate a bivariate normal set of returns
    returns = rbvnorm(1, mu_stocks, mu_bonds, sd_stocks, sd_bonds, rho)
    # Update wealth in each asset
    wealth_by_asset = wealth_by_asset * (1 + returns)
    # # rebalance
    total_wealth = sum(wealth_by_asset)
    wealth_by_asset = total_wealth * weights
  }
  out2[sim] = total_wealth
}
