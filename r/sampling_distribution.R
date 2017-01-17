### Monte Carlo simulation: sampling distributions
### Closely follows the gone fishing walkthrough
library(mosaic) 

# The sample
plot(y~x, data=simdata_samp)
lm1 = lm(y~x, data= simdata_samp)
abline(lm1)
coef(lm1)

# The population
plot(y~x, data=simdata_pop)
lmpop = lm(y~x, data=simdata_pop)
abline(lmpop, col='red', lwd=5)
coef(lmpop)

# A Monte Carlo simulation of the sampling distribution of the OLS estimator
my_sim = do(10000)*{
  this_sample = sample(simdata_pop, 50)
  lmsamp = lm(y ~ x, data=this_sample)
  coef(lmsamp)
}
head(my_sim)

# The standard error of the slope estimate is about 0.1
hist(my_sim$x, 20)
sd(my_sim$x)

# The standard error of the intercept estimate is about 0.6
hist(my_sim$Intercept, 20)
sd(my_sim$Intercept)



