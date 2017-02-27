library(mosaic)
data(SaratogaHouses)
source('http://jgscott.github.io/teaching/r/utils/class_utils.R')


# Question 1: is there a partial relationship between fuel system type
# and price, adjusting for living area, lotsize, and fireplaces?

lm1 = lm(price ~ livingArea + lotSize + fireplaces + fuel, data=SaratogaHouses)
coef(lm1)
rsquared(lm1)

# permutation test: is the fuel variable significant?
perm1 = do(2000)*{
	lm(price ~ livingArea + lotSize + fireplaces + shuffle(fuel), data=SaratogaHouses)
}
head(perm1)

hist(perm1$r.squared, 20)

# The quantile
qdata(perm1$r.squared, p = 0.95)

# the p-value
sum(perm1$r.squared >= rsquared(lm1))



# Question 2: is there an interaction between fireplaces and fuel system?

lm2 = lm(price ~ livingArea + lotSize + fireplaces + fuel + fireplaces:fuel, data=SaratogaHouses)
coef(lm2)
rsquared(lm2)

perm2 = do(2000)*{
	lm(price ~ livingArea + lotSize + fireplaces + fuel + shuffle(fireplaces):shuffle(fuel), data=SaratogaHouses)
}

hist(perm2$r.squared, 20)
abline(v=rsquared(lm2), col='red')

# p-value
sum(perm2$r.squared >= rsquared(lm2))
sum(perm2$r.squared >= rsquared(lm2))/2000

# Compare with p-values from ANOVA table
simple_anova(lm2)
