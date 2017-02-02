head(afc)

mean(Price ~ Neighborhood, data=afc)

# food or atmosphere?
cor(Price ~ FeelScore, data = afc)
cor(Price ~ FoodScore, data = afc)

plot(Price ~ FoodScore, data = afc)
lm1 = lm(Price ~ FoodScore, data = afc)
lm2 = lm(Price ~ FeelScore, data = afc)
sd(resid(lm1))
sd(resid(lm2))

# Adjust price for food score
adjusted_price = resid(lm1)
mean(adjusted_price ~ Neighborhood, data=afc)

xyplot(Price ~ FoodScore | Neighborhood, data=afc)
