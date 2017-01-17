library(mosaic)

# 1A: music festivals
head(musicfestivals)
table1 = xtabs(~acl + bonnaroo, data=musicfestivals)
table1

prop.table(table1, margin=2)

# Can chain the commands together
prop.table(xtabs(~acl + bonnaroo, data=musicfestivals), margin=2)
prop.table(xtabs(~acl + coachella, data=musicfestivals), margin=2)
prop.table(xtabs(~acl + lollapalooza, data=musicfestivals), margin=2)
prop.table(xtabs(~acl + outsidelands, data=musicfestivals), margin=2)

loll_table = prop.table(xtabs(~acl + lollapalooza, data=musicfestivals), margin=2)
loll_table[2,2]/loll_table[2,1]


# 1B: Chipotle
hist(burritos$calories)
summary(burritos$calories)

my_breaks = seq(0, 2600, by = 40)
hist(burritos$calories, breaks=my_breaks)

# mean calorie count?
mean(burritos$calories)
favstats(burritos$calories)

# What percentage of meals have more than 1600 calories?
pdata(burritos$calories, q=1600)
1 - pdata(burritos$calories, q=1600)

# 80% coverage interval
qdata(burritos$calories, p=c(0.1, 0.9))



# 2: S class cars
# So many possibilities!
summary(sclass)
xyplot(price ~ mileage | region, data=sclass)

# Notice how fast the crazy-expensive versions depreciate,
# compared with the merely expensive versions.
xyplot(price ~ mileage | trim, data=sclass)


# 3: Austin food critics
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
