### Soldering
library(mosaic)

solder = read.csv("../data/solder.csv")

# All features look important
bwplot(skips ~ Opening, data=solder)
bwplot(skips ~ Solder, data=solder)
bwplot(skips ~ Mask, data=solder)

# All pairwise interactions
lm1 = lm(skips ~ Opening + Solder + Mask + Solder:Mask + Opening:Solder + Opening:Mask, data=solder)
coef(lm1)

boot1 = do(1000)*{
  lm(skips ~ Opening + Solder + Mask + Solder:Mask + Opening:Solder + Opening:Mask, data=resample(solder))
}

# A few main effects
hist(boot1$OpeningM)
hist(boot1$SolderThin)

# An interaction: this combination is better
# than you'd expect based on main effects only
hist(boot1$OpeningM.SolderThin)

# The best combination?
which.min(fitted(lm2))  # This will be tied with lots of observations
solder[129,]
fitted(lm2)[129]

# The optimal combination looks like this
subset(solder, Opening == 'M' & Solder == 'Thin' & Mask == 'A1.5')

# Compare with a "neighbor"
subset(solder, Opening == 'S' & Solder == 'Thin' & Mask == 'A1.5')

# A big caveat: what if we need a three-way interaction?
lm2 = lm(skips ~ Opening + Solder + Mask + Solder:Mask + Opening:Solder + Opening:Mask + Opening:Solder:Mask, data=solder)
coef(lm2)
anova(lm2)

which.min(fitted(lm2))
solder[6,]

# It looks like our first conclusion was wrong...
subset(solder, Opening == 'L' & Solder == 'Thick' & Mask == 'A1.5')

mean(skips ~ Opening:Solder:Mask, data=solder)
boxplot(skips ~ Opening:Solder:Mask, data=solder)

