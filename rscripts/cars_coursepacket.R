library(ggplot2)
library(plyr)
library(mosaic)
source('simple_anova.R')

cars = read.csv("../data/cars.csv")
theme_set(theme_bw())

# cars$Class ="Sedan"

# classes = colnames(cars)[1:5]
# for(lab in 1:length(classes)) {
	# id = which(cars[,lab] == 1)
	# cars$Class[id] = classes[lab]
# }
# cars$Class = factor(cars$Class)

# cars = cars[,-(1:5)]
# write.csv(cars, file='../data/cars.csv')

pdf("../figures/car_mileage.pdf", height=3, width=10)

qplot(Horsepower, HighwayMPG, facets=.~Class, 
	colour=I(rgb(0.5,0.5,0.5,0.5)),
	main="Highway Gas Mileage versus Engine Power", data=subset(cars, HighwayMPG < 52))
	
dev.off()	


pdf("../figures/car_cylinders.pdf", height=3, width=10)

qplot(factor(Cylinders), Horsepower, facets=.~ Class, geom = "boxplot",
	colour=I(rgb(0.5,0.5,0.5,0.5)), xlab="Engine Cylinders",
	main="Horsepower versus Number of Engine Cylinders", data=subset(cars, Cylinders %% 2 == 0 & Cylinders < 12))

dev.off()


plot_with_lines = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(lm(y ~ x))         
}

# Pass this custom plotting function to xyplot
xyplot(HighwayMPG ~ Horsepower | Class, panel=plot_with_lines,
	data=subset(cars, HighwayMPG < 40))

qplot(Horsepower, HighwayMPG, facets=.~Class, 
	geom=c("point"),
	colour=I(rgb(0.5,0.5,0.5,0.5)),
	main="Highway Gas Mileage versus Engine Power (40 MPG or less)",
	data=subset(cars, HighwayMPG < 40))
	

lm_eqn = function(df){
    m = lm(HighwayMPG ~ Horsepower, df);
    eq <- substitute(y == a - b %.% x, 
         list(a = as.numeric(format(coef(m)[1], digits = 2)), 
              b = abs(as.numeric(format(coef(m)[2], digits = 2))),
              digits = 3))
    as.character(as.expression(eq));                 
}
eq <- ddply(subset(cars, HighwayMPG < 40), .(Class),lm_eqn)

pdf("../figures/car_interaction_fit.pdf", height=3, width=10)
p <- ggplot(data = subset(cars, HighwayMPG < 40), aes(x = Horsepower, y = HighwayMPG)) +
            geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
            geom_point(colour=I(rgb(0.5,0.5,0.5,0.5)))
p + facet_grid(.~Class) + geom_text(data=eq, aes(x = 300, y = 38,label=V1), parse = TRUE, inherit.aes=FALSE) + 
	ggtitle("Highway Gas Mileage versus Engine Power, with fitted lines (40 MPG or less)")
dev.off()

pdf("../figures/car_collinearity.pdf", height=5.5, width=3)
par(mar=c(4,4,1,1))
boxplot(Horsepower ~ Class, data=subset(cars, HighwayMPG < 40),
	horizontal=TRUE, las=1, border = "blue", col='lightgrey', boxwex=0.4,
	xlab="Horsepower")
dev.off()

lm1 = lm(HighwayMPG ~ Horsepower + Class + Class:Horsepower, data=subset(cars, HighwayMPG < 40))
lm2 = lm(HighwayMPG ~ Class + Horsepower + Class:Horsepower, data=subset(cars, HighwayMPG < 40))


anova(lm1)
anova(lm2)

write.table(round(anova(lm1)[,1:2], 0), quote=FALSE, sep=" & ", eol=" \\\\\n")

write.table(round(coef(lm1), 2), quote=FALSE, sep=" & ", eol=" \\\\\n")

