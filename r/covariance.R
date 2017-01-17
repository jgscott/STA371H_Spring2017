library(mosaic)
data(SaratogaHouses)

ptab = prop.table(xtabs(~bedrooms + ceiling(bathrooms), data=SaratogaHouses))
ptab = ptab[-7,-c(1,6)]
ptab = ptab/sum(ptab)
ptab = round(ptab, 3)

dimnames(ptab) = NULL
colnames(ptab) = 1:4
write.table(ptab, sep=" & ", eol="\\\\\n", quote=FALSE)

mu1 = sum( colSums(ptab) * {1:4} )
var1 = sum( colSums(ptab) * {1:4 - mu1}^2 )

mu2 = sum( rowSums(ptab) * {1:6} )
var2 = sum( rowSums(ptab) * {1:6 - mu2}^2 )


cov3 = 0
for(i in 1:6) {
	for(j in 1:4) {
		cov3 = cov3 + ptab[i,j]*(i-mu2)*(j-mu1)
	}
}