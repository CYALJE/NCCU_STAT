# 4.
library(MVN)
air = read.table("/Users/c/MAMidterm/air_pollution.data")
air
x = cbind(air$PT, air$CO, air$SO2)
y = cbind(air$Temp, air$Man, air$Pop, air$Rain)
cxy = cancor(scale(x, scale = T, center = T),
             scale(y, scale = T, center = T))
cxy


mvn(air, mvnTest = c("mardia"), desc = FALSE, multivariatePlot = "qq", 
    multivariateOutlierMethod = "adj", showOutliers = TRUE)
mvn(air, mvnTest = c("hz"), desc = FALSE, multivariatePlot = "qq", 
    multivariateOutlierMethod = "adj", showOutliers = TRUE)
mvn(air, mvnTest = c("royston"), desc = FALSE, multivariatePlot = "qq", 
    multivariateOutlierMethod = "adj", showOutliers = TRUE)
mvn(air, mvnTest = c("dh"), desc = FALSE, multivariateOutlierMethod = 
      "adj", showOutliers = TRUE)
mvn(log(air), mvnTest = c("mardia"), desc = FALSE, multivariatePlot = "qq", 
    multivariateOutlierMethod = "adj", showOutliers = TRUE)
mvn(log(air), mvnTest = c("hz"), desc = FALSE, multivariatePlot = "qq", 
    multivariateOutlierMethod = "adj", showOutliers = TRUE)
mvn(log(air), mvnTest = c("royston"), desc = FALSE, multivariatePlot = "qq", 
    multivariateOutlierMethod = "adj", showOutliers = TRUE)
mvn(log(air), mvnTest = c("dh"), desc = FALSE, multivariateOutlierMethod = 
      "adj", showOutliers = TRUE)

library(CCP)
newair = log(air[, -4])
newx = cbind(newair[, 1], newair[, 2], newair[, 3])
newy = cbind(newair[, 4], newair[, 5], newair[, 6], newair[, 7])
newcxy = cancor(scale(newx, scale = T, center = T), scale(newy, scale = T, center = T))
rho = newcxy$cor
p.asym(rho, 41, 3, 4, tstat = "Wilks")
newcxy
xx = scale(newx, scale = T, center = T)
yy = scale(newy, scale = T, center = T)
scorex = xx %*% newcxy$xcoef[, 1]
scorey = yy %*% newcxy$ycoef[, 1]
cor(scorex, scorey)
plot(scorex, scorey, type = "n")
text(scorex, scorey, row.names(air), cex = 1)
abline(0, 1, col = "red")

u = newcxy$xcoef[, 1] * newair[, c(1, 2, 3)]
v = newcxy$ycoef[, 1] * newair[, c(4, 5, 6, 7)]
plot(u, v)
cor(u, v)
var(u)
# 5.
com = read.table("/Users/c/Desktop/combine.data")
com
chisq.test(com)
com_ca = ca(com, nd = 1)
com_ca
plot.ca(com_ca)

chisq.test(car)
car = read.table("/Users/c/MAMidterm/car_family.data")
car
#install.packages("ca")
library(ca)
car_ca1 = ca(car, nd = 1)
car_ca2 = ca(car, nd = 2)
car_ca1
car_ca2
plot(car_ca2)

# 6.
library(ca)
iss = read.table("/Users/c/MAMidterm/internet_shopping.data")
is_mca_indicator = mjca(iss, nd = 2, lambda = "indicator")
is_mca_indicator
summary(is_mca_indicator)
plot(is_mca_indicator)
is_mca_burt = mjca(iss, nd = 2, lambda = "Burt")
is_mca_burt
summary(is_mca_burt)
plot(is_mca_burt)
plot(is_mca_burt, what = c("all", "all"), col = c("blue", "red"))
is_mca_adjusted = mjca(iss, nd = 2, lambda = "adjusted")
is_mca_adjusted
summary(is_mca_adjusted)
plot(is_mca_adjusted)
is_mca_jca = mjca(iss, nd = 2, lambda = "JCA")
is_mca_jca
summary(is_mca_jca)
plot(is_mca_jca)
plot(is_mca_jca, what = c("all", "all"), col = c("blue", "red"))
plot.mjca(is, dim = c(1, 2))
MCA(iss, quali.sup = c(1:19), ncp = 2)
plot.MCA(is_mca_jca, axes = c(1, 2))
plot.MCA(is_mca_jca, invisible = c('ind', 'var'))
