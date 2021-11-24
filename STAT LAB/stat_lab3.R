install.packages("faraway")

library(faraway)
data(gala)
gala

pairs(gala[, -c(1, 2)])

model = lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
summary(model)

CI_Area = -0.023938 + 0.022422 * c(qt(0.025, 24), qt(0.975, 24))
CI_Area

confint(model)

setting = data.frame(Intercept = 1, Area = 12.03, Elevation = 100, Nearest = 15.0, Scruz = 20.0, Adjacent = 12.0)
predict(model, setting)

par(mfrow = c(3, 3))
for (i in 1:9){
  plot(1:50, rnorm(50), main = "null plot"); abline(h = 0, col = "red")
}

par(mfrow = c(3, 3))
for (i in 1:9){
  plot(1:50, (1:50) * rnorm(50), main = "Strong non-constant Variance"); abline(h = 0, col = "red")
}

par(mfrow = c(3, 3))
for (i in 1:9){
  plot(1:50, cos((1:50) * pi / 25) + rnorm(50), main = "non-linearity"); abline(h=0, col = "red")
}

x = rnorm(50)
qqnorm(x)
qqline(x)

y = runif(50)
qqnorm(y)
qqline(y)

res = model$residual
res

plot(model$fit, model$res, xlab = "fitted value", ylab = "residual", main = "fitted value vs residual")
abline(h = 0, col = "red") 

qqnorm(res)
qqline(res)

model = lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
summary(model)

model2 = lm(Species ~ I(Area^2) + Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
summary(model2)

model3 = lm(Species ~ I(Area^2) + I(Elevation^2) + I(Nearest^2) + I(Scruz^2) + I(Adjacent) + Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
summary(model3)

model4 = lm(Species ~ I(Area^2) + I(Elevation^2) + I(Nearest^2) + I(Scruz^2) + I(Adjacent * Elevation) + Area + Elevation + Nearest + Scruz + Adjacent + Area*Elevation, data = gala)
summary(model4)

