caf = read.table("http://www.stat.umn.edu/~gary/book/fcdae.data/pr3.4", header = TRUE)

mean_y = tapply(caf$y, caf$trt, mean)
var_y = tapply(caf$y, caf$trt, var)
print(mean_y)

round(var_y, 3)

table(caf$trt)  

y = caf$y
dose = c(rep(0, 6), rep(0.1, 5), rep(0.5, 4), rep(1, 5), rep(5, 5), rep(10, 6), rep(25, 6), rep(50, 6))
plot(dose, y)


lm1 = lm(y ~ dose)
summary(lm1)
plot(x = dose, y = summary(lm1)$res)
abline(h = 0, col = "red")


lm2 = lm(y ~ dose + I(dose^2))
summary(lm2)
plot(x = dose, y = summary(lm2)$res)
abline(h = 0, col = "red")

plot(dose, log(y))


lm3 = lm(log(y) ~ dose)
summary(lm3)


lm4 = lm(log(y) ~ dose + I(dose^2))
summary(lm4)

