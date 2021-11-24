# 1
resin = read.table("http://www.stat.umn.edu/~gary/book/fcdae.data/exmpl3.2", header = T)
y = resin$y
temp = resin$temp
g = lm(y ~ factor(temp), data = resin)
summary(g)
anova(g)
mu_hat = mean(y)
group_mean = tapply(y, temp, mean)
alpha_hat = group_mean - mu_hat
table = cbind(group_mean, alpha_hat)
table
mean(resin[, 2])

# 2
angle = read.table("http://www.stat.umn.edu/~gary/book/fcdae.data/ex3.5", header = T)
angle
model = lm(deg ~ factor(trt), data = angle)
summary(model)
anova(model)
group_mean = tapply(angle$deg, angle$trt, mean)
group_mean
deg_mean = mean(angle$deg)
deg_mean
treatment_effects = group_mean - deg_mean
treatment_effects
