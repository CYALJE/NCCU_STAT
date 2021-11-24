# obtain group means (μi) and treatment effects (αi)
resin = read.table("http://www.stat.umn.edu/~gary/book/fcdae.data/exmpl3.2", header = T)
y = resin$y
temp = resin$temp
g = lm(y ~ factor(temp), data = resin)
anova(g)
summary(g)
mu_hat = mean(y)
groupmean = tapply(y, temp, mean)
# group means
groupmean
# alpha_hat
alpha_hat = groupmean - mu_hat
# 製表
table = cbind(groupmean, alpha_hat)
table


# in Example 3.6, obtain ANOVA table
resin = read.table("http://www.stat.umn.edu/~gary/book/fcdae.data/exmpl3.2", header = T)
model.matrix(g1)
g1 = lm(y ~ factor(temp), data = resin)
anova(g1)
