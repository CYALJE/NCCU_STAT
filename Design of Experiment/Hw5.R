epit = read.table("2kfactorial.data")

epitnew = data.frame(A = rep(epit[, 1], 6),
                     B = rep(epit[, 2], 6),
                     C = rep(epit[, 3], 6),
                     D = rep(epit[, 4], 6),
                     y = as.vector(as.matrix(epit[, 5:10])))

# coding
g1 = lm(y ~ A * B * C * D, data = epitnew, contrasts = list(A = contr.sum, B = contr.sum, C = contr.sum, D = contr.sum))
model.matrix(g1)
summary(g1)
anova(g1)
