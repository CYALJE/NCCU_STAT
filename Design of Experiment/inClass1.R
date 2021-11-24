# randomization test for paired data
# x = read.table("http://www.stat.umn.edu/~gary/book/fcdae.data/exmpl2.1" , header=TRUE)
# diffx=x[,2]-x[,1]
# t.test(x[21:30,1],x[21:30,2],alternative="greater",paired=TRUE)
# p-value = 0.4595 for paired t test
# H0: mu_x = mu_y vs H1: mu_x > mu_y
# Now use randomization test (Find the p-value)

data <- read.table("http://www.stat.umn.edu/~gary/book/fcdae.data/exmpl2.1" , header=TRUE)
diff_data <- data[, 2] - data[, 1]

x <- diff_data[21:30]

df <- data.frame(matrix(0, ncol = 1024, nrow = 0))
for (i in 1:length(x)){
  df[nrow(df) + 1,] = c(replicate(2**(i - 1), rep(c(x[i], (-1) * x[[i]]), each = 2**(length(x) - i))))
}
df["Sum", ] = apply(df, 2, sum)
################################
# another way to generate data
for (i in 1:10){
  new = x
  new[i:10] = -new[i:10]
  x = cbind(x, new)
}
################################
total <- df[11, ]
for (i in 1:ncol(total)){
  if (total[1, i] >= 0.23){
    total[">=0.23", i] <-  1
  }
  else{
    total[">=0.23", i] <-  0
  }
}
sum(total[2, ])
p_value <- sum(total[2, ]) / 1024
