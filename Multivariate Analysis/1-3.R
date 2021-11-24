# 1.
# prepare the data
sales = read.table("/Users/c/MAMidterm/sales.data", header = T)
head(sales, 10)
pairs(sales)
cor(sales)
# PCA
library(stats)
pca_sales = princomp(sales, cor = TRUE)
print(pca_sales)
summary(pca_sales)
# scree plot
screeplot(pca_sales, type = "lines")
abline(h = 1, col = "red")
abline(h = 0.7, col = "blue")
# permutation test
sign.pc<-function(x,R=1000,m=length(x), cor=T,...){
  # run PCA
  pc.out<-princomp(x,cor=cor,...)
  # the proportion of variance of each PC
  pve=(pc.out$sdev^2/m)[1:m]
  # a matrix with R rows and m columns that contains
  # the proportion of variance explained by each pc
  # for each randomization replicate.
  pve.perm<-matrix(NA,ncol=m,nrow=R)
  
  for(i in 1:R){
    # permutation each column
    x.perm<-apply(x,2,sample)
    
    # run PCA
    pc.perm.out<-princomp(x.perm,cor=cor,...)
    
    # the proportion of variance of each PC.perm
    pve.perm[i,]=(pc.perm.out$sdev^2/m)[1:m]
  }
  # calcalute the p-values
  pval<-apply(t(pve.perm)>pve,1,sum)/R
  return(list(pve=pve,pval=pval))
}
print(sign.pc(sales, cor = TRUE))

# loadings
loadings(pca_sales)

pcs_sales = predict(pca_sales)
biplot(pca_sales, scale = 1)


# 2.
library(psych)
library(MVN)
mvn(sales, mvnTest = "mardia")
mvn(sales, mvnTest = "hz")
mvn(sales, mvnTest = "royston")
mvn(sales, mvnTest = "dh")
mvn(sales, mvnTest = "energy")
hzTest(sales)
mardiaTest(sales)
mvn(sales)
roystonTest(sales)
sales = read.table("/Users/c/MAMidterm/sales.data", header = T)
sales_cor_mat = cor(sales)
sales_cor_mat
# # of factor = 1
sales_mle = factanal(covmat = as.matrix(sales_cor_mat), factors = 1, n.obs = 50, method = "mle")
print(sales_mle)
sales_mle = factanal(covmat = as.matrix(sales_cor_mat), factors = 1, n.obs = 50, method = "mle", rotation = "promax")

print(1 - sales_mle$uniq)
scree(sales)
fa.diagram(fa(sales))
sales_mle = factanal(covmat = as.matrix(sales_cor_mat), factors = 3, n.obs = 50, method = "mle")
plot(sales_mle$scores[, 1], sales_mle$scores[, 2], type = "n",
     xlab = 'Factor 1', ylab = 'Factor 2')
text(sales_mle$scores[, 1:2], row.names(sales))

# Principal Axis Factor Analysis
library(psych)
fit <- factor.pa(sales, nfactors=1)
fit # print results
plot(fit)
# 3. 論述性題目
