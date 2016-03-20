library('MVA')
demo("Ch-MVA") # generate data

## 1
x <- hypo[!is.na(hypo$age) & !is.na(hypo$IQ),c('age','IQ','weight')]
cov(x)
cor(x)

## 2
x <- hypo[,c('age','IQ','weight')]
cm <- colMeans(x[!is.na(x$age) & !is.na(x$IQ),])
x[is.na(x$age),'age'] <- cm['age']
x[is.na(x$IQ), 'IQ'] <- cm['IQ']

cov(x)
cor(x)

## 3
# 分开观察每个变量的分布是否是正太的
data <- pottery[,1:9]
layout(matrix(1:9, nc = 3))
sapply(colnames(data), function(x) {
  qqnorm(data[[x]], main = x)
  qqline(data[[x]])
})

# 将所有变量整合一起观察是否正太
x <- pottery[,1:9]
par(mfrow=c(1,1))
cm <- colMeans(x)
S <- cov(x)
d <- apply(x, 1, function(x) t(x - cm) %*% solve(S) %*% (x - cm))
plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 6),
     sd <- sort(d),
     ylab = "Ordered distances", 
     xlim = range(qc) * c(1, 1.1))

oups <- which(rank(abs(qc - sd), ties = "random") > nrow(x) - 3)
text(qc[oups], sd[oups]-0.5, names(oups))
abline(a = 0, b = 1)


## 4 
# 相关性矩阵  R=D^(-1/2)SD^(-1/2)
S <- matrix(c(3.8778,2.8110,3.1480,3.5062,
              2.8110,2.1210,2.2669,2.5690,
              3.1480,2.2669,2.6550,2.8341,
              3.5062,2.5690,2.8341,3.2352), 
            nrow=4)
D <- diag(S)
R <- diag(D^(-1/2)) %*% S %*% diag(D^(-1/2))
R

## 5
M <-matrix(c(3,6,4,0,7,
      4,2,7,4,6,
      4,0,3,1,5,
      6,2,6,1,1,
      1,6,2,1,4,
      5,1,2,0,2,
      1,1,2,6,1,
      1,1,5,4,4,
      7,0,1,3,3,
      3,3,0,5,1), ncol = 5, byrow=T)
dist(M, method = "euclidean")
dist(M, method = "manhattan")
