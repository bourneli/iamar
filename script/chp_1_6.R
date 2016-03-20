library('MVA')
library('HSAUR2')
demo("Ch-MVA") # generate data

head(measure)
head(pottery)
head(exam)
head(USairpollution)

x <- measure[, c("chest", "waist", "hips")]
cm <- colMeans(x)
S <- cov(x)

# 计算综合距离，多元正太分布
d <- apply(x, MARGIN = 1, function(x)
  + t(x - cm) %*% solve(S) %*% (x - cm))

par('mfcol'=c(1,3))
qqnorm(measure[,"chest"], main = "chest"); qqline(measure[,"chest"])
qqnorm(measure[,"waist"], main = "waist"); qqline(measure[,"waist"])
qqnorm(measure[,"hips"], main = "hips"); qqline(measure[,"hips"])

# 综合效果
par('mfcol'=c(1,1))
plot(qchisq((1:nrow(x) - 1/2) / nrow(x), df = 3), 
     sort(d),
     ylab = "Ordered distances")
abline(a = 0, b = 1)


# 另外一个数据集
layout(matrix(1:8, nc = 2))
sapply(colnames(USairpollution), function(x) {
  qqnorm(USairpollution[[x]], main = x)
  qqline(USairpollution[[x]])
})




# 综合效果
x <- USairpollution
cm <- colMeans(x)
S <- cov(x)
d <- apply(x, 1, function(x) t(x - cm) %*% solve(S) %*% (x - cm))
plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 6),
      sd <- sort(d),
      ylab = "Ordered distances", 
     xlim = range(qc) * c(1, 1.1))

oups <- which(rank(abs(qc - sd), ties = "random") > nrow(x) - 3)
text(qc[oups], sd[oups] - 1.5, names(oups))
abline(a = 0, b = 1)



