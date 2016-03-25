## 原始数据
n <- 1000
set.seed(4546576)
data <- data.frame(x1 = rnorm(n), 
                   x2 = rnorm(n, mean=12,sd=5), 
                   x3 = 3*rnorm(n, mean=-1,sd=3.5))
# data <- data.frame(x1 = rnorm(n), 
#                    x2 = rnorm(n), 
#                    x3 = rnorm(n))

pairs(data)
cor(data)
hist(data$x1)
hist(data$x2)
hist(data$x3)

plot(data$x1, data$x2)

## 衍生数据
derive_data <- cbind(data,
                     x4=10*jitter(data$x1), 
                     x5=jitter(data$x2+0.6*data$x3),
                     x6=12*jitter(data$x1),
                     x7=16*jitter(data$x1),
                     x8=7*jitter(data$x1))
cor(derive_data)
hist(derive_data$x4)
hist(derive_data$x5)

# 方案1 直接pca
m1 <- princomp(derive_data)
summary(m1)
plot(m1)

# 方案2 使用关联系数
m2 <- princomp(covmat = cor(derive_data))
summary(m2)
plot(m2)

# 方案3 01正规化
minMaxScale <- function(data) {
  max_list <- apply(data,2,max)
  min_list <- apply(data,2,min)
  range_list <- max_list - min_list
  
  scale_data <- data
  for(i in 1:ncol(scale_data)) {
    scale_data[,i] <- (scale_data[,i] - min_list[i])/range_list[i]
  }
  return(scale_data)
}
scale_data <- minMaxScale(derive_data)
m3 <- princomp(scale_data)
summary(m3)
plot(m3)

# 方法4 标准正规化




