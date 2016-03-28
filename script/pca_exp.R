## 原始数据
n <- 1000
set.seed(4546576)
data <- data.frame(x1 = rnorm(n), 
                   x2 = rnorm(n, mean=12,sd=5), 
                   x3 = 3*rnorm(n, mean=-1,sd=3.5))
cor(data)
pairs(data,pch=19)


## 衍生数据
derive_data <- cbind(data,
                     x4=10*jitter(data$x1), 
                     x5=jitter(data$x2+0.6*data$x3),
                     x6=12*jitter(data$x1+data$x2),
                     x7=16*jitter(data$x1),
                     x8=7*jitter(data$x1))
cor(derive_data)



# 方法：直接pca
m1 <- princomp(derive_data)
summary(m1)
plot(m1)

# 方法：使用关联系数
model_corr <- princomp(covmat = cor(derive_data))
summary(model_corr)
plot(model_corr)

# 方法：标准正规化，效果与关联系数一致
data_normal <- as.data.frame(scale(derive_data, 
                                  center=T,
                                  scale=T))
model_normal <- princomp(data_normal)
plot(model_normal)
summary(model_normal)

# 方法：仅将平均设置为1 结果与方案1一致
data_zero_mean <- as.data.frame(scale(derive_data, 
                                      center=T,
                                      scale=F))
model_zero_mean <- princomp(data_zero_mean)
plot(model_zero_mean)
summary(model_zero_mean)

# 方案4 01正规化
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
scale_data_2 <- minMaxScale(derive_data)
m4 <- princomp(scale_data_2)
summary(m4)
plot(m4)

# 添加一个异类主导的例子



# # 方法4 只计算衍生数据的主成份
# only_derive_data <- scale_data[,4:8]
# m4 <- princomp(only_derive_data)
# plot(m4)
# summary(m4)
# 
# # 方法5 只计算衍生数据，使用相关系数
# m5 <- princomp(covmat = cor(derive_data[,4:8]))
# plot(m5)
# summary(m5)


# 据网上问下，当scale是正规化时，correlation与normal等价
# https://www.researchgate.net/post/What_is_the_best_way_to_scale_parameters_before_running_a_Principal_Component_Analysis_PCA
# 方法6 比较先正规化，在pca和直接pca cor的效果，应该一样，还有减去平均值。



