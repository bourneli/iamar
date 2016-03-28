# 随机生成聚类数据
set.seed(1234)
group_size <- 100
group1 <- data.frame(x1 = rnorm(group_size, mean=1),
                     x2 = rnorm(group_size, mean=1),
                     x3 = rnorm(group_size, mean=1),
                     group = 'red')
group2 <- data.frame(x1 = rnorm(group_size, mean=7),
                     x2 = rnorm(group_size, mean=7),
                     x3 = rnorm(group_size, mean=4.5),
                     group = 'green')
group3 <- data.frame(x1 = rnorm(group_size, mean=4),
                     x2 = rnorm(group_size, mean=3),
                     x3 = rnorm(group_size, mean=4.5),
                     group = 'blue')
group4 <- data.frame(x1 = rnorm(group_size, mean=5),
                     x2 = rnorm(group_size, mean=7),
                     x3 = rnorm(group_size, mean=10),
                     group = 'black')
data <- rbind(group1, group2, group3, group4)

# 2d
pairs(data[,1:3],col=data$group)

# 3d
require(rgl)
require(car)
with(data,
     plot3d(x1,x2,x3,col=group))

# MSD
dd <- dist(data[,1:3])
fit <- cmdscale(dd,eig=TRUE, k=2)
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, pch=19, col = data$group)

# 添加线性组合
derive_data <- cbind(data, 
                     x4 = jitter(data$x1 + data$x2),
                     x5 = jitter(data$x2 + data$x3),
                     x6 = jitter(data$x1 + data$x3),
                     x7 = jitter(data$x1 + data$x2 + data$x3),
                     x8 = jitter(data$x1 + 2*data$x2 + 1.5*data$x3),
                     x9 = jitter(data$x1 + data$x2),
                     x10 = jitter(data$x2 + data$x3),
                     x11 = jitter(data$x1 + data$x3),
                     x12 = jitter(data$x1 + data$x2 + data$x3),
                     x13 = jitter(data$x1 + 2*data$x2 + 1.5*data$x3),
                     x14 = jitter(10000000*data$x1))

cor_dist <- dist(derive_data[,-4])
fit <- cmdscale(cor_dist,eig=TRUE, k=2) 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, pch=19, col = derive_data$group)

## pca
pca_mode <- princomp(derive_data[,-4])
plot(pca_mode)
summary(pca_mode)

pca_data <- predict(pca_mode,derive_data[,-4])
head(pca_data)
plot(pca_data[,1],pca_data[,2],col=derive_data$group, pch=19)





