library('MVA')
demo('Ch-PCA')

# chp 3.4
blood_pcacov <- princomp(covmat = blood_cov)
summary(blood_pcacov, loadings = TRUE)
plot(blood_pcacov)

blood_pcacor <- princomp(covmat = blood_corr)
summary(blood_pcacor, loadings = TRUE)
plot(blood_pcacor)

# chp 3.10.1：二维，演示PCA旋转
head_dat <- headsize[, c("head1", "head2")]
colMeans(head_dat)
cov(head_dat)

head_pca <- princomp(x = head_dat)
head_pca
summary(head_pca, loadings = TRUE)

# 原始散点图
a1<-183.84-0.721*185.72/0.693
b1<-0.721/0.693
a2<-183.84-(-0.693*185.72/0.721)
b2<--0.693/0.721
plot(head_dat, xlab = "First son's head length (mm)",
     ylab = "Second son's head length")
abline(a1, b1)
abline(a2, b2, lty = 2)


# 转换后的散点图
xlim <- range(head_pca$scores[,1])
plot(head_pca$scores, xlim = xlim, ylim = xlim)


# chp 3.10.2 分数与主成份的关系
heptathlon$hurdles <- with(heptathlon, max(hurdles)-hurdles)
heptathlon$run200m <- with(heptathlon, max(run200m)-run200m)
heptathlon$run800m <- with(heptathlon, max(run800m)-run800m)

score <- which(colnames(heptathlon) == "score")
round(cor(heptathlon[,-score]), 2)
plot(heptathlon[,-score])

heptathlon_pca <- prcomp(heptathlon[, -score], scale = TRUE)
summary(heptathlon_pca)
plot(heptathlon_pca)

center <- heptathlon_pca$center
scale <- heptathlon_pca$scale


hm <- as.matrix(heptathlon[,-score])
drop(scale(hm, center = center, scale = scale) 
     %*% heptathlon_pca$rotation[,1])
predict(heptathlon_pca)[,1:2] # 与上面等价

cor(heptathlon$score, heptathlon_pca$x[,1]) # 分数与第一个的相关性
plot(heptathlon$score, heptathlon_pca$x[,1])


# chp 3.10.3 主成份可视化和回归分析---方差小，但是效果不小
cor(USairpollution[,-1])
usair_pca <- princomp(USairpollution[,-1], cor = TRUE)


data("USairpollution", package = "HSAUR2")
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="grey", ...)
}
USairpollution$negtemp <- USairpollution$temp * (-1)
USairpollution$temp <- NULL
pairs(USairpollution[,-1], diag.panel = panel.hist,
      pch = ".", cex = 1.5)
summary(usair_pca, loadings = TRUE)

# 绘制主成份
pairs(usair_pca$scores[,1:3], ylim = c(-6, 4), xlim = c(-6, 4),
      panel = function(x,y, ...) {
      text(x, y, abbreviate(row.names(USairpollution)),
           cex = 0.6)
      bvbox(cbind(x,y), add = TRUE)
})


out <- sapply(1:6, function(i) {
  plot(USairpollution$SO2,usair_pca$scores[,i],
        xlab = paste("PC", i, sep = ""),
        ylab = "Sulphur dioxide concentration")
  })


# 使用主成分回归
usair_reg <- lm(SO2 ~ usair_pca$scores,
                data = USairpollution)
summary(usair_reg)

usair_reg2 <- lm(SO2 ~ .,
                data = USairpollution)
summary(usair_reg2)


# 3.13.1 CCA 典型性分析例子
headsize.std <- sweep(headsize, 2,
                      apply(headsize, 2, sd), FUN = "/")
R <- cor(headsize.std)
r11 <- R[1:2, 1:2]
r22 <- R[-(1:2), -(1:2)]
r12 <- R[1:2, -(1:2)]

(E1 <- solve(r11) %*% r12 %*% solve(r22) %*%r21)
(E2 <- solve(r22) %*% r21 %*% solve(r11) %*%r12)
(e1 <- eigen(E1))
(e2 <- eigen(E2))

girth1 <- headsize.std[,1:2] %*% e1$vectors[,1]
girth2 <- headsize.std[,3:4] %*% e2$vectors[,1]

shape1 <- headsize.std[,1:2] %*% e1$vectors[,2]
shape2 <- headsize.std[,3:4] %*% e2$vectors[,2]
(g <- cor(girth1, girth2))
(s <- cor(shape1, shape2))
plot(girth1, girth2)
plot(shape1, shape2)

# 3.13.2 CCA 典型性分析例子2









