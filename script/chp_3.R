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


# chp 3.10.2
heptathlon$hurdles <- with(heptathlon, max(hurdles)-hurdles)
heptathlon$run200m <- with(heptathlon, max(run200m)-run200m)
heptathlon$run800m <- with(heptathlon, max(run800m)-run800m)

score <- which(colnames(heptathlon) == "score")
round(cor(heptathlon[,-score]), 2)
plot(heptathlon[,-score])


