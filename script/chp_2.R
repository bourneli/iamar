library('MVA')
demo("Ch-MVA") # generate data
demo('Ch-Viz')

mlab <- "Manufacturing enterprises with 20 or more workers"
plab <- "Population size (1970 census) in thousands"

# 绘制闪点图和单独的直方图
par(mfrow=c(1,1))
plot(popul ~ manu, data = USairpollution, xlab = mlab, ylab = plab)
rug(USairpollution$manu, side = 1)
rug(USairpollution$popul, side = 2)

layout(matrix(c(2, 0, 1, 3), nrow = 2, byrow = TRUE),
       widths = c(2, 1), heights = c(1, 2), respect = TRUE)
xlim <- with(USairpollution, range(manu)) * 1.1
plot(popul ~ manu, data = USairpollution, cex.lab = 0.9,
    xlab = mlab, ylab = plab, type = "n", xlim = xlim)
with(USairpollution, text(manu, popul, cex = 0.6,
                          labels = abbreviate(row.names(USairpollution))))
with(USairpollution, hist(manu, main = "", xlim = xlim))
with(USairpollution, boxplot(popul))

## 二维boxplot
par(mfrow=c(1,1))
outcity <- match(lab <- c("Chicago", "Detroit",
                          "Cleveland", "Philadelphia"), rownames(USairpollution))
x <- USairpollution[, c("manu", "popul")]
bvbox(x, mtitle = "", xlab = mlab, ylab = plab)
text(x$manu[outcity], x$popul[outcity], labels = lab,
      cex = 0.7, pos = c(2, 2, 4, 2, 2))

## 异类可能影响关联性
with(USairpollution, cor(manu, popul))

# 去掉异类数据后
outcity <- match(c("Chicago", "Detroit",
                   "Cleveland", "Philadelphia"),
                   rownames(USairpollution))
with(USairpollution, cor(manu[-outcity], popul[-outcity]))

## 通过去掉凸包点，观察关联系数
(hull <- with(USairpollution, chull(manu, popul)))
with(USairpollution, plot(manu, popul, pch = 1, xlab = mlab, ylab = plab))
with(USairpollution, polygon(manu[hull], popul[hull], density = 15, angle = 30))
with(USairpollution, cor(manu[-hull],popul[-hull])) # 去掉凸包点

## chi-plot
par(mfcol=c(2,1))
with(USairpollution, plot(manu, popul,
                          xlab = mlab, ylab = plab,
                          cex.lab = 0.9))
with(USairpollution, chiplot(manu, popul))


## 泡泡图
par(mfrow=c(1,1))
ylim <- with(USairpollution, range(wind)) * c(0.95, 1)
plot(wind ~ temp, data = USairpollution,
     xlab = "Average annual temperature (Fahrenheit)",
     ylab = "Average annual wind speed (m.p.h.)", pch = 10,
     ylim = ylim)
with(USairpollution, symbols(temp, wind, circles = SO2, inches = 0.5, add = TRUE))

# 太多维度，没有意义，模式很难发现
plot(wind ~ temp, data = USairpollution,
     xlab = "Average annual temperature (Fahrenheit)",
     ylab = "Average annual wind speed (m.p.h.)", pch = 10,
     ylim = ylim)
with(USairpollution,
     stars(USairpollution[,-c(2,5)], locations = cbind(temp, wind),
     labels = NULL, add = TRUE, cex = 0.5))

# 组合闪点图
pairs(USairpollution, pch = ".", cex = 1.5)
round(cor(USairpollution),4)
pairs(USairpollution,
      panel = function (x, y, ...) {
        points(x, y, ...)
        abline(lm(y ~ x), col = "grey")
      }, pch = ".", cex = 1.5)
