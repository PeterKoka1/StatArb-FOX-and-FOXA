library(tseries)
df <- read.csv('Indicators_Joined_.csv')
names(df)
dropcols <- c('X')
preds <- df[, !(names(df) %in% dropcols)] 
stock.prices <- preds[, 3:dim(preds)[2]-1]
names.ind <- seq(1, length(names(stock.prices)), 2)
stocks.df <- stock.prices[, names.ind]

cors <- cor(stocks.df)
highs = rep(0, dim(cors)[1]); ind = c(NULL,NULL)
mincorval <- 0.86; high <- 0
for (name in names(stocks.df)) {
  for (i in 1:dim(cors)[1]){
    cor <- cors[i, name]
    if ((cor > mincorval)&(cor != 1)) {
      highs[i] = cor
      if ((cor > high)&(cor != 1)) {
        high <- cor
        ind[1] <- name; ind[2] <- i
      }
    }
  }  
}

high # 0.9995816
ind # "GOOGL" "25"
names(stocks.df)[25] # "GOOG"

pr.out <- prcomp(stocks.df, scale = TRUE, retx = TRUE)
pr.var <- pr.out$sdev^2
PVE <- pr.var / sum(pr.var)
par(mfrow=c(1,1))
barplot(PVE[1:20], xlab = "Principal Component", ylab = "Proportion of Variance Explained",
        ylim = c(0,1), col = "navyblue")
lines(cumsum(PVE[1:20]), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1), col = "navyblue", type = "l", pch = 19)
cumsum(PVE[1:20])[3] 
# 3 PCs explain 84.65% of Variance

principal.comps <- pr.out$x[,1:3]
upd.pred <- cbind(principal.comps, df["USvsEURO"])

Cols <- function(vec) {
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
stocks.labs <- names(stocks.df)
par(mfrow = c(1,3))
plot(c(upd.pred$PC1,upd.pred$PC3), col = Cols(stocks.labs), pch=19,
     xlab = "Z1", ylab = "Z3")
plot(c(upd.pred$PC2,upd.pred$PC3), col = Cols(stocks.labs), pch = 19,
     xlab = "Z2", ylab = "Z3")
plot(c(upd.pred$PC1,upd.pred$PC2), col = Cols(stocks.labs), pch = 19,
     xlab = "Z1", ylab = "Z2")

sectors <- read.csv("Sectors.csv")
closes <- sectors[,c(".INX_Close","XLF_Close")]
closes <- na.omit(closes)

par(mfrow=c(1,2))
plot(closes$.INX_Close, type = "l", ylab = "SP500 Closes", xlab = "Time")
INX.first <- diff(closes$.INX_Close, lag = 1, differences = 1)
plot(INX.first, type = "l", ylab = "Difference", xlab = "Time")

par(mfrow=c(1,2))
plot(closes$XLF_Close, type = "l", ylab = "XLF Closes", xlab = "Time")
XLF.first <- diff(closes$XLF_Close, lag = 1, differences = 1)
plot(XLF.first, type = "l", ylab = "Difference", xlab = "Time")

par(mfrow=c(1,1))
plot(INX.first[500:700]/50, type = "l", ylab = "Difference", xlab = "Time")
lines(XLF.first[500:700], col = "red")
legend("topright", legend=c("Diff INX / 20", "Diff XLF"),
       col=c("black","red"), lty=c(1,1), lwd=c(3,2), seg.len = 2, cex=0.8)

series <- data.frame(INX.first, XLF.first)
lm.fit <- lm(INX.first ~ XLF.first, data = data)
summary(lm.fit)

resids <- lm.fit$residuals
acf(resids, type = "correlation", plot = TRUE)
resids.first <- diff(resids, lag = 1, differences = 1)
adf.resids <- adf.test(resids.first, alternative = "stationary", k = 0)
adf.resids$statistic

beta <- lm.fit$coefficients
alpha <- 0
adf.coint <- adf.test(series$XLF.first - alpha - beta * series$INX.first, alternative = "stationary", k = 0)
adf.coint$statistic

#####
par(mfrow=c(1,2))
plot(XLF.first, INX.first)
abline(lm.fit)
plot(resids, type = "l")
###
