install.packages("dse")
library(tseries)
install.packages("plotrix")
library(plotrix)
df <- read.csv('Indicators_Joined_.csv')
df3yr <- df[757:length(df$Periods), ]
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





library(quantmod)
normed.df <- stocks.df # copy of stocks.df for normalized prices
for (i in names(stocks.df)) {
  eq <- stocks.df[,i]
  normed.df[, i] <- (eq - min(eq)) / (max(eq) - min(eq))
}
TS <- ts(stocks.df) # time series of stocks.df
rets <- TS / lag(TS, k = 1) - 1 # calculating daily pct change
rets <- data.frame(rets)

sectors <- read.csv("Sectors.csv")[757:length(df$Periods), ]
closes <- sectors[,c(".INX_Close","XLF_Close")]
closes <- na.omit(closes)

normed.sec <- closes # copy of closes for normalized prices
for (i in names(normed.sec)) {
  eq <- normed.sec[,i]
  normed.sec[, i] <- (eq - min(eq)) / (max(eq) - min(eq))
}

sec.ts <- ts(closes)
sec.rets <- data.frame(sec.ts / lag(sec.ts, k = 1) - 1)
names(sec.rets) <- c("INX", "XLF")


########################
### INX vs. XLF Plots###
########################
par(mfrow=c(1,2))
plot(normed.sec$.INX_Close, xlab = "3Y", ylab = "Normalized Prices", col = "darkblue", 
     type = "l", lwd = 1.5)
lines(normed.sec$XLF_Close, type = "l", col = "darkgray", lwd = 1.5)
legend("topleft", legend=c("INX", "XLF"),
       col=c("darkblue","darkgray"), lty=c(1,1), lwd=c(1.5,1.5), cex=0.8)
plot(normed.sec$.INX_Close - normed.sec$XLF_Close, xlab = "3Y", ylab = "Normalized Price Differences",
     col = "darkblue", type = "l")
# we can see that INX seems to outperform XLF


lm.fit <- lm(normed.sec$.INX_Close ~ normed.sec$XLF_Close, data = normed.sec)
beta <- lm.fit$coefficients[2]
alpha <- lm.fit$coefficients[1]
par(mfrow=c(1,2))
plot(lm.fit$residuals, type = "l")
plot(normed.sec$.INX_Close - (beta * normed.sec$XLF_Close) - alpha, type = "l")
# although intercept term doesn't create too big of a difference in the residuals,
# we will include it

# testing for autocorrelation
par(mfrow=c(1,2))
acf(sec.rets$INX, type = "correlation", plot = TRUE, main = "INX Returns")
acf(sec.rets$XLF, type = "correlation", plot = TRUE, main = "XLF Returns")

# rets sometimes assumed I(0), but we will not conclude
INX.first <- diff(sec.rets$INX, lag = 1, differences = 1)
XLF.first <- diff(sec.rets$XLF, lag = 1, differences = 1)

adf.INX <- adf.test(INX.first, alternative = "stationary", k = 0)
adf.XLF <- adf.test(XLF.first, alternative = "stationary", k = 0)
adf.INX # DFt = -51.033
adf.XLF # DFt = -52.165

lm.total <- lm(INX.first ~ XLF.first, data = data.frame(INX.first, XLF.first))
resids <- lm.total$residuals * 100 # converting to pct

###########################################
### COLLECTED RESIDUALS FROM REGRESSION ###
###########################################
par(mfrow=c(1,1))
plot(resids[200:250], type = "l", xlab = "50 Indexed Days", ylab = "% Spread",
     col = "darkblue", lwd = 2, ylim = c(-2, 2))
model.sd <- sigma(lm.total)
abline(a = ((2 * model.sd)*100), b = 0, lwd = 2, col = "darkgray", lty = 2)
abline(a = ((-2 * model.sd)*100), b = 0, lwd = 2, col = "darkgray", lty = 2)
legend("topleft", legend=c("Residuals", "+/- 2 Std.Dev"),
       col=c("darkblue","darkgray"), lty=c(1,2), lwd=c(2,2), cex=0.8)

resids.ACF <- acf(resids, type = "correlation", plot = TRUE, main = "Spread(%)")





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
# explain over 85% of variance with 3 principal components
