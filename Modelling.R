setwd("C:/Users/PeterKokalov/lpthw/Pairs-Model-FOX-FOXA")
library(tseries);library(quantmod)
df <- read.csv('Joined_Closes.csv')
date.col <- df['Date']
dropcols <- c('X','Date')
all.stocks <- df[, !(names(df) %in% dropcols)]
stock.prices <- df[1:1525, !(names(df) %in% dropcols)] # 2009-12-10 to 2015-12-31

### normalized dataframe with just stocks (used for plotting purposes)
normed.df <- stock.prices # copy of stocks.df for normalized prices
for (i in names(stock.prices)) {
  eq <- stock.prices[,i]
  normed.df[, i] <- (eq - min(eq)) / (max(eq) - min(eq))
}
par(mfrow=c(1,1))
plot(stock.prices[,'FOX'], type = "l", col = "darkblue", ylim = c(min(stock.prices[,'FOXA']),40),
     xlab = "2016-2017", ylab = "Closing Prices USD")
lines(stock.prices[,'FOXA'], type = "l", col = "darkgray")
legend("topleft", legend=c("FOX", "FOXA"),
       col=c("darkblue","darkgray"), lty=c(1,1), lwd=c(1.5,1.5), cex=0.8)

#rets <- data.frame(ts(stock.prices) / lag(ts(stock.prices), k = 1) - 1) # calculating daily pct change **will lose 1st dp**
rets.train <- data.frame(
  Delt(stock.prices$FOX, k = 1, type = "arithmetic"),
  Delt(stock.prices$FOXA, k = 1, type = "arithmetic")
)[2:dim(stock.prices)[1],]
names(rets.train) <- c("FOX", "FOXA")
par(mfrow=c(2,1))
plot(rets.train[1:100,"FOX"], type = "l", col = "darkblue", ylab = "FOX Returns", xlab = "write own (2009-12-10)")
plot(rets.train[1:100,"FOXA"], type = "l", col = "darkblue", ylab = "FOXA Returns", xlab = "write own (2009-12-10)")


##########################
### FOX vs. FOXA Plots ###
##########################
par(mfrow=c(1,2))
plot(normed.df$FOX + 0.2, xlab = "8Y", ylab = "Normalized Prices", col = "darkblue", 
     type = "l", lwd = 1.5, ylim = c(0,1.2))
lines(normed.df$FOXA, type = "l", col = "darkgray", lwd = 1.5)
legend("topleft", legend=c("FOX", "FOXA + 0.2"),
       col=c("darkblue","darkgray"), lty=c(1,1), lwd=c(1.5,1.5), cex=0.8)
plot(normed.df$FOX - normed.df$FOXA, xlab = "8Y", ylab = "Normalized Price Differences",
     col = "darkblue", type = "l")
# we can see that neither FOX nor FOXA seems to outperform

lm.fit <- lm(normed.df$FOX ~ normed.df$FOXA, data = normed.df)
beta <- lm.fit$coefficients[2] # 1.0108
alpha <- lm.fit$coefficients[1] # 0.00216
par(mfrow=c(1,2))
plot(lm.fit$residuals, type = "l")
plot(normed.df$FOX - (beta * normed.df$FOXA) - alpha, type = "l")
# although intercept term doesn't create too big of a difference in the residuals,
# we will include it

# testing for autocorrelation
par(mfrow=c(1,2))
acf(rets.train$FOX, type = "correlation", plot = TRUE, main = "FOX Returns")
acf(rets.train$FOXA, type = "correlation", plot = TRUE, main = "FOXA Returns")

# returns sometimes assumed I(0), but we will not conclude
FOX.first <- diff(rets.train$FOX, lag = 1, differences = 1)
FOXA.first <- diff(rets.train$FOXA, lag = 1, differences = 1)

adf.test(FOX.first, alternative = "stationary", k = 0) # Dickey Fuller Statistic -70.178
adf.test(FOXA.first, alternative = "stationary", k = 0) # Dickey Fuller Statistic -70

### final regression run on DIFFERENCED FOX AND DIFFERENCED FOXA
lm.total <- lm(FOX.first ~ FOXA.first, data = data.frame(FOX.first, FOXA.first))
resids <- lm.total$residuals

###:TESTING
test.data <- all.stocks[1526:dim(all.stocks[1]),]
rets.stocks <- data.frame(
  Delt(test.data$FOX, k = 1, type = "arithmetic"),
  Delt(test.data$FOXA, k = 1, type = "arithmetic")
)[2:dim(test.data)[1],]
names(rets.stocks) <- c("FOX","FOXA")
par(mfrow=c(2,1))
plot(rets.stocks[1:400,"FOX"], type = "l", col = "darkblue", ylab = "FOX Returns", xlab = "Make own (2016-04-01)")
plot(rets.stocks[1:400,"FOXA"], type = "l", col = "darkblue", ylab = "FOXA Returns")

test.FOX <- diff(rets.stocks$FOX, lag = 1, differences = 1)
test.FOXA <- diff(rets.stocks$FOXA, lag = 1, differences = 1)
mean((test.FOX - predict.lm(lm.total, newdata = data.frame(test.FOX, test.FOXA)))^2)

resids.1 <- test.FOX - beta*test.FOXA - alpha
par(mfrow=c(1,1))
plot(resids.1, type = "l", col = "darkgray", ylab = "Residuals")
resids.1.ACF <- acf(resids.1, type = "correlation", plot = TRUE, main = "Spread (%)")
adf.test(resids.1, alternative = "stationary", k = 0) # Dickey Fuller Stat -44.709

mu <- mean(resids); sigma <- sd(resids); alpha <- lm.total$coefficients[1]; beta <- lm.total$coefficients[2]
trade.sig <- (resids - mu) / sigma
trade.sig.1 <- (resids.1 - mu) / sigma

###########################################
### COLLECTED RESIDUALS FROM REGRESSION ###
###########################################
par(mfrow=c(1,2))
plot(resids[1:100]*100, type = "l", xlab = "400 Indexed Days", ylab = "% Spread",
     col = "darkgray", lwd = 1, ylim = c(-2, 2))
model.sd <- sigma(lm.total)
abline(a = ((2 * model.sd)*100), b = 0, lwd = 1, col = "darkblue", lty = 2)
abline(a = ((-2 * model.sd)*100), b = 0, lwd = 1, col = "darkblue", lty = 2)
legend("topleft", legend=c("Residuals", "+/- 2.0 Std.Dev"),
       col=c("darkgray","darkblue"), lty=c(1,2), lwd=c(1,1), cex=0.8)

par(mfrow=c(1,2))
for (indx in c(50,200)) {
  plot(resids.1[1:indx]*100, type = "l", xlab = sprintf("%i Indexed Days", indx), ylab = "% Spread",
       col = "darkgray", lwd = 1, ylim = c(-2, 2))
  model.sd <- sigma(lm.total)
  abline(a = ((1.5 * model.sd)*100), b = 0, lwd = 1, col = "darkblue", lty = 2)
  abline(a = ((-1.5 * model.sd)*100), b = 0, lwd = 1, col = "darkblue", lty = 2)
  legend("topleft", legend=c("Residuals", "+/- 1.5 Std.Dev"),
         col=c("darkgray","darkblue"), lty=c(1,2), lwd=c(1,1), cex=0.8)
}

####################################
### DISTRIBUTION OF SPREAD GRAPH ###
####################################
par(mfrow=c(1,1))
hist(trade.sig, breaks=100, col="darkblue", main = "", xlab = "Std.Dev")

test.data$FOX # X1
test.data$FOXA # Y1

plot(resids.1[100:150]*100, type = "l", xlab = "50 Indexed Days", ylab = "% Spread",
     col = "darkblue", lwd = 2, ylim = c(-2, 2.5))
abline(a = ((1.5 * model.sd)*100), b = 0, lwd = 2, col = "darkgray", lty = 2)
abline(a = ((-1.5 * model.sd)*100), b = 0, lwd = 2, col = "darkgray", lty = 2)
legend("topleft", legend=c("Residuals", "+/- 1.5 Std.Dev"),
       col=c("darkblue","darkgray"), lty=c(1,2), lwd=c(2,2), cex=0.8)
dates.test <- date.col[1526:dim(all.stocks)[1],]
output <- data.frame(
  data.frame(test.data$FOX, test.data$FOXA)[3:(dim(test.data)[1]),],
  test.FOX, test.FOXA,
  trade.sig.1, rep(beta, length(trade.sig.1)),
  data.frame(test.data$FOX - test.data$FOXA*beta - alpha)[3:dim(test.data)[1],],
  dates.test[3:length(dates.test)]
)
#names(output)[1:2] <- sub("teststock.prices.", "", names(output)[1:2])
names(output)[1:2] <- sub("test.data.", "", names(output)[1:2])
names(output)[3:4] <- sub("test.", "diff", names(output[3:4]))
names(output)[5] <- sub(".1", "", names(output[5]))
names(output)[6] <- sub("rep.beta..length.trade.sig.1..", "beta", names(output[6]))
names(output)[7] <- sub("data.frame.test.data.FOX...test.data.FOXA...beta...alpha..3.dim.test.data..1...",
                        "hedge", names(output[7]))
names(output)[8] <- sub("dates.test.3.length.dates.test..", "Date", names(output[8]))
write.csv(output, "signals1.csv")

par(mfrow=c(1,2))
plot(output$trade.sig[1:10], type = "l")
for (i in 1:10) {
  abline(v=i, col = "red", lty = 2)
}
plot(test.FOX[1:10], type = "l", col = "darkblue", ylim = c(-0.07, 0.07))
lines(test.FOXA[1:10], type = "l", col = "darkgray")
abline(a = 0, b = 0, col = "red", lwd = 2, lty = 2)

cum.rets <- read.csv("Cumulative_Returns.csv")
cum.rets <- cum.rets[2:dim(cum.rets)[1], 3:dim(cum.rets)[2]]
names(cum.rets)[1] <- sub("cumRets_1.0", "CumRet", names(cum.rets)[1])
par(mfrow=c(1,1))
plot(cum.rets[2:249,]$CumRet, type = "l", col = "darkblue", ylab = "Cumulative Returns",
     xlab = "2016 Backtest", lwd = 1.5)
returns <- (cum.rets$CumRet[249] - cum.rets$CumRet[1]) * 100
returns # 5.028% 2016 returns. Profitable!
