setwd("C:/Users/PeterKokalov/lpthw/Pairs-Model-FOX-FOXA")
library(tseries);library(quantmod)
df <- read.csv('Joined_Closes.csv')
length(df$Date)
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
plot(test.data[,'FOX'], type = "l", col = "darkblue", ylim = c(min(test.data[,'FOXA']),35),
     xlab = "2016-2017", ylab = "Closing Prices USD")
lines(test.data[,'FOXA'], type = "l", col = "darkgray")
legend("topleft", legend=c("FOX", "FOXA"),
       col=c("darkblue","darkgray"), lty=c(1,1), lwd=c(1.5,1.5), cex=0.8)

#rets <- data.frame(ts(stock.prices) / lag(ts(stock.prices), k = 1) - 1) # calculating daily pct change **will lose 1st dp**
rets.train <- data.frame(
  Delt(stock.prices$FOX, k = 1, type = "arithmetic"),
  Delt(stock.prices$FOXA, k = 1, type = "arithmetic")
)[2:dim(stock.prices)[1],]
names(rets.train) <- c("FOX", "FOXA")
par(mfrow=c(2,1))
plot(rets.train[1:100,"FOX"], type = "l", col = "darkblue", ylab = "FOX Returns", xlab = "")
plot(rets.train[1:100,"FOXA"], type = "l", col = "darkblue", ylab = "FOXA Returns", xlab = "100 Indexed Days")


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
mu <- mean(resids); sigma <- sd(resids); alpha <- lm.total$coefficients[1]; beta <- lm.total$coefficients[2]

###:TESTING
test.data <- all.stocks[1526:dim(all.stocks[1]),]
rets.stocks <- data.frame(
  Delt(test.data$FOX, k = 1, type = "arithmetic"),
  Delt(test.data$FOXA, k = 1, type = "arithmetic")
)[2:dim(test.data)[1],]
names(rets.stocks) <- c("FOX","FOXA")
par(mfrow=c(2,1))
plot(rets.stocks[1:400,"FOX"], type = "l", col = "darkblue", ylab = "FOX Returns", xlab = "")
plot(rets.stocks[1:400,"FOXA"], type = "l", col = "darkblue", ylab = "FOXA Returns", xlab = "400 Indexed Days")

test.FOX <- diff(rets.stocks$FOX, lag = 1, differences = 1)
test.FOXA <- diff(rets.stocks$FOXA, lag = 1, differences = 1)
mean((test.FOX - predict.lm(lm.total, newdata = data.frame(test.FOX, test.FOXA)))^2)

####################################
### RESIDUALS AND TRADING SIGNAL ###
####################################
resids.1 <- test.FOX - beta*test.FOXA - alpha
trade.sig.1 <- (resids.1 - mu) / sigma
par(mfrow=c(1,1))
plot(resids.1, type = "l", col = "darkgray", ylab = "Residuals")
resids.1.ACF <- acf(resids.1, type = "correlation", plot = TRUE, main = "Spread (%)")
adf.test(resids.1, alternative = "stationary", k = 0) # Dickey Fuller Stat -44.709


###########################################
### COLLECTED RESIDUALS FROM REGRESSION ###
###########################################
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
hist(trade.sig.1, breaks=50, col="darkblue", main = "", xlab = "Std.Dev")

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

FOX.first.add <- data.frame(FOX.first);names(FOX.first.add) <- "FOX"
FOX.first.add2 <- data.frame(output$diffFOX);names(FOX.first.add2) <- "FOX"
FOXA.first.add <- data.frame(FOXA.first);names(FOXA.first.add) <- "FOXA"
FOXA.first.add2 <- data.frame(output$diffFOXA);names(FOXA.first.add2) <- "FOXA"
update.beta <- data.frame(rbind(FOX.first.add, FOX.first.add2),rbind(FOXA.first.add, FOXA.first.add2) )

beta.list <- rep(0, length(output$beta))
cur.beta <- beta
look.back <- 1:length(beta.list)
periods <- look.back[seq(1, length(look.back), 25)]
for (i in 1:length(beta.list)) {
  if (i %in% periods) {
    beta.update <- lm(update.beta$FOX[1:1525+i] ~ update.beta$FOXA[1:1525+i], data = update.beta)$coefficients[2]
    cur.beta <- beta.update
  }
  beta.list[i] <- cur.beta
}
plot(beta.list, type = "l", col = "darkblue", ylab = "Value of Beta", xlab = "Backtest Period")

resids.add1 <- data.frame(resids);names(resids.add1) <- "resids"
resids.add2 <- data.frame(resids.1);names(resids.add2) <- "resids"
update.musd <- rbind(resids.add1, resids.add2)
mean.list <- rep(0, dim(output)[1])
cur.mean <- mu
for (i in 1:length(mean.list)) {
  mean.list[i] <- rollapplyr(update.musd$resids, width = 1:1525 + i, mean, fill = 0)[1525+i]
}
sd.list <- rep(0, dim(output)[1])
cur.sd <- sigma
for (i in 1:length(sd.list)) {
  sd.list[i] <- rollapplyr(update.musd$resids, width = 1:1525 + i, sd, fill = 0)[1525+i]
}
sd.list[489] <- sd.list[488];sd.list[490] <- sd.list[489]
mean.list[489] <- mean.list[488];mean.list[490] <- mean.list[489]
trade.sig.update <- (resids.1 - mean.list) / sd.list
# less profitable to update sd and mean
sigma

par(mfrow=c(1,1))
plot(resids.1[1:50], xlim = c(3,50), type = "l", col = "darkgray", xlab = "50 Indexed Days During Backtest", ylab = "Residuals")
for (i in c(1, -1)) {
  lines(rep(i * sigma, length(resids.1)), col = "darkblue", lty = 2)  
}
lines(rep(mu, length(resids.1)), col = "red", lty = 2)
legend("topright", legend=c("Residuals", "+/- 1.0 Std.Dev", "Mean = 0.0"),
       col=c("darkgray","darkblue", "red"), lty=c(1,2,2), lwd=c(1,1), cex=0.8)

output$beta <- beta.list
output$hedge <- data.frame(test.data$FOX - test.data$FOXA*output$beta)[3:dim(test.data)[1],]
write.csv(output, "signals1.csv")

par(mfrow=c(1,2))
plot(output$trade.sig[1:10], type = "l")
for (i in 1:10) {
  abline(v=i, col = "red", lty = 2)
}
plot(test.FOX[1:10], type = "l", col = "darkblue", ylim = c(-0.07, 0.07))
lines(test.FOXA[1:10], type = "l", col = "darkgray")
abline(a = 0, b = 0, col = "red", lwd = 2, lty = 2)

dim(df)
(test.data$FOX[492] - test.data$FOX[1]) / test.data$FOX[492]
test.data$FOX[492]
plot(df$FOX, type = "l")
dim(test.data)
plot(test.data$FOX, type = "l")

cum.rets <- read.csv("Cumulative_Returns.csv")
par(mfrow=c(1,1))
dim(cum.rets)
cum.rets
plot(cum.rets[,3], type = "l", col = "darkblue", ylab = "Pairs Returns", xlab = "2-Year Backtest")
plot(output$FOX, type = "l", col = "red")
plot(cum.rets[,1], type = "l", col = "darkblue", ylab = "Returns", xlab = "2Y")
plot(output$FOX, type = "l", col = "darkblue", ylab = "FOX Price", xlab = "2Y")
returns <- (cum.rets$cumRets_1.0[489] - cum.rets$cumRets_1.0[2]) * 100
returns # 14.28%. Profitable!
sharpe(na.remove(cum.rets[,3]), r = 0) # 2.547
maxdrawdown(na.remove(cum.rets[,3]))$maxdrawdown * 100 # 1.20%
