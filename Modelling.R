install.packages("dse")
library(tseries)
df <- read.csv('Joined_Closes.csv')
date.col <- df['Date']
dropcols <- c('X','Date')
all.stocks <- df[, !(names(df) %in% dropcols)]
stock.prices <- df[1:1525, !(names(df) %in% dropcols)] # 2009-12-10 to 2015-12-31

### normalized dataframe with just stocks (will use primarily for plotting purposes)
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


### FIGURE OUT HOW TO ITERATE ENTIRE DATAFRAME
#rets <- data.frame(ts(stock.prices) / lag(ts(stock.prices), k = 1) - 1) # calculating daily pct change **will lose 1st dp**
rets.train <- data.frame(
  Delt(stock.prices$FOX, k = 1, type = "arithmetic"),
  Delt(stock.prices$FOXA, k = 1, type = "arithmetic")
                    )[2:dim(stock.prices)[1],]
names(rets.train) <- c("FOX", "FOXA")
plot(rets.train[1:100,"FOX"], type = "l", col = "darkblue")

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
test.data <- all.stocks[1526:dim(all.stocks[1]),]


###: FIGURE OUT HOW TO MERGE PCT CHANGE DF
#rets.1 <- data.frame(ts(test.data) / lag(ts(test.data), k = 1) - 1) # calculating daily pct change **will lose 1st dp**
rets.1 <- data.frame()
for (i in names(test.data)) {
  if (dim(rets.1)[1] == 1) {
    rets.1 <- data.frame(Delt(test.data$i, k = 1, type = "arithmetic"))
  } else {
    rets.1 <- cbind(rets.1, data.frame(Delt(test.data$i, k = 1, type = "arithmetic")))
  }
}

rets.stocks <- data.frame(
    Delt(test.data$FOX, k = 1, type = "arithmetic"),
    Delt(test.data$FOXA, k = 1, type = "arithmetic")
                      )[2:dim(test.data)[1],]
names(rets.stocks) <- c("FOX","FOXA")
par(mfrow=c(1,1));plot(rets.stocks[100:400,"FOX"], type = "l", col = "darkblue")

test.FOX <- diff(rets.stocks$FOX, lag = 1, differences = 1)
test.FOXA <- diff(rets.stocks$FOXA, lag = 1, differences = 1)
mean((test.FOX - predict.lm(lm.total, newdata = data.frame(test.FOX, test.FOXA)))^2)

resids.1 <- test.FOX - beta*test.FOXA - alpha
plot(resids.1, type = "l", col = "darkgray")
resids.1.ACF <- acf(resids.1, type = "correlation", plot = TRUE, main = "Spread (%)")
adf.test(resids.1, alternative = "stationary", k = 0) # Dickey Fuller Stat -44.709

mu <- mean(resids); sigma <- sd(resids); alpha <- lm.total$coefficients[1]; beta <- lm.total$coefficients[2]
trade.sig <- (resids - mu) / sigma
trade.sig.1 <- (resids.1 - mu) / sigma

###########################################
### COLLECTED RESIDUALS FROM REGRESSION ###
###########################################
par(mfrow=c(1,1))
plot(resids[100:500]*100, type = "l", xlab = "400 Indexed Days", ylab = "% Spread",
     col = "darkgray", lwd = 1, ylim = c(-2, 2))
model.sd <- sigma(lm.total)
abline(a = ((2 * model.sd)*100), b = 0, lwd = 1, col = "darkblue", lty = 2)
abline(a = ((-2 * model.sd)*100), b = 0, lwd = 1, col = "darkblue", lty = 2)
legend("topleft", legend=c("Residuals", "+/- 2.0 Std.Dev"),
       col=c("darkgray","darkblue"), lty=c(1,2), lwd=c(1,1), cex=0.8)

###: WHICH COLOR SCHEME DO YOU LIKE MORE?

par(mfrow=c(1,1))
plot(resids.1[100:500]*100, type = "l", xlab = "400 Indexed Days", ylab = "% Spread",
     col = "darkblue", lwd = 1, ylim = c(-2, 2))
model.sd <- sigma(lm.total)
abline(a = ((1.5 * model.sd)*100), b = 0, lwd = 2, col = "darkgray", lty = 2)
abline(a = ((-1.5 * model.sd)*100), b = 0, lwd = 2, col = "darkgray", lty = 2)
legend("topleft", legend=c("Residuals", "+/- 1.5 Std.Dev"),
       col=c("darkblue","darkgray"), lty=c(1,2), lwd=c(1,2), cex=0.8)

####################################
### DISTRIBUTION OF SPREAD GRAPH ###
####################################
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

cum.rets <- read.csv("Cumulative_Returns.csv")[,2:9]
names(cum.rets) <- sub("cumRets_", "z", names(cum.rets))
par(mfrow=c(1,1))
plot(cum.rets$z1.3, type = "l", col = "darkblue", ylab = "Cumulative Returns",
     xlab = "2Y (2016 & 2017)", lwd = 1.5)

### KEEP IN MIND THIS IS ENTIRE DF (in terms of dates - might need to do train/test)
### first do PCE, then convert to returns??
###: try for standardized, then regular PCA when backtesting final results
normed.df[1:5,1]
names(normed.d)
pr.out <- prcomp(normed.df, scale = TRUE, retx = TRUE)
pr.var <- pr.out$sdev^2
PVE <- pr.var / sum(pr.var)
par(mfrow=c(1,1))
barplot(PVE[1:12], xlab = "Principal Component", ylab = "Proportion of Variance Explained",
        col = "navyblue", ylim = c(0,1)) # first 20 PC
lines(cumsum(PVE[1:12]), col = "darkgray", type = "l", lwd = 2)
install.packages("Hmisc");library(Hmisc)
#minor.tick(ny=1, tick.ratio=1)
cumsum(PVE)

p.vals <- rep(NA, length(names(normed.df)))
for (tick in names(normed.df)) {
  tickr.first <- diff(normed.df[,tick], lag = 1, differences = 1)
  options(warn = -1)
  adf.tickr <- adf.test(tickr.first, alternative = "stationary", k = 0)
  p.vals[count] <- adf.tickr$statistic
  count <- count + 1
}
p.vals <- na.omit(p.vals)
failed <- rep(NA, length(names(normed.df)))
for (i in 1:length(p.vals)) {
  if (p.vals[i] > -20) {
    failed[i] <- names(normed.df)[i]
  }
}
failed

principal.comps <- pr.out$x[,1:3]
Cols <- function(vec) {
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
stocks.labs <- names(normed.df)
par(mfrow = c(1,3))
plot(c(principal.comps[,1],principal.comps[,3]), col = Cols(stocks.labs), pch=19,
     xlab = "Z1", ylab = "Z3")
plot(c(principal.comps[,2],principal.comps[,3]), col = Cols(stocks.labs), pch = 19,
     xlab = "Z2", ylab = "Z3")
plot(c(principal.comps[,1],principal.comps[,2]), col = Cols(stocks.labs), pch = 19,
     xlab = "Z1", ylab = "Z2")
# explain over 85% of variance with 3 principal components
par(mfrow=c(1,1))
plot(principal.comps[,1], col = "darkgray", ylab = "Variance from PC's", type = "l")
lines(principal.comps[,2], col = "lightskyblue", ylab = "Z1", type = "l")
lines(principal.comps[,3], col = "darkblue", ylab = "Z1", type = "l")

# explain over 85% of variance with 3 principal components

## create a line chart with 2sd matlines accross the entire time period with 
## ROLLING sd and mean. (investopedia link)
