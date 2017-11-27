df <- read.csv('SP500JC.csv')
attach(df)
df <- na.omit(df)
dropcols <- c('X', 'X30m')
stocks <- df[, !(names(df) %in% dropcols)] 
  
pr.out <- prcomp(stocks, scale = TRUE)
biplot(pr.out, scale = 0)
pr.var <- pr.out$sdev^2
PVE <- pr.var / sum(pr.var)
par(mfrow=c(1,2))
plot(PVE, xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     ylim = c(0,1), type = "b")
plot(cumsum(PVE), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type = "b")
1 - PVE[3] # 3 principal components account for 95% of explained variance
