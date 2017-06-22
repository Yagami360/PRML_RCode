#############################
# density function          #
#############################
xMin <- 0.0 
xMax <- 10.0
yMin <- 0.0
yMax <- 1.5
datX <- seq( from=xMin, to=xMax, by=0.01 )

dfDNorm1 <- data.frame( x=datX, y=dnorm( x=datX, mean=2.0, sd=0.3 ) )
dfDNorm2 <- data.frame( x=datX, y=dnorm( x=datX, mean=3.0, sd=0.8 ) )


#############################
# ROC                       #
#############################
xMin <- 0.0 
xMax <- 1.0
yMin <- 0.0
yMax <- 1.01
dat <- seq( from=0.0, to=10.0, by=0.01 )

dfROC <- data.frame( 
  sigma1 = pnorm( q=dat, mean=2.0, sd=0.3, lower.tail=TRUE ),
  sigma2 = pnorm( q=dat, mean=3.0, sd=0.8, lower.tail=TRUE )
)

# set graphics parameters
titleROC <- "ROC曲線 [ROC Curve]"
xlab <- "偽陽性率 [false positive rate]"
ylab <- "真陽性率 [true positive rate]"
xlim <- range( c(xMin,xMax) )
ylim <- range( c(yMin,yMax) )

# plot ROC Curve
plot( dfROC$sigma2, dfROC$sigma1,
      main = titleROC,
      xlab = xlab, ylab = ylab,
      xlim = xlim, ylim = ylim,
      type = "l"      
)
grid()  # 図にグリッド線を追加

#############################
# Loss lines                #
#############################
dfLossLine1 <- data.frame( dat1=c(0.0,0.75),dat2=c(0.20,1.050) ) # 等損失直線１（データフレーム）
dfLossLine2 <- data.frame( dat1=c(0.0,0.65),dat2=c(0.40,1.120) ) # 等損失直線２（データフレーム）
dfLossLine3 <- data.frame( dat1=c(0.0,0.40),dat2=c(0.65,1.120) ) # 等損失直線３（データフレーム）
dfLossLine4 <- data.frame( dat1=c(0.0,0.25),dat2=c(0.80,1.100) ) # 等損失直線４（データフレーム）

# plot Loss lines
par(new=T)
plot( dfLossLine1$dat1, dfLossLine1$dat2,
      xlab = xlab, ylab = ylab,
      xlim = xlim, ylim = ylim,
      type = "l",
      lty = "dotdash"
)

par(new=T)
plot( dfLossLine2$dat1, dfLossLine2$dat2,
      xlab = xlab, ylab = ylab,
      xlim = xlim, ylim = ylim,
      type = "l",
      lty = "dotdash" 
)

par(new=T)
plot( dfLossLine3$dat1, dfLossLine3$dat2,
      xlab = xlab, ylab = ylab,
      xlim = xlim, ylim = ylim,
      type = "l",
      lty = "dotdash",
      col = "red"
)

par(new=T)
plot( dfLossLine4$dat1, dfLossLine4$dat2,
      xlab = xlab, ylab = ylab,
      xlim = xlim, ylim = ylim,
      type = "l",
      lty = "dotdash"      
)
