############################
# 正規分布関数の描写         #
############################
xMin <- -20
xMax <- 20
yMin <- 0
yMax <- 0.25
dat <- seq( from=-20.0, to=20.0, by=0.01 )
numRam <- 25  # 乱数の数

dfDNorm1 <- data.frame( x=dat, y=dnorm( x=dat, mean=0.0, sd=2.0 ) )
dfRNorm1 <- data.frame( x=rnorm( n=numRam, mean=0, sd=2 ), y=0.0  )   # 正規分布に基づく乱数生成
dfRNorm1$y <- dnorm( x=dfRNorm1$x, mean=0.0, sd=2.0 )

dfDNorm2 <- data.frame( x=dat, y=dnorm( x=dat, mean=10.0, sd=2.0 ) )
dfRNorm2 <- data.frame( x=rnorm( n=numRam, mean=10, sd=2 ), y=0.0  )   # 正規分布に基づく乱数生成
dfRNorm2$y <- dnorm( x=dfRNorm2$x, mean=10.0, sd=2.0 )

dfDNorm3 <- data.frame( x=dat, y=dnorm( x=dat, mean=0.0, sd=4.0 ) )
dfRNorm3 <- data.frame( x=rnorm( n=numRam, mean=0, sd=4 ), y=0.0  )   # 正規分布に基づく乱数生成
dfRNorm3$y <- dnorm( x=dfRNorm3$x, mean=0.0, sd=4.0 )

dfDNorm4 <- data.frame( x=dat, y=dnorm( x=dat, mean=0.0, sd=8.0 ) )
dfRNorm4 <- data.frame( x=rnorm( n=numRam, mean=0, sd=8 ), y=0.0  )   # 正規分布に基づく乱数生成
dfRNorm4$y <- dnorm( x=dfRNorm4$x, mean=0.0, sd=8.0 )

# set graphics parameters
par( mfrow=c(2,2) )   # 2*2画面表示

title1 <- "μ=0.0, σ=2.0"
title2 <- "μ=10.0, σ=2.0"
title3 <- "μ=0.0, σ=4.0"
title4 <- "μ=0.0, σ=8.0"
xlab <- "x"
ylab <- "確率 [probability]"
xlim <- range( c(xMin,xMax) )
ylim <- range( c(yMin,yMax) )

# plot 1-1
plot( 
      dfDNorm1,
      main = title1,
      xlab = xlab, ylab = ylab,
      xlim = xlim, ylim = ylim,
      type = "l"      
)
grid()  # 図にグリッド線を追加
par(new=T)
plot(
  dfRNorm1,
  main = title1,
  xlab = xlab, ylab = ylab,
  xlim = xlim, ylim = ylim,
  type = "p"
)

# plot 1-2
plot( 
  dfDNorm2,
  main = title2,
  xlab = xlab, ylab = ylab,
  xlim = xlim, ylim = ylim,
  type = "l"      
)
grid()  # 図にグリッド線を追加
par(new=T)
plot(
  dfRNorm2,
  main = title2,
  xlab = xlab, ylab = ylab,
  xlim = xlim, ylim = ylim,
  type = "p"
)

# plot 2-1
plot( 
  dfDNorm3,
  main = title3,
  xlab = xlab, ylab = ylab,
  xlim = xlim, ylim = ylim,
  type = "l"      
)
grid()  # 図にグリッド線を追加
par(new=T)
plot(
  dfRNorm3,
  main = title3,
  xlab = xlab, ylab = ylab,
  xlim = xlim, ylim = ylim,
  type = "p"
)

# plot 2-2
plot( 
  dfDNorm4,
  main = title4,
  xlab = xlab, ylab = ylab,
  xlim = xlim, ylim = ylim,
  type = "l"      
)
grid()  # 図にグリッド線を追加
par(new=T)
plot(
  dfRNorm4,
  main = title4,
  xlab = xlab, ylab = ylab,
  xlim = xlim, ylim = ylim,
  type = "p"
)
