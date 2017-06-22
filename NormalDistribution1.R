xMin <- -20
xMax <- 20
yMin <- 0
yMax <- 0.25
dat <- seq( from=-20.0, to=20.0, by=0.01 )
numRam <- 25  # �����̐�

dfDNorm1 <- data.frame( x=dat, y=dnorm( x=dat, mean=0.0, sd=2.0 ) )
dfRNorm1 <- data.frame( x=rnorm( n=numRam, mean=0, sd=2 ), y=0.0  )   # ���K���z�Ɋ�Â���������
dfRNorm1$y <- dnorm( x=dfRNorm1$x, mean=0.0, sd=2.0 )

dfDNorm2 <- data.frame( x=dat, y=dnorm( x=dat, mean=10.0, sd=2.0 ) )
dfRNorm2 <- data.frame( x=rnorm( n=numRam, mean=10, sd=2 ), y=0.0  )   # ���K���z�Ɋ�Â���������
dfRNorm2$y <- dnorm( x=dfRNorm2$x, mean=10.0, sd=2.0 )

dfDNorm3 <- data.frame( x=dat, y=dnorm( x=dat, mean=0.0, sd=4.0 ) )
dfRNorm3 <- data.frame( x=rnorm( n=numRam, mean=0, sd=4 ), y=0.0  )   # ���K���z�Ɋ�Â���������
dfRNorm3$y <- dnorm( x=dfRNorm3$x, mean=0.0, sd=4.0 )

dfDNorm4 <- data.frame( x=dat, y=dnorm( x=dat, mean=0.0, sd=8.0 ) )
dfRNorm4 <- data.frame( x=rnorm( n=numRam, mean=0, sd=8 ), y=0.0  )   # ���K���z�Ɋ�Â���������
dfRNorm4$y <- dnorm( x=dfRNorm4$x, mean=0.0, sd=8.0 )

# set graphics parameters
par( mfrow=c(2,2) )   # 2*2��ʕ\��

title1 <- "��=0.0, ��=2.0"
title2 <- "��=10.0, ��=2.0"
title3 <- "��=0.0, ��=4.0"
title4 <- "��=0.0, ��=8.0"
xlab <- "x"
ylab <- "�m�� [probability]"
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
grid()  # �}�ɃO���b�h����ǉ�
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
grid()  # �}�ɃO���b�h����ǉ�
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
grid()  # �}�ɃO���b�h����ǉ�
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
grid()  # �}�ɃO���b�h����ǉ�
par(new=T)
plot(
  dfRNorm4,
  main = title4,
  xlab = xlab, ylab = ylab,
  xlim = xlim, ylim = ylim,
  type = "p"
)