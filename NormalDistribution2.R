#options( repos="http://cran.ism.ac.jp" )
#install.packages( 'mvtnorm' )
#install.packages( 'scatterplot3d' )

library(mvtnorm)                             # 多変量正規分布を扱う
library(scatterplot3d)                       # scatterplot3d関数を使用

dfAxis <- data.frame(                        # 軸に関してのデータ
  xMin = -5.0, xMax = 5.0,                   # x軸の最小値、最大値
  yMin = -5.0, yMax = 5.0                    # y軸の最小値、最大値
)

datX1 <- seq( from=-3.0, to=5.0, by=0.20 )   # x1軸の値ベクトル
datX2 <- seq( from=-3.0, to=5.0, by=0.20 )   # x2軸の値ベクトル
datU <- c( 2.0, 1.0 )                        # 平均値ベクトル
matS <- matrix(                              # 共分散分散行列
  data = c( 0.7, 0.5, 0.5, 2.0 ),
  nrow = 2, ncol = 2 
)

############################
# set desitiny functions   #
############################
funcNormDim2 <- function( x1, x2 ) 
{ 
  dmvnorm(
    matrix( c(x1,x2), ncol=2 ), 
    mean=datU, sigma=matS
  ) 
}

dfNorm <- data.frame( x1=datX1, x2=datX2, z=0 )         # データフレームにまとめる
dfNorm$z <- outer(dfNorm$x1, dfNorm$x2, funcNormDim2)   # x1とx2の外積で縦軸成分を求める

numRam <- 1000                                         # 乱数の数
datRNorm <- rmvnorm( n=numRam, mean=datU, sigma=matS )  # 2次元の正規分布に基づく乱数生成
dfRNorm <- data.frame(
  x1 = datRNorm[,1],
  x2 = datRNorm[,2],
  z =  dmvnorm( x = datRNorm, mean = datU, sigma = matS)
)

############################
# set graphics parameters  #
############################
par( mfrow=c(1,1) )

title <- "正規分布関数（２変数）"
xlim <- range( c(dfAxis$xMin, dfAxis$xMax) )
ylim <- range( c(dfAxis$yMin, dfAxis$yMax) )
xlab <- "x1"
ylab <- "x2"
zlab <- "probability"

############################
# Draw in dim3 figure      #
############################
#win.graph() # 別ののグラフィックウインドウに作図
persp(
  x = dfNorm$x1, y = dfNorm$x2, z = dfNorm$z, 
  theta = 20, phi = 20, expand = 0.5,
  main = title,
  xlab = xlab, ylab = ylab, zlab = zlab,
  ticktype = "detailed",
  col = "lightblue"
)

#win.graph() # 別ののグラフィックウインドウに作図
scatterplot3d(
  x = dfRNorm$x1, y = dfRNorm$x2, z = dfRNorm$z,
  main = title,
  xlab = xlab, ylab = ylab, zlab = zlab,
  highlight = TRUE
)


############################
# Draw in dim2 figure      #
############################
# 乱数のplot
#plot( 
#  dfRNorm,
#  main = title,
#  xlab = xlab, ylab = ylab,
#  xlim = xlim, ylim = ylim,
#  type = "p"      
#)
#grid()

# 中央線（平均値）の直線追加
#abline( v = datU[1], h = datU[2], lty = "dotdash" )

# 等高線のplot追加
#par(new=T)
#plot(
#  dfNorm$x1[500:1000], dfNorm$x2[500:1000],
#  main = title,
#  xlab = xlab, ylab = ylab,
#  xlim = xlim, ylim = ylim,
#  pch = 3,
#  type = "p"      
#)
#contour(
#  x = dfNorm$x1,
#  y = dfNorm$x2,
#  z = z,  # 行列に変換
#  nlevels = 1
#)