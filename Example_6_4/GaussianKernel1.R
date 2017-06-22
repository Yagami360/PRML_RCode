#install.packages("kernlab")

# expand lib on memory
library( MASS )    # MASS package
library( kernlab ) # 
library( scatterplot3d )  # scatterplot3d関数を使用

# set options
options( digits=7 ) # 表示桁数

#########################
# set gauss kernel
#########################
#---------------------------------------------------------------
# GaussianKernel()
# [in]
#  x1 :       [vector] input data set x1-axis
#  x2 :       [vector] input data set x2-axis
#  alpha :    [scaler] constant value
#  mean :     [vector] mean vector
#  matSigma : [matrix] covariance matrix
# [out]
#  gauss :    [scaler] function value (z value)
#---------------------------------------------------------------
GaussianKernel <- function( x1, x2, alpha, mean, matSigma )
{
  datX <- cbind( x1,x2 ) #cbindで列をキープ
  mean <- matrix( rep(mean, each = nrow(datX)), ncol = ncol(datX) ) #datXと同次元の行列へ
  devX <- t( datX - mean ) #先に差を計算。ただし以降の計算で使えるよう転置。
#print(datX)
#print(mean)
#print(devX)
  gauss <- exp( -alpha*t( devX )%*% solve( matSigma ) %*% devX ) 
#print(gauss)
  return( gauss )
}

lstGKernel1 <- list(
  x1 = seq( from = -10, to = 10, by = 0.1 ),
  x2 = seq( from = -10, to = 10, by = 0.1 ),
  z = matrix( 0, nrow = 200, ncol = 200 ),
  
  dat_alpha = 0.005,
  vec_u = c( -7.61,0.22 ),
  matS = matrix( c(0.72,-0.53,-0.53,0.84), nrow = 2, ncol = 2 )
)
lstGKernel1$z = matrix( 0, nrow = length(lstGKernel1$x1), ncol = length(lstGKernel1$x2) )

lstGKernel2 <- list(
  x1 = seq( from = -10, to = 10, by = 0.1 ),
  x2 = seq( from = -10, to = 10, by = 0.1 ),
  z = matrix( 0, nrow = 200, ncol = 200 ),
  
  dat_alpha = 0.1,
  vec_u = c( 1.83,-0.73 ),
  matS = matrix( c(1.07,0.24,0.24,0.76), nrow = 2, ncol = 2 )
)
lstGKernel2$z = matrix( 0, nrow = length(lstGKernel2$x1), ncol = length(lstGKernel2$x2) )

# outer() 関数を利用し z軸の値を求める
#lstGKernel1$z <- outer(
#   lstGKernel1$x1, lstGKernel1$x2, 
#   GaussianKernel,
#   lstGKernel1$dat_alpha, lstGKernel1$vec_u ,lstGKernel1$matS
#)

# forループで処理する場合
# for( i in 1:length(lstGKernel1$x2) )
# {
#  for( j in 1:length(lstGKernel1$x1))
#  {
#      lstGKernel1$z[i][j] <- GaussianKernel( 
#        lstGKernel1$x1[j], lstGKernel1$x2[i],
#        lstGKernel1$dat_alpha, lstGKernel1$vec_u ,lstGKernel1$matS
#      )
#     
#      lstGKernel2$z[i][j] <- GaussianKernel( 
#        lstGKernel2$x1[j], lstGKernel2$x2[i],
#        lstGKernel2$dat_alpha, lstGKernel2$vec_u ,lstGKernel2$matS
#      )
#    }
#  }

# ガウス核関数のz値取得
lstGKernel1$z <- GaussianKernel( 
   lstGKernel1$x1, lstGKernel1$x2, 
   lstGKernel1$dat_alpha, lstGKernel1$vec_u ,lstGKernel1$matS
)
#print(lstGKernel1$z)

#
lstGKernel2$z <- GaussianKernel( 
  lstGKernel2$x1, lstGKernel2$x2, 
  lstGKernel2$dat_alpha, lstGKernel2$vec_u ,lstGKernel2$matS
)

# scatterplot3d用データ作成
forPlot1 <- lstGKernel1$z
dimnames( forPlot1 ) <- list( lstGKernel1$x1, lstGKernel1$x2 )
s3d_dat1 <- data.frame(
  x1 = as.vector( col(forPlot1) ),
  x2 = as.vector( row(forPlot1) ),
  value = as.vector( forPlot1 )
)

forPlot2 <- lstGKernel2$z
dimnames( forPlot2 ) <- list( lstGKernel2$x1, lstGKernel2$x2 )
s3d_dat2 <- data.frame(
  x1 = as.vector( col(forPlot2) ),
  x2 = as.vector( row(forPlot2) ),
  value = as.vector( forPlot2 )
)

############################
# set graphics parameters  #
############################
par( mfrow=c(1,1) )

# 軸に関してのデータリスト
lstAxis <- list(                        
  xMin = 0.0, xMax = 1.0,  # x軸の最小値、最大値
  yMin = 0.0, yMax = 1.0,  # y軸の最小値、最大値
   zMin = 0.0, zMax = 1.0,  # z軸の最小値、最大値
   xlim = range( c(0.0, 1.0) ), 
   ylim = range( c(0.0, 1.0) ), 
   zlim = range( c(0.0, 1.0) ),
   mainTitle = "mainTitle", # 図のメインタイトル（図の上）
   subTitle  = "subTitle",  # 図のサブタイトル（図の下）
   xlab      = "x", # x軸の名前
   ylab      = "y", # y軸の名前
   zlab      = "z"  # z軸の名前
)
lstAxis$xMin <- lstGKernel1$x1[1]
lstAxis$xMax <- lstGKernel1$x1[length(lstGKernel1$x1)]
lstAxis$yMin <- lstGKernel1$x2[1]
lstAxis$yMax <- lstGKernel1$x2[length(lstGKernel1$x2)]
lstAxis$zMin <- 0
lstAxis$zMax <- 1
lstAxis$xlim = range( c(lstAxis$xMin, lstAxis$xMax) )
lstAxis$ylim = range( c(lstAxis$yMin, lstAxis$yMax) )
lstAxis$zlim = range( c(lstAxis$zMin, lstAxis$zMax) )
lstAxis$xlab <- "x1"
lstAxis$ylab <- "x2"
lstAxis$mainTitle <- "ガウス核関数 [Gaussian kernel function]"

############################
# Draw figure              #
############################
lstAxis$subTitle <- "α=0.005, μ1=-7.61, μ2=0.02\nσ11=0.72, σ12=-0.53, σ21=-0.53, σ22=0.84"
scatterplot3d(
  s3d_dat1,
  type = 'p', 
  pch = 1,
  main = lstAxis$mainTitle,
  sub = lstAxis$subTitle,
  xlab=lstAxis$xlab, ylab=lstAxis$ylab, zlab=lstAxis$zlab,
  zlim=lstAxis$zlim,
#  xlim=lstAxis$xlim, ylim=lstAxis$ylim, zlim=lstAxis$zlim,
#  x.ticklabs = colnames(forPlot), y.ticklabs = rownames(forPlot),
  highlight.3d = TRUE
)

lstAxis$subTitle <- "α=0.1, μ1=1.83, μ2=-0.73\nσ11=1.07, σ12=0.24, σ21=0.24, σ22=0.76"
scatterplot3d(
  s3d_dat2,
  type = 'p', 
  pch =1,
  main = lstAxis$mainTitle,
  sub = lstAxis$subTitle,
  xlab=lstAxis$xlab, ylab=lstAxis$ylab, zlab=lstAxis$zlab,
  zlim=lstAxis$zlim,
#  x.ticklabs=lstAxis$xlim, ylim=lstAxis$ylim, zlim=lstAxis$zlim,
#  x.ticklabs = colnames(forPlot), y.ticklabs = rownames(forPlot),
  grid = TRUE,
  highlight.3d = TRUE
)

persp(
   lstGKernel1$x1, lstGKernel1$x2, lstGKernel1$z,
   theta = 20, phi = 20, expand = 0.5,
   main = lstAxis$mainTitle,
   xlab=lstAxis$xlab, ylab=lstAxis$ylab, zlab=lstAxis$zlab,
   ticktype = "detailed",
   col = "lightblue"
)

persp(
  lstGKernel2$x1, lstGKernel2$x2, lstGKernel2$z,
  theta = 20, phi = 20, expand = 0.5,
  main = lstAxis$mainTitle,
  xlab=lstAxis$xlab, ylab=lstAxis$ylab, zlab=lstAxis$zlab,
  ticktype = "detailed",
  col = "lightblue"
)