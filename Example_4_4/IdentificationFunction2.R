library( MASS )     # MASS package（ピマ・インディアンPima のデータを使用）

# set options
options( digits=5 ) # 表示桁数

# Pima data expand on memory
data( Pima.tr )
data( Pima.te )

# copy data from Pima data
lstPimaTrain <- list(
  numNoDiabetes = 0,       # 糖尿病未発症人数（0で初期化）
  numDiabetes = 0,         # 糖尿病発症人数（0で初期化）
  glu = Pima.tr$glu, 
  bmi = Pima.tr$bmi, 
  bResult = rep(FALSE, length(Pima.tr$glu)) # 糖尿病か否か？（FALSE:糖尿病でない、TRUE:糖尿病）
)

# Pima.tr$type のデータ（Yes,No）を符号化[encoding]（※簡単のためforループ使用）
for ( i in 1:length(Pima.tr$type) ) 
{
  if(Pima.tr$type[i] == "Yes" )
  {
    lstPimaTrain$numDiabetes <- (lstPimaTrain$numDiabetes + 1)
    lstPimaTrain$bResult[i] <- TRUE
  }
  else if( Pima.tr$type[i] == "No" )
  {
    lstPimaTrain$numNoDiabetes <- (lstPimaTrain$numNoDiabetes + 1)
    lstPimaTrain$bResult[i] <- FALSE
  }
  else{
    # Do Nothing
  }
}

# sort Pima data
lstPimaTrain$glu <- lstPimaTrain$glu[ order(lstPimaTrain$bResult) ]
lstPimaTrain$bmi <- lstPimaTrain$bmi[ order(lstPimaTrain$bResult) ]
lstPimaTrain$bResult <- lstPimaTrain$bResult[ order(lstPimaTrain$bResult) ]

# split data to class C1 and C2
dfPimaTrain_C1 <- data.frame(
  glu = lstPimaTrain$glu[1:lstPimaTrain$numNoDiabetes], 
  bmi = lstPimaTrain$bmi[1:lstPimaTrain$numNoDiabetes],
  bResult = FALSE             # 糖尿病か否か？（FALSE:糖尿病でない、TRUE:糖尿病）
)
dfPimaTrain_C2 <- data.frame(
  glu = lstPimaTrain$glu[(lstPimaTrain$numNoDiabetes+1):(lstPimaTrain$numNoDiabetes+lstPimaTrain$numDiabetes)], 
  bmi = lstPimaTrain$bmi[(lstPimaTrain$numNoDiabetes+1):(lstPimaTrain$numNoDiabetes+lstPimaTrain$numDiabetes)],
  bResult = TRUE             # 糖尿病か否か？（FALSE:糖尿病でない、TRUE:糖尿病）
)

# sort class C1,C2 data
dfPimaTrain_C1$glu <- dfPimaTrain_C1$glu[ order(dfPimaTrain_C1$glu) ]
dfPimaTrain_C1$bmi <- dfPimaTrain_C1$bmi[ order(dfPimaTrain_C1$glu) ]
dfPimaTrain_C2$glu <- dfPimaTrain_C2$glu[ order(dfPimaTrain_C2$glu) ]
dfPimaTrain_C2$bmi <- dfPimaTrain_C2$bmi[ order(dfPimaTrain_C2$glu) ]

# release memory
rm(Pima.tr) 
rm(Pima.te)

#-------------------
# set class C1 data
#-------------------
datU1_C1 <- mean( dfPimaTrain_C1$glu ) # クラス１（糖尿病発症なし）の変数１（glu）の平均値
datU2_C1 <- mean( dfPimaTrain_C1$bmi ) # クラス１（糖尿病発症有り）の変数２（BMI）の平均値
datU_C1 <- matrix( c(datU1_C1,datU2_C1), nrow = 2, ncol = 1)      # クラス１（糖尿病発症なし）の平均ベクトル

matS_C1 <- matrix( c(0,0,0,0), nrow = 2, ncol = 2 ) # クラス１（糖尿病発症なし）の共分散行列
matS_C1[1,1] <- sqrt( var( dfPimaTrain_C1$glu, dfPimaTrain_C1$glu ) )
matS_C1[1,2] <- sqrt( var( dfPimaTrain_C1$glu, dfPimaTrain_C1$bmi ) )
matS_C1[2,1] <- sqrt( var( dfPimaTrain_C1$bmi, dfPimaTrain_C1$glu ) )
matS_C1[2,2] <- sqrt( var( dfPimaTrain_C1$bmi, dfPimaTrain_C1$bmi ) )

#-------------------
# set class C2 data
#-------------------
datU1_C2 <- mean( dfPimaTrain_C2$glu ) # クラス２（糖尿病発症なし）の変数１（glu）の平均値
datU2_C2 <- mean( dfPimaTrain_C2$bmi ) # クラス２（糖尿病発症有り）の変数２（BMI）の平均値
datU_C2 <- matrix( c(datU1_C2,datU2_C2), nrow = 2, ncol = 1)      # クラス１（糖尿病発症なし）の平均ベクトル

matS_C2 <- matrix( c(0,0,0,0), nrow = 2, ncol = 2 ) # クラス２（糖尿病発症なし）の共分散行列
matS_C2[1,1] <- sqrt( var( dfPimaTrain_C2$glu, dfPimaTrain_C2$glu ) )
matS_C2[1,2] <- sqrt( var( dfPimaTrain_C2$glu, dfPimaTrain_C2$bmi ) )
matS_C2[2,1] <- sqrt( var( dfPimaTrain_C2$bmi, dfPimaTrain_C2$glu ) )
matS_C2[2,2] <- sqrt( var( dfPimaTrain_C2$bmi, dfPimaTrain_C2$bmi ) )

##################################
# secondary idification function
##################################
Dim2IdificationFunc <- function( x1, x2, u_C1, u_C2, matS1, matS2, P_C1=0.5, P_C2=0.5 )
{
  datX <- matrix( 0, nrow = 2, ncol = 1  )
  datX[1,] <- x1
  datX[2,] <- x2
  matW <- matrix( c(0,0,0,0), nrow = 2, ncol = 2 )
  matW <- ( solve(matS1) - solve(matS2) )
  vct <- ( t(u_C2)%*%solve(matS2) - t(u_C1)%*%solve(matS1) )
  r <- ( t(u_C1)%*%solve(matS1)%*%u_C1 - t(u_C2)%*%solve(matS2)%*%u_C2 + log( det(matS1)/det(matS2) ) - 2*log(P_C1/P_C2) )
  
  z0 <- ( t(datX)%*%matW%*%datX )
  z1 <- ( 2*vct%*%datX )
  z <- ( z0 + z1 + r )
  return(z)
}

###############################
# liner idification function
###############################
Dim1IdificationFunc <- function( x1, x2, u_C1, u_C2, matS1, matS2, P_C1=0.5, P_C2=0.5 )
{
  datX <- matrix( 0, nrow = 2, ncol = 1  )
  datX[1,] <- x1
  datX[2,] <- x2
  
  matSp <- matrix( c(0,0,0,0), nrow = 2, ncol = 2 )
  matSp <- P_C1*matS1 + P_C2*matS2
  vct <- ( t(u_C2)%*%solve(matSp) - t(u_C1)%*%solve(matSp) )
  r <- ( t(u_C1)%*%solve(matSp)%*%u_C1 - t(u_C2)%*%solve(matSp)%*%u_C2 + log( det(matSp)/det(matSp) ) - 2*log(P_C1/P_C2) )
  
  z <- ( 2*vct%*%datX + r )
  return(z)
}

############################
# ROC Curve                #
############################
#----------------------------------------------------------------------
# 偽陽性率[false positive rate]と真陽性率[true positive rate]の算出
#----------------------------------------------------------------------
datBorder <- seq( from = -200, to = 200, by = 5 )

# secondary idification function
dfROC2 <- data.frame(
  sigma2 = matrix( 0, nrow = length(datBorder), ncol = 1 ),
  sigma1 = matrix( 0, nrow = length(datBorder), ncol = 1 )
)

matZResult2 <- matrix( 0,     nrow = (lstPimaTrain$numNoDiabetes+lstPimaTrain$numDiabetes), ncol = length(datBorder) )
matResult2  <- matrix( FALSE, nrow = (lstPimaTrain$numNoDiabetes+lstPimaTrain$numDiabetes), ncol = length(datBorder) )

for( m in 1:length(datBorder) )
{
  for( k in 1:(lstPimaTrain$numNoDiabetes+lstPimaTrain$numDiabetes) )
  {
    matZResult2[k,m] <- Dim2IdificationFunc(
      x1 = lstPimaTrain$glu[k], x2 = lstPimaTrain$bmi[k],
      u_C1 = datU_C1, u_C2 = datU_C2, matS1 = matS_C1, matS2 = matS_C2
    )
    
    if( matZResult2[k,m] >= datBorder[m] )
    {
      matResult2[k,m] <- TRUE
    }
    else
    {
      matResult2[k,m] <- FALSE
    }
    
    tblRes <- table( lstPimaTrain$bResult, matResult2[,m] )
    dfROC2$sigma2[m] <- tblRes[2,1]/(lstPimaTrain$numDiabetes)
    dfROC2$sigma1[m] <- tblRes[1,1]/(lstPimaTrain$numNoDiabetes)      
  }
}

# liner idification function
dfROC1 <- data.frame(
  sigma2 = matrix( 0, nrow = length(datBorder), ncol = 1 ),
  sigma1 = matrix( 0, nrow = length(datBorder), ncol = 1 )
)
matZResult1 <- matrix( 0,     nrow = (lstPimaTrain$numNoDiabetes+lstPimaTrain$numDiabetes), ncol = length(datBorder) )
matResult1  <- matrix( FALSE, nrow = (lstPimaTrain$numNoDiabetes+lstPimaTrain$numDiabetes), ncol = length(datBorder) )

for( m in 1:length(datBorder) )
{
  for( k in 1:(lstPimaTrain$numNoDiabetes+lstPimaTrain$numDiabetes) )
  {
    matZResult1[k,m] <- Dim2IdificationFunc(
      x1 = lstPimaTrain$glu[k], x2 = lstPimaTrain$bmi[k],
      u_C1 = datU_C1, u_C2 = datU_C2, matS1 = matS_C1, matS2 = matS_C2,
      P_C1 = lstPimaTrain$numNoDiabetes/(lstPimaTrain$numNoDiabetes+lstPimaTrain$numDiabetes),
      P_C2 = lstPimaTrain$numNoDiabetes/(lstPimaTrain$numDiabetes+lstPimaTrain$numDiabetes)
    )
    
    if( matZResult1[k,m] >= datBorder[m] )
    {
      matResult1[k,m] <- TRUE
    }
    else
    {
      matResult1[k,m] <- FALSE
    }
    
    tblRes <- table( lstPimaTrain$bResult, matResult2[,m] )
    dfROC1$sigma2[m] <- tblRes[2,1]/(lstPimaTrain$numDiabetes)
    dfROC1$sigma1[m] <- tblRes[1,1]/(lstPimaTrain$numNoDiabetes)      
  }
}

############################
# 等損失直線[loss line]    #
############################
dfLossLine1 <- data.frame( # 等損失直線L12
  dat1 = c( -0.01, 0.10 ),
  dat2 = c( -0.01, 0.98 ) 
)
dfLossLine2 <- data.frame( # 等損失直線L21
  dat1 = c( -0.01, 0.17 ),
  dat2 = c( 0.00, 0.98 ) 
)


############################
# set graphics parameters  #
############################
# 軸に関してのデータリスト
lstAxis <- list(                        
  xMin = 0.0, xMax = 1.0,  # x軸の最小値、最大値
  yMin = 0.0, yMax = 1.0,  # y軸の最小値、最大値
  zMin = 0.0, zMax = 1.0,  # z軸の最小値、最大値
  xlim = range( c(0.0, 1.0) ), 
  ylim = range( c(0.0, 1.0) ), 
  zlim = range( c(0.0, 1.0) ),
  mainTitle1 = "ROC曲線（２次元正規分布）\n２次識別関数[secondary idification function]", # 図のメインタイトル（図の上）
  mainTitle2 = "ROC曲線（２次元正規分布）\n線形識別関数[liner idification function]",     # 図のメインタイトル（図の上）
  mainTitle3 = "ROC曲線（２次元正規分布）\n２次識別関数+線形識別関数",     # 図のメインタイトル（図の上）
  subTitle1  = "２次識別関数[dim2 idification function]",    # 図のサブタイトル（図の下）
  subTitle2  = "線形識別関数[liner idification function]",    # 図のサブタイトル（図の下）
  subTitle3  = "２次識別関数+線形識別関数",    # 図のサブタイトル（図の下）
  xlab      = "偽陽性率 [false positive rate]",        # x軸の名前
  ylab      = "真陽性率 [true positive rate]",         # y軸の名前
  zlab      = "z"            # z軸の名前
)
lstAxis$xlim = range( c(lstAxis$xMin, lstAxis$xMax) )
lstAxis$ylim = range( c(lstAxis$yMin, lstAxis$yMax) )
lstAxis$zlim = range( c(lstAxis$zMin, lstAxis$zMax) )

#########################
# Draw figure           #
#########################
#------------------------------------------------
# plot ROC Curve (secondary idification function)
#------------------------------------------------
plot(
  dfROC2$sigma2, dfROC2$sigma1,
  main = lstAxis$mainTitle1,
  xlab = lstAxis$xlab, ylab = lstAxis$ylab,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  col = "red",
  pch = 1,     # pch=0(□), pch=1(○), pch=2(△), pch=3(+)
  type = "p"   # 点と線      
)
grid()  # 図にグリッド線を追加

# add plot loss line （等損失直線）
par(new=T)
plot( 
  dfLossLine1$dat1, dfLossLine1$dat2,
  xlab = lstAxis$xlab, ylab = lstAxis$ylab,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  col = "green",
  type = "l"
)

par(new=T)
plot( 
  dfLossLine2$dat1, dfLossLine2$dat2,
  xlab = lstAxis$xlab, ylab = lstAxis$ylab,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  col = "6",
  type = "l" 
)

# 凡例の追加
legend(
  x = 0.4, y = 0.5,
  legend = "２次識別関数",
  col = "red",
  pch = 1,
  text.width = 0.3
)
legend(
  x = 0.4, y = 0.3,
  legend = c("損失直線１", "損失直線２"),
  col = c("green","6"),
  lty = 1,
  text.width = 0.3
)

#------------------------------------------------
# plot ROC Curve (liner idification function)
#------------------------------------------------
plot(
  dfROC1$sigma2, dfROC1$sigma1,
  main = lstAxis$mainTitle2,
  xlab = lstAxis$xlab, ylab = lstAxis$ylab,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  col = "blue",
  pch = 3,     # pch=0(□), pch=1(○), pch=2(△), pch=3(+)
  type = "p"   # 点と線      
)
grid()  # 図にグリッド線を追加

# add plot loss line （等損失直線）
par(new=T)
plot( 
  dfLossLine1$dat1, dfLossLine1$dat2,
  xlab = lstAxis$xlab, ylab = lstAxis$ylab,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  col = "green",
  type = "l"
)

par(new=T)
plot( 
  dfLossLine2$dat1, dfLossLine2$dat2,
  xlab = lstAxis$xlab, ylab = lstAxis$ylab,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  col = "6",
  type = "l"
)

# 凡例の追加
legend(
  x = 0.4, y = 0.5,
  legend = "線形識別関数",
  col = "blue",
  pch = 3,
  text.width = 0.3
)
legend(
  x = 0.4, y = 0.3,
  legend = c("損失直線１", "損失直線２"),
  col = c("green","6"),
  lty = 1,
  text.width = 0.3
)

#-------------------------------------------------------------------------------
# plot ROC Curve (secondary idification function + liner idification function)
#-------------------------------------------------------------------------------
plot(
  dfROC2$sigma2, dfROC2$sigma1,
  main = lstAxis$mainTitle3,
  xlab = lstAxis$xlab, ylab = lstAxis$ylab,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  col = "red",
  pch = 1,     # pch=0(□), pch=1(○), pch=2(△), pch=3(+)
  type = "p"   # 点と線      
)
grid()  # 図にグリッド線を追加
par(new=T)
plot(
  dfROC1$sigma2, dfROC1$sigma1,
  main = lstAxis$mainTitle3,
  xlab = lstAxis$xlab, ylab = lstAxis$ylab,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  col = "blue",
  pch = 3,     # pch=0(□), pch=1(○), pch=2(△), pch=3(+)
  type = "p"   # 点と線      
)

# add plot loss line （等損失直線）
par(new=T)
plot( 
  dfLossLine1$dat1, dfLossLine1$dat2,
  xlab = lstAxis$xlab, ylab = lstAxis$ylab,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  col = "green",
  type = "l"
)

par(new=T)
plot( 
  dfLossLine2$dat1, dfLossLine2$dat2,
  xlab = lstAxis$xlab, ylab = lstAxis$ylab,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  col = "6",
  type = "l" 
)

# 凡例の追加
legend(
  x = 0.4, y = 0.6,
  legend = c("２次識別関数", "線形識別関数"),
  col = c("red","blue"),
  pch = c(1,3),
  text.width = 0.3
)
legend(
  x = 0.4, y = 0.3,
  legend = c("損失直線１", "損失直線２"),
  col = c("green","6"),
  lty = 1,
  text.width = 0.3
)