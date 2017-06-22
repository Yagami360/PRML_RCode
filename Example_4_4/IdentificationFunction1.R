library( MASS )   # MASS package（ピマ・インディアンPima のデータを使用）

# Pima data expand on memory
data( Pima.tr )
data( Pima.te )

# check Pima data structure
#str( Pima.tr )
#summary( Pima.tr )

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

#print(dfPimaTrain_C1)
#print(dfPimaTrain_C2)

# release memory
rm(Pima.tr) 
rm(Pima.te)

#####################################
# define idification functions      #
#####################################
#--------------
# クラスC1
#--------------
datU1_C1 <- mean( dfPimaTrain_C1$glu ) # クラス１（糖尿病発症なし）の変数１（glu）の平均値
datU2_C1 <- mean( dfPimaTrain_C1$bmi ) # クラス１（糖尿病発症有り）の変数２（BMI）の平均値
datU_C1 <- matrix( c(datU1_C1,datU2_C1), nrow = 2, ncol = 1)      # クラス１（糖尿病発症なし）の平均ベクトル

matS_C1 <- matrix( c(0,0,0,0), nrow = 2, ncol = 2 ) # クラス１（糖尿病発症なし）の共分散行列
matS_C1[1,1] <- sqrt( var( dfPimaTrain_C1$glu, dfPimaTrain_C1$glu ) )
matS_C1[1,2] <- sqrt( var( dfPimaTrain_C1$glu, dfPimaTrain_C1$bmi ) )
matS_C1[2,1] <- sqrt( var( dfPimaTrain_C1$bmi, dfPimaTrain_C1$glu ) )
matS_C1[2,2] <- sqrt( var( dfPimaTrain_C1$bmi, dfPimaTrain_C1$bmi ) )

#--------------
# クラスC2
#--------------
datU1_C2 <- mean( dfPimaTrain_C2$glu ) # クラス２（糖尿病発症なし）の変数１（glu）の平均値
datU2_C2 <- mean( dfPimaTrain_C2$bmi ) # クラス２（糖尿病発症有り）の変数２（BMI）の平均値
datU_C2 <- matrix( c(datU1_C2,datU2_C2), nrow = 2, ncol = 1)      # クラス１（糖尿病発症なし）の平均ベクトル

matS_C2 <- matrix( c(0,0,0,0), nrow = 2, ncol = 2 ) # クラス２（糖尿病発症なし）の共分散行列
matS_C2[1,1] <- sqrt( var( dfPimaTrain_C2$glu, dfPimaTrain_C2$glu ) )
matS_C2[1,2] <- sqrt( var( dfPimaTrain_C2$glu, dfPimaTrain_C2$bmi ) )
matS_C2[2,1] <- sqrt( var( dfPimaTrain_C2$bmi, dfPimaTrain_C2$glu ) )
matS_C2[2,2] <- sqrt( var( dfPimaTrain_C2$bmi, dfPimaTrain_C2$bmi ) )

#-----------------
# ２次元識別関数
#-----------------
Dim2IdificationFunc <- function( x1, x2, u_C1, u_C2, matS1, matS2, P_C1=0.5, P_C2=0.5 )
{
  datX <- matrix( 0, nrow = 2, ncol = 1  )
  datX[1,] <- x1
  datX[2,] <- x2
#print(datX)
  matW <- matrix( c(0,0,0,0), nrow = 2, ncol = 2 )
  matW <- ( solve(matS1) - solve(matS2) )
  vct <- ( t(u_C2)%*%solve(matS2) - t(u_C1)%*%solve(matS1) )
  r <- ( t(u_C1)%*%solve(matS1)%*%u_C1 - t(u_C2)%*%solve(matS2)%*%u_C2 + log( det(matS1)/det(matS2) ) - 2*log(P_C1/P_C2) )
#print(matW)
#print(vct)  
#print(r)
  z0 <- ( t(datX)%*%matW%*%datX )
#print(z0)
  z1 <- ( 2*vct%*%datX )
#print(z1)
  z <- ( z0 + z1 + r )
#print(z)
  return(z)
}

#------------------------------
# liner idification function
#-------------------------------
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

#------------------------------
# Set data
#-------------------------------
datX1 <- seq( from=0, to=200, by =4 )  # x1軸の値ベクトル
datX2 <- seq( from=0, to=50, by =1 )   # x2軸の値ベクトル
datX1 <- as.matrix( datX1 )
datX2 <- as.matrix( datX2 )
matZDim2 <- matrix( 0, nrow=length(datX1), ncol=length(datX2) )
#matZDim2 <- outer( X=datX1, Y=datX2, FUN=Dim2IdificationFunc )
matZDim1 <- matrix( 0, nrow=length(datX1), ncol=length(datX2) )

# for loop matZDim2[i,j]
for( j in 1:length(datX2) )
{
  for(i in 1:length(datX1) )
  {
    matZDim2[i,j] <- Dim2IdificationFunc( 
                        x1 = datX1[i], x2 = datX2[j],
                        u_C1 = datU_C1, u_C2 = datU_C2, matS1 = matS_C1, matS2 = matS_C2 
    )
    
    matZDim1[i,j] <- Dim1IdificationFunc( 
      x1 = datX1[i], x2 = datX2[j],
      u_C1 = datU_C1, u_C2 = datU_C2, matS1 = matS_C1, matS2 = matS_C2 , 
      P_C1 = lstPimaTrain$numNoDiabetes/(lstPimaTrain$numNoDiabetes+lstPimaTrain$numDiabetes),
      P_C2 = lstPimaTrain$numNoDiabetes/(lstPimaTrain$numDiabetes+lstPimaTrain$numDiabetes)
    )
  }
}
#print(matZDim2)

#----------------------------
# 学習データの再代入誤り率
#----------------------------
matZResult2 <- matrix( 0,     nrow = (lstPimaTrain$numNoDiabetes+lstPimaTrain$numDiabetes), ncol = 1 )
matZResult1 <- matrix( 0,     nrow = (lstPimaTrain$numNoDiabetes+lstPimaTrain$numDiabetes), ncol = 1 )
matResult   <- matrix( FALSE, nrow = (lstPimaTrain$numNoDiabetes+lstPimaTrain$numDiabetes), ncol = 2 )

for( k in 1:(lstPimaTrain$numNoDiabetes+lstPimaTrain$numDiabetes) )
{
  matZResult2[k,1] <- Dim2IdificationFunc(
      x1 = lstPimaTrain$glu[k], x2 = lstPimaTrain$bmi[k],
      u_C1 = datU_C1, u_C2 = datU_C2, matS1 = matS_C1, matS2 = matS_C2 )

   if( matZResult2[k,1] >= 0 )
   {
     matResult[k,1] <- TRUE
   }
   else
   {
     matResult[k,1] <- FALSE
   }
}
for( k in 1:(lstPimaTrain$numNoDiabetes+lstPimaTrain$numDiabetes) )
{
  matZResult1[k,1] <- Dim1IdificationFunc(
    x1 = lstPimaTrain$glu[k], x2 = lstPimaTrain$bmi[k],
    u_C1 = datU_C1, u_C2 = datU_C2, matS1 = matS_C1, matS2 = matS_C2,
    P_C1 = lstPimaTrain$numNoDiabetes/(lstPimaTrain$numNoDiabetes+lstPimaTrain$numDiabetes),
    P_C2 = lstPimaTrain$numNoDiabetes/(lstPimaTrain$numDiabetes+lstPimaTrain$numDiabetes)
  )
  if( matZResult1[k,1] >= 0 )
  {
    matResult[k,2] <- TRUE
  }
  else
  {
    matResult[k,2] <- FALSE
  }
}

tblRes2 <- table( lstPimaTrain$bResult, matResult[,1] )
tblRes1 <- table( lstPimaTrain$bResult, matResult[,2] )
print(tblRes2)
print(tblRes1)


############################
# set graphics parameters  #
############################
par( mfrow=c(1,1) )

# 軸に関してのデータリスト
lstAxis <- list(                        
  xMin = 0.0, xMax = 200.0,  # x軸の最小値、最大値
  yMin = 0.0, yMax = 50.0,   # y軸の最小値、最大値
  zMin = 0.0, zMax = 1.0,   # z軸の最小値、最大値
  xlim = range( c(0.0, 1.0) ), 
  ylim = range( c(0.0, 1.0) ), 
  zlim = range( c(0.0, 1.0) ),
  mainTitle1 = "Pima.tr（散布図）", # 図のメインタイトル（図の上）
  mainTitle2 = "２次識別関数",     # 図のメインタイトル（図の上）
  mainTitle3 = "線形識別関数",     # 図のメインタイトル（図の上）
  subTitle  = "subTitle",    # 図のサブタイトル（図の下）
  xlab      = "glu",         # x軸の名前
  ylab      = "BMI",         # y軸の名前
  zlab      = "z"            # z軸の名前
)
lstAxis$xlim = range( c(lstAxis$xMin, lstAxis$xMax) )
lstAxis$ylim = range( c(lstAxis$yMin, lstAxis$yMax) )
lstAxis$zlim = range( c(lstAxis$zMin, lstAxis$zMax) )


#########################
# Draw figure           #
#########################
#-----------------------
#  scatter plot
#-----------------------
# 糖尿病無しのplot
plot(
  x = dfPimaTrain_C1$glu, 
  y = dfPimaTrain_C1$bmi,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  main = lstAxis$mainTitle1,
  xlab = lstAxis$xlab, ylab = lstAxis$ylab,
  col = "blue",
  pch = 5,     # pch=0(□), pch=1(○), pch=2(△), pch=3(+)
  type = "p"
)
grid()

# 糖尿病有りのplotの追加
par(new=TRUE)
plot(
  x = dfPimaTrain_C2$glu, 
  y = dfPimaTrain_C2$bmi,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  main = lstAxis$mainTitle,
  xlab = lstAxis$xlab, ylab = lstAxis$ylab,
  col = "red",
  pch = 1,     # pch=0(□), pch=1(○), pch=2(△), pch=3(+)
  type = "p"
)

# 凡例の追加
legend(
  x = 0, y = 15,
  legend = c("糖尿病発症無し","糖尿病発症有り"),
  col = c("blue","red"),
  pch = c(5,1),
  text.width = 60
) 

#-------------------------------
# scatter plot
# + 2dim idification function
#-------------------------------
#win.graph() # 別ののグラフィックウインドウに作図
# 糖尿病無しのplot
plot(
  x = dfPimaTrain_C1$glu, 
  y = dfPimaTrain_C1$bmi,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  main = lstAxis$mainTitle2,
  xlab = lstAxis$xlab, ylab = lstAxis$ylab,
  col = "blue",
  pch = 5,     # pch=0(□), pch=1(○), pch=2(△), pch=3(+)
  type = "p"
)
grid()

# 糖尿病有りのplotの追加
par(new=TRUE)
plot(
  x = dfPimaTrain_C2$glu, 
  y = dfPimaTrain_C2$bmi,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  main = lstAxis$mainTitle2,
  xlab = lstAxis$xlab, ylab = lstAxis$ylab,
  col = "red",
  pch = 1,     # pch=0(□), pch=1(○), pch=2(△), pch=3(+)
  type = "p"
)

# contour() 関数で、等高線を追記
par(new=TRUE)
contour(
  x = datX1, y = datX2, z = matZDim2,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  lty = "dotdash",
  nlevels = 8
)
# 識別境界となる等高線を強調表示
par(new=TRUE)
contour(
  x = datX1, y = datX2, z = matZDim2,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  col = "green",
  levels = 0,
  nlevels = 1
)

# 凡例の追加
legend(
  x = 0, y = 15,
  legend = c("糖尿病発症無し","糖尿病発症有り"),
  col = c("blue","red"),
  pch = c(5,1),
  text.width = 60
) 

#------------------------------
# scatter plot
# + liner idification function
#------------------------------
#win.graph() # 別ののグラフィックウインドウに作図
# 糖尿病無しのplot
plot(
  x = dfPimaTrain_C1$glu, 
  y = dfPimaTrain_C1$bmi,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  main = lstAxis$mainTitle3,
  xlab = lstAxis$xlab, ylab = lstAxis$ylab,
  col = "blue",
  pch = 5,     # pch=0(□), pch=1(○), pch=2(△), pch=3(+)
  type = "p"
)
grid()

# 糖尿病有りのplotの追加
par(new=TRUE)
plot(
  x = dfPimaTrain_C2$glu, 
  y = dfPimaTrain_C2$bmi,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  main = lstAxis$mainTitle3,
  xlab = lstAxis$xlab, ylab = lstAxis$ylab,
  col = "red",
  pch = 1,     # pch=0(□), pch=1(○), pch=2(△), pch=3(+)
  type = "p"
)

# contour() 関数で、等高線を追記
par(new=TRUE)
contour(
  x = datX1, y = datX2, z = matZDim1,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  lty = "dotdash",
  nlevels = 8
)
# 識別境界となる等高線を強調表示
par(new=TRUE)
contour(
  x = datX1, y = datX2, z = matZDim1,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  col = "green",
  levels = 0,
  nlevels = 1
)

# 凡例の追加
legend(
  x = 0, y = 15,
  legend = c("糖尿病発症無し","糖尿病発症有り"),
  col = c("blue","red"),
  pch = c(5,1),
  text.width = 60
) 

#------------------------------
# plot ROC Curve
#------------------------------
#win.graph() # 別ののグラフィックウインドウに作図
