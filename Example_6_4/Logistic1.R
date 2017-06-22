#install.packages("kernlab")

# expand lib on memory
library( MASS )    # MASS package
library( kernlab ) # 

# set options
options( digits=7 ) # 表示桁数

#####################################
# set iris data
#####################################
# expand on memory
data( iris )

# copy data from iris data
dfIris <- data.frame(
  datSLength = iris$Sepal.Length,
  datSWidth  = iris$Sepal.Width,
  datPLength = iris$Petal.Length,
  datPWidth  = iris$Petal.Width,
  
  # 種別（setosa,virginica）を１つのクラスにまとめて,
  # ２つのクラス（C1：setosa and virginica、C2：versicolor）の識別問題に変換する。
  class   = c( rep("C1",50), rep("C2",50), rep("C1",50) )
)

# sort from C1 to C2（線形分離不可能な識別問題にするためソートしない）
#dfIris$datClass   <- dfIris$datClass[ order( dfIris$datClass ) ]
#dfIris$datSLength <- dfIris$datSLength[ order( dfIris$datClass ) ]
#dfIris$datSWidth  <- dfIris$datSWidth[ order( dfIris$datClass ) ]
#dfIris$datPLength <- dfIris$datPLength[ order( dfIris$datClass ) ]
#dfIris$datPWidth  <- dfIris$datPWidth[ order( dfIris$datClass ) ]

#-------------------------------------
# LDA : liner discriminant analysis
#-------------------------------------
iris_ida <- lda( dfIris$class ~ . , data = dfIris )
summary( iris_ida )
print( iris_ida )
irislinerC <- apply( iris_ida$means%*%iris_ida$scaling, 2, mean ) # 定数項C
cat( "\nConstant term:\n" )
print( irislinerC )

# print idification result in table
resultIris <- predict( iris_ida )
tblIris <- table( dfIris$class, resultIris$class )
cat( "\nIdification result:" )
print( tblIris )

# release memory
#rm( iris )

# Coefficients of idification lines 
datSLength_LD <- iris_ida$scaling[1]
datSWidth_LD  <- iris_ida$scaling[2]
datPLength_LD <- iris_ida$scaling[3]
datPWidth_LD  <- iris_ida$scaling[4]

# mapping iris data （アヤメデータを写像）
dfIrisTransLd <- data.frame(
  x1 = rep(0,150), x2 = rep(0,150),
  class = c( rep("C1", 50), rep("C2", 50), rep("C1", 50) ) 
)

# setosa
#dfIrisTransLd$x1[1:50] <- ( datSLength_LD*iris$Sepal.Length[1:50] + datSWidth_LD*iris$Sepal.Width[1:50] + irislinerC )
#dfIrisTransLd$x2[1:50] <- ( datPLength_LD*iris$Petal.Length[1:50] + datPWidth_LD*iris$Petal.Width[1:50] + irislinerC )
dfIrisTransLd$x1[1:50] <- ( datSLength_LD*iris$Sepal.Length[1:50] + datPLength_LD*iris$Petal.Length[1:50] + irislinerC )
dfIrisTransLd$x2[1:50] <- ( datSWidth_LD*iris$Sepal.Width[1:50] + datPWidth_LD*iris$Petal.Width[1:50] + irislinerC )

# versicolor
#dfIrisTransLd$x1[51:100] <- ( datSLength_LD*iris$Sepal.Length[51:100] + datSWidth_LD*iris$Sepal.Width[51:100] + irislinerC )
#dfIrisTransLd$x2[51:100] <- ( datPLength_LD*iris$Petal.Length[51:100] + datPWidth_LD*iris$Petal.Width[51:100] + irislinerC )
dfIrisTransLd$x1[51:100] <- ( datSLength_LD*iris$Sepal.Length[51:100] + datPLength_LD*iris$Petal.Length[51:100] + irislinerC )
dfIrisTransLd$x2[51:100] <- ( datSWidth_LD*iris$Sepal.Width[51:100] + datPWidth_LD*iris$Petal.Width[51:100] + irislinerC )

# virginica
#dfIrisTransLd$x1[101:150] <- ( datSLength_LD*iris$Sepal.Length[101:150] + datSWidth_LD*iris$Sepal.Width[101:150] + irislinerC )
#dfIrisTransLd$x2[101:150] <- ( datPLength_LD*iris$Petal.Length[101:150] + datPWidth_LD*iris$Petal.Width[101:150] + irislinerC )
dfIrisTransLd$x1[101:150] <- ( datSLength_LD*iris$Sepal.Length[101:150] + datPLength_LD*iris$Petal.Length[101:150] + irislinerC )
dfIrisTransLd$x2[101:150] <- ( datSWidth_LD*iris$Sepal.Width[101:150] + datPWidth_LD*iris$Petal.Width[101:150] + irislinerC )

#-------------------------------------
# logisitic regression
#-------------------------------------
iris_glm <- glm(                      # 一般化線形モデルの関数
  formula = dfIrisTransLd$class ~ .,  # モデル式
  family = binomial(link = "logit"),  # 目的変数の確率分布
                                      # リンク関数 binomial(link = "logit") 目的変数が2値変数
                                      # 対応しているリンク関数は'logit'（ロジスティック回帰／ロジットモデル）
  data = dfIrisTransLd
)
print( iris_glm )
#summary( iris_glm )

# print idification result in table
resultIris <- predict( iris_ida )
tblIris <- table( dfIris$class, resultIris$class )
cat( "\nIdification result:" )
print( tblIris )

# ロジスティック回帰関数plot用のデータ構成
# logit(pi) = iris_glm$coefficients[1] + iris_glm$coefficients[2]*x1 + iris_glm$coefficients[3]*x2
LogisticFunction <- function( x1, x2, cof_x1, cof_x2, c_term )
{
  logistic <- cof_x1*x1 + cof_x2*x2 + c_term
  return(logistic)
}
dfIrisLogistic <- data.frame(
  x1 = seq( from = -20, to = 20, by = 0.5 ),
  x2 = seq( from = -20, to = 20, by = 0.5 ),
  z = 0
)
dfIrisLogistic$z <- matrix( 0, nrow = length(dfIrisLogistic$x1), ncol = length(dfIrisLogistic$x2) )

dfIrisLogistic$z <- outer( 
  dfIrisLogistic$x1, dfIrisLogistic$x2,
  LogisticFunction,
  iris_glm$coefficients[2],
  iris_glm$coefficients[3],
  iris_glm$coefficients[1]
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
  mainTitle = "mainTitle", # 図のメインタイトル（図の上）
  subTitle  = "subTitle",  # 図のサブタイトル（図の下）
  xlab      = "x", # x軸の名前
  ylab      = "y", # y軸の名前
  zlab      = "z"  # z軸の名前
)
lstAxis$xMin <- -10
lstAxis$xMax <-  5
lstAxis$yMin <- -20
lstAxis$yMax <- -10
lstAxis$zMin <- 0
lstAxis$zMax <- 1
lstAxis$xlim = range( c(lstAxis$xMin, lstAxis$xMax) )
lstAxis$ylim = range( c(lstAxis$yMin, lstAxis$yMax) )
lstAxis$zlim = range( c(lstAxis$zMin, lstAxis$zMax) )
lstAxis$xlab <- "LD_x1"
lstAxis$ylab <- "LD_x2"
lstAxis$mainTitle <- "線形ロジスティック回帰モデルによるアヤメデータの識別\n[ logisitic regression of iris data ]"
lstAxis$subTitle <- "class C1 : setosa and virginica, class C2 : versicolor"

# plot frame only
par(new=F)
plot.new()  # clear
plot( c(), type='n',
      main = lstAxis$mainTitle,
      xlim=lstAxis$xlim, ylim=lstAxis$ylim,
      xlab=lstAxis$xlab, ylab=lstAxis$ylab
)
grid() #グリッド線を追加

############################
# Draw figure              #
############################
# plot setosa
par(new=T)
plot(
  dfIrisTransLd$x1[1:50], dfIrisTransLd$x2[1:50],
  type='p',
  col = "red",
  pch = 's',
  main = lstAxis$mainTitle,
  sub = lstAxis$subTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)
grid() #グリッド線を追加

# add plot versicolor
par(new=T)
plot(
  dfIrisTransLd$x1[51:100], dfIrisTransLd$x2[51:100],
  type='p',
  col = "blue",
  pch = 'c',
  main = lstAxis$mainTitle,
  sub = lstAxis$subTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)

# add plot virginica
par(new=T)
plot(
  dfIrisTransLd$x1[101:150], dfIrisTransLd$x2[101:150],
  type='p',
  col = "red",
  pch = 'v',
  main = lstAxis$mainTitle,
  sub = lstAxis$subTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab
)

# logistic回帰の等高線追記
par(new=TRUE)
contour(
  x = dfIrisLogistic$x1, y = dfIrisLogistic$x2, z = dfIrisLogistic$z,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim,
  lwd = 2,
  lty = 3,
  nlevels = 20
)
# 識別境界となる等高線を強調表示
par(new=TRUE)
contour(
  x = dfIrisLogistic$x1, y = dfIrisLogistic$x2, z = dfIrisLogistic$z,
  xlim = lstAxis$xlim, ylim = lstAxis$ylim,
  lwd = 2,
  col = "green",
  levels = 0.5,
  nlevels = 1
)

# 凡例の追加
legend(
  x = -20, y = -2,
  legend = c( "setosa","versicolor","virginica" ),
  col = c( "red","blue", "red"),
  pch = c( 's','c','v'),
  text.width = 2
)

#-------------------------------
# logistic regression (3d plot) 
#-------------------------------
lstAxis$mainTitle <- "線形ロジスティック回帰の関数\n[function of logisitic regression]"
lstAxis$subTitle <- "線形な超平面\n[liner hyper space] "
persp( 
  x =  dfIrisLogistic$x1, y = dfIrisLogistic$x2, z = dfIrisLogistic$z,
  theta = 20, phi = 20, expand = 0.5,
  main = lstAxis$mainTitle,
  sub = lstAxis$subTitle,
  xlab=lstAxis$xlab, ylab=lstAxis$ylab, zlab=lstAxis$zlab,
  ticktype = "detailed",
  col = "lightblue"
)
