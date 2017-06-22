#install.packages("lda")
#install.packages("jpeg")

# expand lib on memory
library( MASS )   # MASS package
library( jpeg )   # jpeg画像の読み込み

# set options
options( digits=7 ) # 表示桁数

# load image data (jpeg)
jpeg_NekoSensei <- readJPEG( "nekosensei_greyscale.jpg" ) # 560*420 = 235200 pixel(3data<rgb> per 1 pixel)
class( jpeg_NekoSensei )      # 3次元配列 um[1:420, 1:560, 1:3]
attributes( jpeg_NekoSensei )

########################################
# convert to thresholding/binary image #
########################################
dfNekoSensei <- as.data.frame( expand.grid(jpeg_NekoSensei) ) # convert to data frame
dat_levels <- c(
  rep( min(dfNekoSensei$Var1), length(dfNekoSensei$Var1)/2 ), 
  rep( max(dfNekoSensei$Var1), length(dfNekoSensei$Var1)/2 ) 
) # binary data set (0 or 1)

dat_lda <- lda( dat_levels ~ . , data = dfNekoSensei )
print( dat_lda )

# 線形判別係数 [coefficients of linear discriminants] から線形識別関数を求める
linerC <- apply( dat_lda$means%*%dat_lda$scaling, 2, mean ) # 定数項C
cat("\nConstant term:\n")
print(linerC)

# print idification result in table
result <- predict( dat_lda )
tbl <- table( dat_levels, result$class )
cat("\nIdification result:")
print(tbl)

# result$class convert to vector
imgNekosensei1 <- array( 
  data = rep( 0,length(dfNekoSensei$Var1) ),
  dim = c(420,560,3)
)
imgNekosensei2 <- array( 
  data = rep( 0,length(dfNekoSensei$Var1) ),
  dim = c(420,560,3)
)

# バイナリ値に変換（簡単のためforループ使用）
for( i in 1:length(imgNekosensei) )
{
  if( result$class[i] == 1 )
  {
    imgNekosensei1[i] <- 1
    imgNekosensei2[i] <- 0
  }
  else
  {
    imgNekosensei1[i] <- 0    
    imgNekosensei2[i] <- 1
  }
}

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
lstAxis$xMin <- 0
lstAxis$xMax <- 560
lstAxis$yMin <- 0
lstAxis$yMax <- 420
lstAxis$xlim = range( c(lstAxis$xMin, lstAxis$xMax) )
lstAxis$ylim = range( c(lstAxis$yMin, lstAxis$yMax) )
lstAxis$zlim = range( c(lstAxis$zMin, lstAxis$zMax) )
lstAxis$xlab <- "x1"
lstAxis$ylab <- "x2"
lstAxis$mainTitle <- "ねこ先生（グレースケール）[Greyscale]" # 図のメインタイトル（図の上）

# plot frame only
par(new=F)
plot.new()  # clear
plot( c(), type='n',
      main = lstAxis$mainTitle,
      xlim=lstAxis$xlim, ylim=lstAxis$ylim,
      xlab=lstAxis$xlab, ylab=lstAxis$ylab
)
#grid() #グリッド線を追加

############################
# Draw Image and figure    #
############################
# draw original image
rasterImage( 
  image = jpeg_NekoSensei, 
  xleft = lstAxis$xMin, xright = lstAxis$xMax, 
  ybottom = lstAxis$yMin, ytop = lstAxis$yMax
)

# draw converted image
lstAxis$mainTitle <- "ねこ先生（２値化処理後）"  # 図のメインタイトル（図の上）
plot( c(), type='n',
      main = lstAxis$mainTitle,
      xlim=lstAxis$xlim, ylim=lstAxis$ylim,
      xlab=lstAxis$xlab, ylab=lstAxis$ylab
)
rasterImage(
  image = imgNekosensei1, # 変換後のデータ
  xleft = lstAxis$xMin, xright = lstAxis$xMax, 
  ybottom = lstAxis$yMin, ytop = lstAxis$yMax
)

lstAxis$mainTitle <- "ねこ先生（２値化処理後＜反転＞）"  # 図のメインタイトル（図の上）
plot( c(), type='n',
      main = lstAxis$mainTitle,
      xlim=lstAxis$xlim, ylim=lstAxis$ylim,
      xlab=lstAxis$xlab, ylab=lstAxis$ylab
)
rasterImage(
  image = imgNekosensei2, # 変換後のデータ
  xleft = lstAxis$xMin, xright = lstAxis$xMax, 
  ybottom = lstAxis$yMin, ytop = lstAxis$yMax
)
