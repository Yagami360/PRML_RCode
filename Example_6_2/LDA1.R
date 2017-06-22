#install.packages("lda")
#install.packages("jpeg")

# expand lib on memory
library( MASS )   # MASS package
library( jpeg )   # jpeg�摜�̓ǂݍ���

# set options
options( digits=7 ) # �\������

# load image data (jpeg)
jpeg_NekoSensei <- readJPEG( "nekosensei_greyscale.jpg" ) # 560*420 = 235200 pixel(3data<rgb> per 1 pixel)
class( jpeg_NekoSensei )      # 3�����z�� um[1:420, 1:560, 1:3]
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

# ���`���ʌW�� [coefficients of linear discriminants] ������`���ʊ֐������߂�
linerC <- apply( dat_lda$means%*%dat_lda$scaling, 2, mean ) # �萔��C
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
# ���Ɋւ��Ẵf�[�^���X�g
lstAxis <- list(                        
  xMin = 0.0, xMax = 1.0,  # x���̍ŏ��l�A�ő�l
  yMin = 0.0, yMax = 1.0,  # y���̍ŏ��l�A�ő�l
  zMin = 0.0, zMax = 1.0,  # z���̍ŏ��l�A�ő�l
  xlim = range( c(0.0, 1.0) ), 
  ylim = range( c(0.0, 1.0) ), 
  zlim = range( c(0.0, 1.0) ),
  mainTitle = "mainTitle", # �}�̃��C���^�C�g���i�}�̏�j
  subTitle  = "subTitle",  # �}�̃T�u�^�C�g���i�}�̉��j
  xlab      = "x", # x���̖��O
  ylab      = "y", # y���̖��O
  zlab      = "z"  # z���̖��O
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
lstAxis$mainTitle <- "�˂��搶�i�O���[�X�P�[���j[Greyscale]" # �}�̃��C���^�C�g���i�}�̏�j

# plot frame only
par(new=F)
plot.new()  # clear
plot( c(), type='n',
      main = lstAxis$mainTitle,
      xlim=lstAxis$xlim, ylim=lstAxis$ylim,
      xlab=lstAxis$xlab, ylab=lstAxis$ylab
)
#grid() #�O���b�h����ǉ�

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
lstAxis$mainTitle <- "�˂��搶�i�Q�l��������j"  # �}�̃��C���^�C�g���i�}�̏�j
plot( c(), type='n',
      main = lstAxis$mainTitle,
      xlim=lstAxis$xlim, ylim=lstAxis$ylim,
      xlab=lstAxis$xlab, ylab=lstAxis$ylab
)
rasterImage(
  image = imgNekosensei1, # �ϊ���̃f�[�^
  xleft = lstAxis$xMin, xright = lstAxis$xMax, 
  ybottom = lstAxis$yMin, ytop = lstAxis$yMax
)

lstAxis$mainTitle <- "�˂��搶�i�Q�l�������ぃ���]���j"  # �}�̃��C���^�C�g���i�}�̏�j
plot( c(), type='n',
      main = lstAxis$mainTitle,
      xlim=lstAxis$xlim, ylim=lstAxis$ylim,
      xlab=lstAxis$xlab, ylab=lstAxis$ylab
)
rasterImage(
  image = imgNekosensei2, # �ϊ���̃f�[�^
  xleft = lstAxis$xMin, xright = lstAxis$xMax, 
  ybottom = lstAxis$yMin, ytop = lstAxis$yMax
)