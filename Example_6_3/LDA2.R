#install.packages("lda")

# expand lib on memory
library( MASS )   # MASS package

# set options
options( digits=7 ) # �\������

#----------------------------------
# set iris data
#----------------------------------
# expand on memory
data( iris )

# copy data from iris data
dfIris <- iris
dfIris2 <- iris[,3:5]

# release memory
rm( iris )

#######################################
# LDA : liner discriminant analysis   #
#######################################
iris_ida <- lda( dfIris$Species ~ . , data = dfIris )
print( iris_ida )

irislinerC <- apply( iris_ida$means%*%iris_ida$scaling, 2, mean ) # �萔��C
cat( "\nConstant term:\n" )
print( irislinerC )

# print idification result in table
resultIris <- predict( iris_ida )
tblIris <- table( dfIris$Species, resultIris$class )
cat( "\nIdification result:" )
print( tblIris )

# print idification result in table
resultIris <- predict( iris_ida )
tblIris <- table( dfIris$Species, resultIris$class )
cat( "\nIdification result:" )
print( tblIris )

# idification lines
datSLength_LD1 <- 0.8293776
datSWidth_LD1  <- 1.5344731
datPLength_LD1 <- -2.2012117
datPWidth_LD1  <- -2.8104603

datSLength_LD2 <- 0.02410215
datSWidth_LD2  <- 2.16452123
datPLength_LD2 <- -0.93192121
datPWidth_LD2  <- 2.83918785

dat_x <- seq( from= -50.0, to=50.0, by=0.001 )
irisline_lda1 <- ( datSLength_LD1*dat_x + datSWidth_LD1*dat_x + datPLength_LD1*dat_x + datPWidth_LD1*dat_x + irislinerC[1] )
irisline_lda2 <- ( datSLength_LD2*dat_x + datSWidth_LD2*dat_x + datPLength_LD2*dat_x + datPWidth_LD2*dat_x + irislinerC[2] )

# mapping iris data �i�A�����f�[�^���ʑ��j
dfIrisTrans <- data.frame(
  s_ld1 = rep(0,50),
  s_ld2 = rep(0,50),
  
  c_ld1 = rep(0,50),
  c_ld2 = rep(0,50),
    
  v_ld1 = rep(0,50),
  v_ld2 = rep(0,50)  
)

dfIrisTrans$s_ld1 <- ( 
    datSLength_LD1*dfIris$Sepal.Length[1:50] + datPLength_LD1*dfIris$Petal.Length[1:50]
  + datSWidth_LD1*dfIris$Sepal.Width[1:50]   + datPWidth_LD1*dfIris$Petal.Width[1:50]
  + irislinerC[1]           
)

dfIrisTrans$s_ld2 <- ( 
    datSLength_LD2*dfIris$Sepal.Length[1:50] + datPLength_LD2*dfIris$Petal.Length[1:50]
  + datSWidth_LD2*dfIris$Sepal.Width[1:50]   + datPWidth_LD2*dfIris$Petal.Width[1:50] 
  + irislinerC[2] 
)

dfIrisTrans$c_ld1 <- ( 
    datSLength_LD1*dfIris$Sepal.Length[51:100] + datPLength_LD1*dfIris$Petal.Length[51:100]
  + datSWidth_LD1*dfIris$Sepal.Width[51:100]   + datPWidth_LD1*dfIris$Petal.Width[51:100]
  + irislinerC[1]
)
dfIrisTrans$c_ld2 <- ( 
    datSLength_LD2*dfIris$Sepal.Length[51:100] + datPLength_LD2*dfIris$Petal.Length[51:100]
  + datSWidth_LD2*dfIris$Sepal.Width[51:100]   + datPWidth_LD2*dfIris$Petal.Width[51:100]
  + irislinerC[2]
)

dfIrisTrans$v_ld1 <- ( 
    datSLength_LD1*dfIris$Sepal.Length[101:150] + datPLength_LD1*dfIris$Petal.Length[101:150]
  + datSWidth_LD1*dfIris$Sepal.Width[101:150]   + datPWidth_LD1*dfIris$Petal.Width[101:150]
  + irislinerC[1]
)
dfIrisTrans$v_ld2 <- (
    datSLength_LD2*dfIris$Sepal.Length[101:150] + datPLength_LD2*dfIris$Petal.Length[101:150]
  + datSWidth_LD2*dfIris$Sepal.Width[101:150]   + datPWidth_LD2*dfIris$Petal.Width[101:150]
  + irislinerC[2]
)

#######################################
# LDA : liner discriminant analysis   #
#######################################
iris_ida <- lda( dfIris$Species ~ . , data = dfIris2 )
print( iris_ida )

irislinerC2 <- apply( iris_ida$means%*%iris_ida$scaling, 2, mean ) # �萔��C
cat( "\nConstant term:\n" )
print( irislinerC2 )

# print idification result in table
resultIris <- predict( iris_ida )
tblIris <- table( dfIris$Species, resultIris$class )
cat( "\nIdification result:" )
print( tblIris )

# idification lines
datPLength2_LD1 <- 1.544371
datPWidth2_LD1  <- 2.402394

datPLength2_LD2 <- -2.161222
datPWidth2_LD2  <- 5.042599

dat_x <- seq( from= -50.0, to=50.0, by=0.001 )
irisline2_lda1 <- ( datPLength_LD1*dat_x + datPWidth_LD1*dat_x + irislinerC2[1] )
irisline2_lda2 <- ( datPLength_LD2*dat_x + datPWidth_LD2*dat_x + irislinerC2[2] )

# mapping iris data �i�A�����f�[�^���ʑ��j
dfIrisTrans2 <- data.frame(
  s_ld1 = rep(0,50),  s_ld2 = rep(0,50),
  c_ld1 = rep(0,50),  c_ld2 = rep(0,50),
  v_ld1 = rep(0,50),  v_ld2 = rep(0,50)  
)

dfIrisTrans2$s_ld1 <- ( datPLength2_LD1*dfIris2$Petal.Length[1:50] + irislinerC2[1] )
dfIrisTrans2$s_ld2 <- ( datPWidth2_LD1*dfIris2$Petal.Width[1:50]   + irislinerC2[2] )

dfIrisTrans2$c_ld1 <- ( datPLength2_LD1*dfIris2$Petal.Length[51:100] + irislinerC2[1] )
dfIrisTrans2$c_ld2 <- ( datPWidth2_LD1*dfIris2$Petal.Width[51:100]   + irislinerC2[2] )

dfIrisTrans2$v_ld1 <- ( datPLength2_LD1*dfIris2$Petal.Length[101:150] + irislinerC2[1] )
dfIrisTrans2$v_ld2 <- ( datPWidth2_LD1*dfIris2$Petal.Width[101:150]   + irislinerC2[2] )

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
lstAxis$xMin <- -15
lstAxis$xMax <- 7
lstAxis$yMin <- 0
lstAxis$yMax <- 21
lstAxis$xlim = range( c(lstAxis$xMin, lstAxis$xMax) )
lstAxis$ylim = range( c(lstAxis$yMin, lstAxis$yMax) )
lstAxis$zlim = range( c(lstAxis$zMin, lstAxis$zMax) )
lstAxis$xlab <- "LD1"
lstAxis$ylab <- "LD2"
lstAxis$mainTitle <- "�S����������Ԃ���Q�������ʋ�Ԃւ̎ʑ����z"
lstAxis$subTitle <- "�A�����f�[�^[iris]"

# plot frame only
par(new=F)
plot.new()  # clear
plot( c(), type='n',
      main = lstAxis$mainTitle,
      sub = lstAxis$subTitle,
      xlim=lstAxis$xlim, ylim=lstAxis$ylim,
      xlab=lstAxis$xlab, ylab=lstAxis$ylab
)
grid() #�O���b�h����ǉ�

############################
# Draw figure              #
############################
# plot setosa
par(new=T)
plot(
  dfIrisTrans$s_ld1, dfIrisTrans$s_ld2,
  type='p',
  col = "red",
  pch = 's',
  main = lstAxis$mainTitle,
  sub = lstAxis$subTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)
grid() #�O���b�h����ǉ�

# plot versicolor
par(new=T)
plot(
  dfIrisTrans$c_ld1, dfIrisTrans$c_ld2,
  type='p',
  col = "blue",
  pch = 'c',
  main = lstAxis$mainTitle,
  sub = lstAxis$subTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)

# plot virginica
par(new=T)
plot(
  dfIrisTrans$v_ld1, dfIrisTrans$v_ld2,
  type='p',
  col = "green",
  pch = 'v',
  main = lstAxis$mainTitle,
  sub = lstAxis$subTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)

# add polt liner idification function
par(new=T)
plot(
  dat_x, irisline_lda1,
  type= 'l',
  main = lstAxis$mainTitle,
  sub = lstAxis$subTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)

par(new=T)
plot(
  dat_x, irisline_lda2,
 type='l',
 col = 5,
 main = lstAxis$mainTitle,
 sub = lstAxis$subTitle,
 xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
 xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)

# �}��̒ǉ�
legend(
 x = -15, y = 7,
 legend = c( "setosa","versicolor","virginica" ),
 col = c( "red","blue", "green"),
 pch = c( 's','c','v'),
 text.width = 5
) 
#----------------------------
# dim2
#----------------------------
lstAxis$mainTitle <- "�Q����������Ԃ���Q�������ʋ�Ԃւ̎ʑ����z"
lstAxis$subTitle <- "�A�����f�[�^[iris]"
lstAxis$xlab <- "�ԕق̒���[Petal length]"
lstAxis$ylab <- "�ԕق̕�[Petal width]"
lstAxis$xMin <- 0.0
lstAxis$xMax <- 7.0
lstAxis$yMin <- 0.0
lstAxis$yMax <- 3.0
lstAxis$xlim = range( c(lstAxis$xMin, lstAxis$xMax) )
lstAxis$ylim = range( c(lstAxis$yMin, lstAxis$yMax) )

# add polt liner idification function
plot(
  dat_x, irisline2_lda1,
  type= 'l',
  main = lstAxis$mainTitle,
  sub = lstAxis$subTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)

par(new=T)
plot(
  dat_x, irisline2_lda2,
  type='l',
  col = 5,
  main = lstAxis$mainTitle,
  sub = lstAxis$subTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)

# plot setosa
par(new=T)
plot(
  dfIris$Petal.Length[1:50], dfIris$Petal.Width[1:50],
  type='p',
  col = "red",
  pch = 's',
  main = lstAxis$mainTitle,
  sub = lstAxis$subTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)
grid() #�O���b�h����ǉ�

# plot versicolor
par(new=T)
plot(
  dfIris$Petal.Length[51:100], dfIris$Petal.Width[51:100],
  type='p',
  col = "blue",
  pch = 'c',
  main = lstAxis$mainTitle,
  sub = lstAxis$subTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)
# plot virginica
par(new=T)
plot(
  dfIris$Petal.Length[101:150], dfIris$Petal.Width[101:150],
  type='p',
  col = "green",
  pch = 'v',
  main = lstAxis$mainTitle,
  sub = lstAxis$subTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)

# �}��̒ǉ�
legend(
  x = 0, y = 3,
  legend = c( "setosa","versicolor","virginica" ),
  col = c( "red","blue", "green"),
  pch = c( 's','c','v'),
  text.width = 1.5
)