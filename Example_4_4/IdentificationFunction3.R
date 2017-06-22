# expand lib on memory
library( MASS )     # MASS package (use iris data)
library( mvtnorm )  # ���ϗʐ��K���z������

#install.packages("lda")

# set options
options( digits=7 ) # �\������

#----------------------------------
# set Normal Distribution's data
#----------------------------------
set.seed(8888) # ���񓯂������𔭐�������B
numRam <- 150  # �����̐�

# class C1 data
vecNorm_u1 <- c(2,2) # ���σx�N�g��
matNorm_S1 <- matrix( c(1.5,0,0,1.5), nrow = 2, ncol = 2 ) # �����U�s��
datRNorm1 <- rmvnorm( n=numRam, mean=vecNorm_u1, sigma=matNorm_S1 )  # 2�����̐��K���z�Ɋ�Â���������
dfNorm1 <- data.frame(
  x1 = datRNorm1[,1],
  x2 = datRNorm1[,2],
  z =  dmvnorm( x = datRNorm1, mean = vecNorm_u1, sigma = matNorm_S1),
  label = "C1"
)

# class C2 data
vecNorm_u2 <- c(-2,2) # ���σx�N�g��
matNorm_S2 <- matrix( c(1.0,0,0,1.0), nrow = 2, ncol = 2 ) # �����U�s��
datRNorm2 <- rmvnorm( n=numRam, mean=vecNorm_u2, sigma=matNorm_S2 )  # 2�����̐��K���z�Ɋ�Â���������
dfNorm2 <- data.frame(
  x1 = datRNorm2[,1],
  x2 = datRNorm2[,2],
  z =  dmvnorm( x = datRNorm2, mean = vecNorm_u2, sigma = matNorm_S2),
  label = "C2"
)

# class C3 data
vecNorm_u3 <- c(0,-2) # ���σx�N�g��
matNorm_S3 <- matrix( c(2,0,0,2), nrow = 2, ncol = 2 ) # �����U�s��
datRNorm3 <- rmvnorm( n=numRam, mean=vecNorm_u3, sigma=matNorm_S3 )  # 2�����̐��K���z�Ɋ�Â���������
dfNorm3 <- data.frame(
  x1 = datRNorm3[,1],
  x2 = datRNorm3[,2],
  z =  dmvnorm( x = datRNorm3, mean = vecNorm_u3, sigma = matNorm_S3),
  label = "C3"
)

#----------------------------------
# set iris data
#----------------------------------
# expand on memory
data( iris )

# copy data from iris data
dfIris <- iris[,3:5]

# release memory
rm( iris )

############################
# idification function     #
############################
#--------------------------
# normal distributions
#--------------------------
labelNorm <- c( 
  rep( "C1",length(dfNorm1$z) ),
  rep( "C2",length(dfNorm2$z) ),
  rep( "C3",length(dfNorm3$z) )
)
datNorm <- data.frame(
  x1 = c( dfNorm1$x1, dfNorm2$x1, dfNorm3$x1 ),
  x2 = c( dfNorm1$x2, dfNorm2$x2, dfNorm3$x2 )
)

dat_ida <- lda( labelNorm ~ . , data = datNorm )
print( dat_ida )

# ���`���ʌW�� [coefficients of linear discriminants] ������`���ʊ֐������߂�
linerC <- apply( dat_ida$means%*%dat_ida$scaling, 2, mean ) # �萔��C
cat("\nConstant term:\n")
print(linerC)

# print idification result in table
result <- predict( dat_ida )
tbl <- table( labelNorm, result$class )
cat("\nIdification result:")
print(tbl)

# idification lines
dat_x1 <- seq( from=-10.0,to=10.0,by=0.01 )
dat_x2 <- seq( from=-10.0,to=10.0,by=0.01 )

line1_x1 <- ( -0.37594*dat_x1 + linerC[1] )
line1_x2 <- ( -0.74802*dat_x2 + linerC[1] )
line2_x1 <- (  0.73623*dat_x1 + linerC[2] )
line2_x2 <- ( -0.29603*dat_x2 + linerC[2] )
line3_x1 <- (line1_x1+line2_x1)/2
line3_x2 <- (line1_x2+line2_x2)/2

#--------------------------
# iris
#--------------------------
iris_ida <- lda( dfIris$Species ~ . , data = dfIris )
print( iris_ida )

irislinerC <- apply( iris_ida$means%*%iris_ida$scaling, 2, mean ) # �萔��C
cat("\nConstant term:\n")
print(irislinerC)

# print idification result in table
resultIris <- predict( iris_ida )
tblIris <- table( dfIris$Species, resultIris$class )
cat("\nIdification result:")
print(tblIris)

# idification lines
dat_x1 <- seq( from= -100.0, to=100.0, by=0.01 )
dat_x2 <- seq( from= -100.0, to=100.0, by=0.01 )

irisline1_x1 <- ( 1.5443705304055673*dat_x1 + irislinerC[1] )
irisline1_x2 <- ( 2.4023943825909795*dat_x2 + irislinerC[1] )
irisline2_x1 <- (-2.1612223488303783*dat_x1 + irislinerC[2] )
irisline2_x2 <- ( 5.0425991578181089*dat_x2 + irislinerC[2] )
irisline3_x1 <- ( irisline1_x1+irisline2_x1 )/2
irisline3_x2 <- ( irisline1_x2+irisline2_x2 )/2

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
lstAxis$xMin <- -5.0
lstAxis$xMax <- 5.0
lstAxis$yMin <- -5.0
lstAxis$yMax <- 5.0
lstAxis$xlim = range( c(lstAxis$xMin, lstAxis$xMax) )
lstAxis$ylim = range( c(lstAxis$yMin, lstAxis$yMax) )
lstAxis$zlim = range( c(lstAxis$zMin, lstAxis$zMax) )
lstAxis$xlab <- "x1"
lstAxis$ylab <- "x2"

# plot frame only
par(new=F)
plot.new()  # clear
plot( c(), type='n',
      xlim=lstAxis$xlim, ylim=lstAxis$ylim,
      xlab=lstAxis$xlab, ylab=lstAxis$ylab
)
grid() #�O���b�h����ǉ�

#########################
# Draw figure           #
#########################
#---------------------------------
# draw Normal Distribution figure
#---------------------------------
# polt normal distributions (class C1,C2,C3)
lstAxis$mainTitle <- "�ő厯�ʊ֐��ɂ�鎯�ʁi���K���z�j"

# class C1
par(new=T)
plot(
  dfNorm1$x1, dfNorm1$x2,
  type='p',
  col = "red",
  pch = 1,
  main = lstAxis$mainTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab
)

# class C2
par(new=T)
plot(
  dfNorm2$x1, dfNorm2$x2,
  type='p',
  col = "blue",
  pch = 2,
  main = lstAxis$mainTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)

# class C3
par(new=T)
plot(
  dfNorm3$x1, dfNorm3$x2,
  type='p',
  col = "green",
  pch = 3,
  main = lstAxis$mainTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)

# add polt liner idification function
par(new=T)
plot(
  line1_x1[950:length(line1_x1)], line1_x2[950:length(line1_x1)],
  type='l',
  main = lstAxis$mainTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)
par(new=T)
plot(
  line2_x1[1000:length(line2_x2)], line2_x2[1000:length(line2_x2)],
  type='l',
  main = lstAxis$mainTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)
par(new=T)
plot(
  line3_x1[1:1000], line3_x2[1:1000],
  type='l',
  main = lstAxis$mainTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)

#---------------------------------
# draw iris figure
#---------------------------------
# plot iris data
lstAxis$mainTitle <- "�ő厯�ʊ֐��ɂ�鎯�ʁi�A�����f�[�^[iris]�j"
lstAxis$xlab <- "�ԕق̒���[Petal length]"
lstAxis$ylab <- "�ԕق̕�[Petal width]"
lstAxis$xMin <- 0.0
lstAxis$xMax <- 7.0
lstAxis$yMin <- 0.0
lstAxis$yMax <- 3.0
lstAxis$xlim = range( c(lstAxis$xMin, lstAxis$xMax) )
lstAxis$ylim = range( c(lstAxis$yMin, lstAxis$yMax) )

plot( 
  c(),
  type='p',
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)
grid()

# plot setosa
par(new=T)
plot(
  dfIris$Petal.Length[1:50], dfIris$Petal.Width[1:50],
  type='p',
  col = "red",
  pch = 's',
  main = lstAxis$mainTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)

# plot versicolor
par(new=T)
plot(
  dfIris$Petal.Length[51:100], dfIris$Petal.Width[51:100],
  type='p',
  col = "blue",
  pch = 'c',
  main = lstAxis$mainTitle,
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
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)

# add polt liner idification function
par(new=T)
plot(
  irisline1_x1[1:length(irisline1_x1)], irisline1_x2[1:length(irisline1_x2)],
  type='l',
  main = lstAxis$mainTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)

par(new=T)
plot(
  irisline2_x1, irisline2_x2,
  type='l',
  main = lstAxis$mainTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)
par(new=T)
plot(
  irisline3_x1[1:length(irisline3_x1)], irisline3_x2[1:length(irisline3_x2)],
  type='l',
  main = lstAxis$mainTitle,
  xlim=lstAxis$xlim, ylim=lstAxis$ylim, 
  xlab=lstAxis$xlab, ylab=lstAxis$ylab  
)

# �}��̒ǉ�
legend(
  x = 0.1, y = 2.5,
  legend = c( "setosa","versicolor","virginica" ),
  col = c( "red","blue", "green"),
  pch = c( 's','c','v'),
  text.width = 1.5
) 