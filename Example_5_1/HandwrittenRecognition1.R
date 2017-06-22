library(MASS)                             
library(class)                                  # knn�@���܂ރ��C�u����

path_data = "train.csv"

dfData <- read.csv( path_data ) # �i�������x���t���́j42000�l���̃f�[�^

# �z�[���h�A�E�g�@ [hold-out method] �Ńf�[�^����
numData <- 1500
matTrain <- data.matrix( dfData[1:numData,-1] )                 # �f�[�^����w�K�f�[�^�������_���T���v�����O
matTest  <- data.matrix( dfData[(numData+1):(2*numData),-1] )   # �f�[�^����e�X�g�f�[�^�������_���T���v�����O
fTrainTeach <- factor( dfData[1:numData,1] )                    # �w�K�f�[�^�̋��t�f�[�^
fTestTeach  <- factor( dfData[(numData+1):(2*numData),1] )      # �e�X�g�f�[�^�̋��t�f�[�^

# �f�[�^�̉摜���q�[�g�}�b�v�\��
par( mfrow=c(20,20) )   # 20*20��ʕ\��
par( mar=c(0,0,0,0) )   # �]���Ȃ�
par( xaxt="n" )
par( yaxt="n" )


#�i���ȒP�̂���for���[�v�g�p�j
for ( i in 1:numData ) # plot train_data image
{
  matImage <- matrix( matTrain[i,], 28, 28 )      # �P�̉摜�� 28*28=784 �s�N�Z��
  image( matImage[,28:1] )
}

#par( xaxt="y" )
#par( yaxt="y" )
#for ( i in 1:numData ) # plot test_data image
#{
#  matImage <- matrix( matTest[i,], 28, 28 )      # �P�̉摜�� 28*28=784 �s�N�Z��
#  image( matImage[,28:1] )
#}

# �w�K�f�[�^�̓���̉摜���q�[�g�}�b�v�\��
#matImage <- matrix( matTrain[2,], 28, 28 )      # �P�̉摜�� 28*28=784 �s�N�Z��
#image( matImage[,28:1] )


# �w�K�f�[�^�̐��K��


# �w�K�f�[�^�̃t�B���^�����O


# �ŋߖT�@�ik=1�j
k <- 1    # knn�@�� k=1 �̂Ƃ��͍ŋߖT�@
result <- knn( matTrain,matTest,fTrainTeach,k,prob=TRUE )
tblRes <- table( fTestTeach,result )
print(tblRes)


#�뎯�ʗ��̕\��
tblRes <- unclass(tblRes)
errorRate <- (tblRes[1,1]+tblRes[2,2]+tblRes[3,3]+tblRes[4,4]+tblRes[5,5]+tblRes[6,6]+tblRes[7,7]+tblRes[8,8]+tblRes[9,9]+tblRes[10,10])
errorRate <- 100-(errorRate/numData)*100

errorRates <- c(1:10)
texts <- c(1:10)
for( i in 1:length(errorRates) )
{
  errorRates[i] <- (tblRes[i,i])/(tblRes[i,1]+tblRes[i,2]+tblRes[i,3]+tblRes[i,4]+tblRes[i,5]+tblRes[i,6]+tblRes[i,7]+tblRes[i,8]+tblRes[i,9]+tblRes[i,10])
  errorRates[i] <- 100-(errorRates[i]*100)
  texts[i] <- paste( i-1,"�F",errorRates[i],"%" )
}

paste( "�S�뎯�ʗ��F",errorRate,"%" )
print( texts )