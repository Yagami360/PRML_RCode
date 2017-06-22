library(MASS)                             
library(class)                                  # knn法を含むライブラリ

path_data = "train.csv"                         # use kaggle MIST data( train.csv:73.22 MB )

dfData <- read.csv( path_data ) # （正解ラベル付きの）42000人分のデータ

# ホールドアウト法 [hold-out method] でデータ分割
numData <- 1500
matTrain <- data.matrix( dfData[1:numData,-1] )                 # データから学習データをランダムサンプリング
matTest  <- data.matrix( dfData[(numData+1):(2*numData),-1] )   # データからテストデータをランダムサンプリング
fTrainTeach <- factor( dfData[1:numData,1] )                    # 学習データの教師データ
fTestTeach  <- factor( dfData[(numData+1):(2*numData),1] )      # テストデータの教師データ

# データの画像をヒートマップ表示
par( mfrow=c(20,20) )   # 20*20画面表示
par( mar=c(0,0,0,0) )   # 余白なし
par( xaxt="n" )
par( yaxt="n" )


#（※簡単のためforループ使用）
for ( i in 1:numData ) # plot train_data image
{
  matImage <- matrix( matTrain[i,], 28, 28 )      # １つの画像は 28*28=784 ピクセル
  image( matImage[,28:1] )
}

#par( xaxt="y" )
#par( yaxt="y" )
#for ( i in 1:numData ) # plot test_data image
#{
#  matImage <- matrix( matTest[i,], 28, 28 )      # １つの画像は 28*28=784 ピクセル
#  image( matImage[,28:1] )
#}

# 学習データの特定の画像をヒートマップ表示
#matImage <- matrix( matTrain[2,], 28, 28 )      # １つの画像は 28*28=784 ピクセル
#image( matImage[,28:1] )


# 学習データの正規化


# 学習データのフィルタリング


# 最近傍法（k=1）
k <- 1    # knn法で k=1 のときは最近傍法
result <- knn( matTrain,matTest,fTrainTeach,k,prob=TRUE )
tblRes <- table( fTestTeach,result )
print(tblRes)


#誤識別率の表示
tblRes <- unclass(tblRes)
errorRate <- (tblRes[1,1]+tblRes[2,2]+tblRes[3,3]+tblRes[4,4]+tblRes[5,5]+tblRes[6,6]+tblRes[7,7]+tblRes[8,8]+tblRes[9,9]+tblRes[10,10])
errorRate <- 100-(errorRate/numData)*100

errorRates <- c(1:10)
texts <- c(1:10)
for( i in 1:length(errorRates) )
{
  errorRates[i] <- (tblRes[i,i])/(tblRes[i,1]+tblRes[i,2]+tblRes[i,3]+tblRes[i,4]+tblRes[i,5]+tblRes[i,6]+tblRes[i,7]+tblRes[i,8]+tblRes[i,9]+tblRes[i,10])
  errorRates[i] <- 100-(errorRates[i]*100)
  texts[i] <- paste( i-1,"：",errorRates[i],"%" )
}

paste( "全誤識別率：",errorRate,"%" )
print( texts )
