library(MASS)                             # 多変数正規乱数の組込み関数を含むライブラリ
library(class)                            # knn法を含むライブラリ

set.seed(8888)                            # 毎回同じ乱数を発生させる。

# 各クラスの値の構成
mean_train1 <- c(2,2)                                  # クラス１の中央値
sigma_train1 <- matrix( c(2,0,0,2),2,2 )               # クラス１の共分散行列を生成
dat_train1 <- mvrnorm( 100,mean_train1,sigma_train1 )  # 多変数の正規分布に基づく乱数からデータ取得（＝ランダムサンプリング）
mean_train2 <- c(-2,0)                                 # クラス２の中央値
sigma_train2 <- matrix( c(2,0,0,2),2,2 )               # クラス２の共分散行列を生成
dat_train2 <- mvrnorm( 100,mean_train2,sigma_train2 )  # 多変数の正規分布に基づく乱数からデータ取得（＝ランダムサンプリング）
mean_train3 <- c(2,-2)                                 # クラス３の中央値
sigma_train3 <- matrix( c(2,0,0,2),2,2 )               # クラス３の共分散行列を生成
dat_train3 <- mvrnorm( 100,mean_train3,sigma_train3 )  # 多変数の正規分布に基づく乱数からデータ取得（＝ランダムサンプリング）

# 学習データ、教師データを作成 
train <- rbind( dat_train1,dat_train2,dat_train3 )               # 学習データ
teach <- factor( c(rep("C1",100),rep("C2",100),rep("C3",100)) )  # 教師データ


# 各テストデータの値の構成
mean_test1 <- c(2,2)                                   # テストデータ１の中央値
sigma_test1 <- matrix( c(2,0,0,2),2,2 )                # テストデータの共分散行列を生成
dat_test1 <- mvrnorm( 100,mean_test1,sigma_test1 )     # 多変数の正規分布に基づく乱数からデータ取得（＝ランダムサンプリング）
mean_test2 <- c(-2,0)                                  # テストデータ２の中央値
sigma_test2 <- matrix( c(2,0,0,2),2,2 )                # テストデータ２の共分散行列を生成
dat_test2 <- mvrnorm( 100,mean_test2,sigma_test2 )     # 多変数の正規分布に基づく乱数からデータ取得（＝ランダムサンプリング）
mean_test3 <- c(2,-2)                                  # テストデータ３の中央値
sigma_test3 <- matrix( c(2,0,0,2),2,2 )                # テストデータ３の共分散行列を生成
dat_test3 <- mvrnorm( 100,mean_test3,sigma_test3 )     # 多変数の正規分布に基づく乱数からデータ取得（＝ランダムサンプリング）

# テストデータを作成
test <- rbind( dat_test1,dat_test2,dat_test3 )  


# knn法
k <- 1
result <- knn( train,test,teach,k,prob=TRUE )
table( teach,result )

# 作図
# 全学習データとテストデータをクラス毎に色分けしてプロット
plot( dat_train1[,1],dat_train1[,2], col="red",  pch=1, xlim=c(-4,4),ylim=c(-4,4), xlab="x",ylab="y" )
par(new=T)
plot( dat_train2[,1],dat_train2[,2], col="blue", pch=2, xlim=c(-4,4),ylim=c(-4,4), xlab="x",ylab="y" )
par(new=T)
plot( dat_train3[,1],dat_train3[,2], col="green",pch=0, xlim=c(-4,4),ylim=c(-4,4), xlab="x",ylab="y" )
par(new=T)
plot( dat_test1[,1],dat_test1[,2], col="black",pch=1, xlim=c(-4,4),ylim=c(-4,4), xlab="x",ylab="y" )
par(new=T)
plot( dat_test2[,1],dat_test2[,2], col="black",pch=2, xlim=c(-4,4),ylim=c(-4,4), xlab="x",ylab="y" )
par(new=T)
plot( dat_test3[,1],dat_test3[,2], col="black",pch=0, xlim=c(-4,4),ylim=c(-4,4), xlab="x",ylab="y" )
par(new=T)