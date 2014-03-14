# 必要なパッケージのインストール
# install.packages("e1071")
# install.packages("ggplot2")

# 必要なパッケージのロード
library(e1071)
library(ggplot2)

# データの読み込みと確認
coins.auth = read.table("./CodeIQ_auth.txt",header=F,sep="")
names(coins.auth) <- c("volume","weight","truth")
coins.auth$truth <- factor(coins.auth$truth)
print(head(coins.auth))

coins.my = read.table("./CodeIQ_mycoins.txt",header=F,sep=" ")
names(coins.my) <- c("volume","weight")
print(head(coins.my))

# summary 関数の適用(全体)
print(summary(coins.auth))
print(summary(coins.my))

# summary 関数の適用(真贋それぞれ別に)
print(summary(coins.auth[coins.auth$truth == 0,]))
print(summary(coins.auth[coins.auth$truth == 1,]))

# CodeIQ_auth.txt をグラフ化
graph = ggplot(coins.auth,aes(x=volume,y=weight)) + geom_point(aes(color=truth))
print(graph)

# SVM のモデルを作成してみる
model <- svm(
    truth ~ ., data = coins.auth, # coins.auth の truth を他の変数から予測
    kernel = "linear", # 線形カーネル
    cross = 5 # 交差検定の分割数
    )
print(summary(model)) # モデルの確認

### 正解率 100 % 達成！

## # 最適化(この時点で Acc: 1.0 なので今回はいらない)
## tune=tune.svm(
##     truth ~ .,
##     data = coins.auth,
##     kernel = "radial",
##     gamma=2^(seq(-15, 3)),
##     cost=2^(seq(-5, 15)),
##     tunecontrol=tune.control(
##         sampling="cross",
##         cross=10)
##     )
## cat("- Best parameters:\n")
## cat("Gamma =", tune$best.parameters$gamma,
##     "; Cost =", tune$best.parameters$cost, ";\n")
## cat("Accuracy:", 100 - tune$best.performance * 100, "%\n\n")

# 作成したモデルで分類してみる
model <- svm(
    truth ~ ., data = coins.auth, # coins.auth の truth を他の変数から予測
    kernel = "linear" # 線形カーネル
    )
ocoins.my$truth <- predict(model, coins.my)

graph = ggplot(coins.my,aes(x=volume,y=weight)) + geom_point(aes(color=truth))
print(graph)
