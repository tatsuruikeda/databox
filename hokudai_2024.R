install.packages("ROCR")
install.packages("randomForest")
install.packages("GGally")
install.packages("MLmetrics")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caTools")
install.packages("caret")
install.packages("lattice")

library(GGally) 
library(ggplot2)
library(caTools)
library(ROCR)
library(caret)
library(lattice)
library(MLmetrics)
library(randomForest)
library(rpart)
library(rpart.plot)

# 線形回帰のためのデータセット　============================

data <- read.csv("tcga_coad.csv")

log_transform <- function(x) {
  return (log(x+0.01))
}

data <- as.data.frame(lapply(data, log_transform))

# 散布図行列 --------------------------------------------------

ggpairs(data,lower=list(continuous=wrap("points",size=0.1)), 
        upper=list(continuous=wrap("cor",size=5)))

#勾配降下法のための準備 ======================================

X <- cbind(rep(1, nrow(data)), data[, 4]);y <- data[, 8] # TCF4 vs ZEB1

#勾配降下法のためのコスト関数定義

computeCost <- function(X, y, theta) {
  m <- length(y)
  J <- 0
  J <- 1/(2*m) * t(X %*% theta - y) %*% (X %*% theta - y)
  return(J)
}

#勾配降下法アルゴリズム

gradientDescent <- function(X, y, theta, alpha, num_iters) {
  m <- length(y) 
  J_history <- numeric(num_iters) 
  theta_history <- matrix(0, nrow = num_iters, ncol = length(theta))
  for (iter in 1:num_iters) {
    theta <- theta - alpha * (1/m) * (t(X) %*% ((X %*% theta) - y))
    J_history[iter] <- computeCost(X, y, theta)
    theta_history[iter, ] <- theta
  }
  result <- list(theta = theta, J_history = J_history, theta_history = theta_history)
  return(result)
}

# 勾配降下法実行

theta <- c(-5, -5);num_iters <- 1000;alpha <- 0.02

result <- gradientDescent(X, y, theta, alpha, num_iters)

print(result$theta)

# ３D図および等高線プロット作図
theta0_vals <- seq(-10, 10, length.out = 20) 
theta1_vals <- seq(-10, 10, length.out = 20)
J_vals <- matrix(0, nrow = length(theta0_vals), ncol = length(theta1_vals))

for (i in 1:length(theta0_vals)) {
  for (j in 1:length(theta1_vals)) {
    t <- c(theta0_vals[i], theta1_vals[j])
    J_vals[i, j] <- computeCost(X, y, t)
  }
}

# 3D
persp(theta1_vals, theta0_vals, J_vals, col = "lightyellow", theta = 60, phi = 15)

# 等高線
levels <- seq(0, 100, by = 5)
contour(theta0_vals, theta1_vals, J_vals, levels=levels) 

# Rによるlm関数===============================================================

Lm <- lm(data$ZEB1 ~ data$TCF4, data=data)
summary(Lm)

# lm関数の結果
plot(data$TCF4, data$ZEB1, col = "black")
abline(Lm, lwd = 2, col = "blue")

# 勾配降下法の結果
x1 <- seq(-2, 5, length=1000);y1 <- 0.56+1.05*x1 
plot(data$TCF4, data$ZEB1, col = "black")
lines(x1, y1, lwd = 2,col="red") 

# 混同行列作成関数　=========================================================

drawing_matrix <- function(model, type) {
  type_string <- deparse(substitute(type))
  
  if (type_string == "response"){
    predictTrain = predict(model, type=type_string, data=Train)
    predicted_train <- factor(as.numeric( predictTrain> 0.5))
    predictTest = predict(model, type=type_string, newdata=Test)
    predicted_test <- factor(as.numeric(predictTest > 0.5))
  } else{
    predicted_train <- predict(model, type=type_string, data=Train)
    predicted_test <- predict(model, type=type_string, newdata=Test)
  }
  
  actual_train <- factor(Train$category)
  actual_test <- factor(Test$category)
  accuracy_train <- Accuracy(y_pred = predicted_train, y_true = actual_train)
  accuracy_test <- Accuracy(y_pred = predicted_test, y_true = actual_test)
  precision_train <- Precision(y_pred = predicted_train, y_true = actual_train, positive = "1")
  precision_test <- Precision(y_pred = predicted_test, y_true = actual_test, positive = "1")
  sensitivity_train <- Sensitivity(y_pred = predicted_train, y_true = actual_train, positive = "1")
  specificity_train <- Specificity(y_pred = predicted_train, y_true = actual_train, positive = "1")
  sensitivity_test <- Sensitivity(y_pred = predicted_test, y_true = actual_test, positive = "1")
  specificity_test <- Specificity(y_pred = predicted_test, y_true = actual_test, positive = "1")
  
  
  print(ConfusionMatrix(predicted_train, actual_train)) 
  print(sprintf("Accuracy: %.3f", accuracy_train))
  print(sprintf("Precision: %.3f", precision_train))
  print(sprintf("Sensitivity: %.3f", sensitivity_train))
  print(sprintf("Specificity: %.3f", specificity_train))
  print(ConfusionMatrix(predicted_test, actual_test)) 
  print(sprintf("Accuracy: %.3f", accuracy_test))
  print(sprintf("Precision: %.3f", precision_test))
  print(sprintf("Sensitivity: %.3f", sensitivity_test))
  print(sprintf("Specificity: %.3f", specificity_test))
}

　# type　glmにはresponse, tree，RFにはclass

# ROC curve 作成function ====================================================

plotting_roc <- function(model, type){
  type_string <- deparse(substitute(type))
  predictTrain <- predict(model, type=type_string, data=Train)
  predictTest = predict(model, type=type_string, newdata=Test)
  set.seed(1)
  if (type_string == "response"){
    pred_train <- prediction(predictTrain, Train$category)
    pred_test <- prediction(predictTest, Test$category)
  } else{
    pred_train <- prediction(predictTrain[,2], Train$category)
    pred_test <- prediction(predictTest[,2], Test$category)
  }
  
  perf_train <- performance(pred_train, "tpr", "fpr")
  perf_test <- performance(pred_test, "tpr", "fpr")
  
  #plot(perf_train, lty=1, lwd=2,  col="blue", main="Train")
  plot(perf_test, lty=1, lwd=2,  col="red", main="Test")
  
  #print(as.numeric(performance(pred_train, "auc")@y.values))
  print(as.numeric(performance(pred_test, "auc")@y.values))
}

# 分類問題のためのデータセット　==========================================

data = read.csv("classification.csv")
data = data[, -1]
data$category <- as.factor(data$category)

# 訓練データとテストデータの分割------------------------------------------
set.seed(1)
split = sample.split(data$category, SplitRatio = 0.75)　#caTools
Train = subset(data, split == TRUE)
Test = subset(data, split == FALSE)

# ロジスティック回帰=====================================================

Log <- glm(category ~ aa_score+position+blosum+gnomad, data = Train, family=binomial) 
#aa_score+position+blosum+gnomad

drawing_matrix(Log, response)

dev.off()
plotting_roc(Log, response)

# 交差検証　ロジスティクス回帰 ==========================================

data = read.csv("classification.csv")
data = data[, -1]
data$category <- as.factor(data$category)

set.seed(123)
k <- 5
folds <- createFolds(data$category, k = k, list = TRUE, returnTrain = TRUE)

train_results <- list();test_results <- list()
for(i in 1:k) {
  train_indices <- folds[[i]]
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  Log <- glm(train_data$category ~aa_score+position+blosum+gnomad, 
             data = train_data, family=binomial)
  
  predictTest = predict(Log, type="response", newdata=test_data)
  pred_test <- prediction(predictTest, test_data$category)
  test_results[[i]] <- as.numeric(performance(pred_test, "auc")@y.values)
  
}

test_mean_auc <- mean(unlist(test_results));print(test_mean_auc)

# 他のツールとの比較 ======================================

# ROC curves
data = read.csv("/content/classification.csv")
data = data[, -1]
data$category <- as.factor(data$category)

set.seed(1)
split = sample.split(data$category, SplitRatio = 0.75)　#caTools
Train = subset(data, split == TRUE)
Test = subset(data, split == FALSE)

Log <- glm(category ~ aa_score+position+blosum+gnomad, data = Train, family=binomial)
plotting_roc(Log, response)

for(i in 12:63){ 
  pred <- prediction(Test[,i], Test$category) # Test!
  perf <- performance(pred, "tpr", "fpr")
  plot(perf, lty=1, lwd=0.5, colorize=FALSE, add=TRUE)
}

# 各アルゴリズムのAUCを一括取得-----------------------------

auc <- numeric(63)
for(i in 12:63){
  pred <- prediction(Test[,i], Test$category) # Test!
  auc[i] <- as.numeric(performance(pred, "auc")@y.values)
}

auc.names <- names(Test)
auc.df <- data.frame(algorithm=auc.names, auc=auc)

# 決定木　===============================================================

set.seed(123)
Tree = rpart(category ~ aa_score+position+blosum+gnomad, 
             data=Train, method="class", 
             minsplit = 30, 
             minbucket = 10, 
             cp = 0.01, 
             maxdepth = 4, 
             xval = 10)

prp(Tree)

drawing_matrix(Tree, class)
plotting_roc(Tree, prob)

# Randomforest =========================================================

set.seed(123)
Forest <- randomForest(category ~ aa_score+position+blosum+gnomad, data=Train, ntree=500)

varImpPlot(Forest,pch=19, main=NULL)
plotting_roc(Forest, prob)

# ランダムフォレストにおける各遺伝子のauc==============================

genename <- c("ATM", "BRCA1", "BRCA2","MSH2", "MSH6", "PTCH1", "TP53", "TSC1", "TSC2")
auc_test_genes <- rep(0, length(genename)) 
set.seed(1)

for (i in seq_along(genename)) {
  data_gene <- data[data$genename == genename[i], ]
  split = sample.split(data_gene$category, SplitRatio = 0.75)
  Train = subset(data_gene, split == TRUE)
  Test = subset(data_gene, split == FALSE)
  Forest <- randomForest(category ~ gnomad+blosum+position+ref_character, data=Train, ntree=500)
  predictTest = predict(Forest, type="prob", newdata=Test)
  pred_test <- prediction(predictTest[,2], Test$category)
  auc_test_genes[i] <- as.numeric(performance(pred_test, "auc")@y.values)
}

auc_test_genes

# 各遺伝子の変数重要度 =================================================

genename <- c("ATM", "BRCA1", "BRCA2",  "MSH2", "MSH6", "PTCH1", "TP53", "TSC1", "TSC2")

set.seed(1)
dev.off()
par(mfrow= c(3,3)) 
for (i in seq_along(genename)) {
  data_gene <- data[data$genename == genename[i], ]
  split = sample.split(data_gene$category, SplitRatio = 0.75)
  Train = subset(data_gene, split == TRUE)
  Test = subset(data_gene, split == FALSE)
  Forest <- randomForest(category ~ position+aa_score+blosum+gnomad, data=Train, ntree=500)
  varImpPlot(Forest,pch=19, cex.axis=2, main=genename[i])
}

#　過学習シミュレーション　==========================

plot_fitting <- function(n,sd) {
  x <- seq(0, 2 * pi, length.out = n)
  set.seed(144); y <- sin(x) + rnorm(n, 0, sd)
  df <- data.frame(x = x, y = y)
  plot(x, y, pch = 1, ylim = c(-2, 2), col = "black", xlab = "x", ylab = "y")
  
  # 回帰曲線の描画
  for (i in 1:3) {
    model <- paste0("p",i)
    # 動的にモデルを作成
    if (model == "p1") {
      fit <- lm(y ~ x, data = df)
    } else if (model == "p2") {
      fit <- lm(y ~ x + I(x^2) + I(x^3), data = df)
    } else if (model == "p3") {
      fit <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + 
                  I(x^8) + I(x^9) + I(x^10), data = df)
    }  else {
      stop("Invalid model specified.")
    }
    
    #print(summary(fit)$r.squared) # Multiple R-squared 1-(残差平方和/全平方和)
    predictions <- predict(fit)
    mse <- sqrt(mean((y - predictions)^2))
    print(mse) #平均二乗誤差平方根
    
    colors = c("brown", "blue", "red")
    lines(x, predict(fit, newdata = data.frame(x = x)), col = colors[i], lwd = 3)
  } 
}

# 使用例
plot_fitting(20, 0.5) # データ数，標準偏差

