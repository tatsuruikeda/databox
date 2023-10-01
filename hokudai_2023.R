library(pROC)
library(rpart)
library(rpart.plot)
library(randomForest)
library(neuralnet)
library(ape)
library(ggplot2)
library(ggrepel)
library(lattice)
library(caret)

# 線形回帰 =================================================

data <- read.csv("lm_data.csv")
X <- cbind(rep(1, nrow(data)), data[, 1]);y <- data[, 2]
theta <- rep(0, 2);num_iters <- 1500;alpha <- 0.01

#コスト関数
computeCost <- function(X, y, theta) {
  m <- length(y)
  J <- 0
  J <- 1/(2*m) * t(X %*% theta - y) %*% (X %*% theta - y)
  return(J)
}

#勾配降下法
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

# 実行
result <- gradientDescent(X, y, theta, alpha, num_iters)
# 結果表示
print(result$theta)

# ３D図および等高線プロット作図
theta0_vals <- seq(-10, 10, length.out = 100)
theta1_vals <- seq(-1, 4, length.out = 100)
J_vals <- matrix(0, nrow = length(theta0_vals), ncol = length(theta1_vals))

for (i in 1:length(theta0_vals)) {
  for (j in 1:length(theta1_vals)) {
    t <- c(theta0_vals[i], theta1_vals[j])
    J_vals[i, j] <- computeCost(X, y, t)
  }
}

# 3D
persp(theta0_vals, theta1_vals, J_vals, col = heat.colors(100)) 
# 等高線
levels <- 10^seq(-2, 3, length.out = 20)
contour(theta0_vals, theta1_vals, J_vals, levels=levels)

# lm関数
Lm <- lm(y ~ x, data=data)
summary(Lm)

plot(data$x, data$y, col = "red")
abline(Lm, lwd = 2, col = "blue")
x1 <- seq(0, 25, length=1000);y1 <- -3.36+1.13*x1
lines(x1, y1, lwd = 2, col="red")

# 正規方程式 octaveでは　theta = pinv(X' * X) * X' * y
theta <- solve(t(X) %*% X) %*% t(X) %*% y;theta

# ロジスティック回帰==========================================

Train = read.csv("Train.csv");Test = read.csv("Test.csv")
Train = Train[, -1];Test = Test[, -1]
Train$category <- as.factor(Train$category);Test$category <- as.factor(Test$category)

Log <- glm(category ~ ., data = Train, family=binomial)
summary(Log)

# 混同行列　train set
predictLogTrain = predict(Log, type="response", data=Train)
predicted <- factor(as.numeric(predictLogTrain > 0.5))
actual <- factor(Train$category)
confusionMatrix(predicted, actual, positive="1") 

# 混同行列　test set
predictLogTest = predict(Log, type="response", newdata=Test)
predicted <- factor(as.numeric(predictLogTest > 0.5))
actual <- factor(Test$category)
confusionMatrix(predicted, actual, positive="1") 

# ROC curve
par(mfrow= c(2,2)) 

Predict_test = predict(Log, type="response", newdata = Test) 

ROC1 <- roc(category ~ Predict_test, data = Test); plot(ROC1, main = "Logistic model")
ROC2 <- roc(category ~ vest4, Test);plot(ROC2, main = "VEST4")
ROC3 <- roc(category ~ primateai , Test);plot(ROC3, main = "PrimateAI")
ROC4 <- roc(category ~ fathmm, Test);plot(ROC4, main = "FATHMM")

dev.off()

ROC1$auc;ROC2$auc;ROC3$auc;ROC4$auc

# 決定木　===========================================================

Train = read.csv("Train.csv");Test = read.csv("Test.csv")
Train = Train[, -1];Test = Test[, -1]
Train$category <- as.factor(Train$category);Test$category <- as.factor(Test$category)

set.seed=1
tree = rpart(category ~ ., data=Train, method="class")
prp(tree)

# 混同行列　train set
predictCARTtrain <- predict(tree, data=Train, type="class")
predicted <- predictCARTtrain
actual <- factor(Train$category)
confusionMatrix(predicted, actual, positive="1") 

# 混同行列　train set
predictCARTtest <- predict(tree, newdata=Test, type="class")
predicted <- predictCARTtest
actual <- factor(Test$category)
confusionMatrix(predicted, actual, positive="1") 

# ROC 
Predict_tree_train = predict(tree, type="prob", data = Train) 
ROC <- roc(category ~ Predict_tree_train[,2], data = Train) 
ROC;plot(ROC)

Predict_tree_test = predict(tree, type="prob", newdata = Test)
ROC <- roc(category ~ Predict_tree_test[,2], data = Test) 
ROC;plot(ROC)

# Randomforest =========================================================
Train = read.csv("Train.csv");Test = read.csv("Test.csv")
Train = Train[, -1];Test = Test[, -1]
Train$category <- as.factor(Train$category);Test$category <- as.factor(Test$category)

set.seed=1
Forest <- randomForest(category ~ ., data=Train, ntree=1000)

varImpPlot(Forest,pch=19)

# 混同行列　train set
predictForesttrain <- predict(Forest, data=Train, type="class")
predicted <- predictForesttrain
actual <- factor(Train$category)
confusionMatrix(predicted, actual, positive="1") 

# 混同行列　test set
predictForesttest <- predict(Forest, newdata=Test, type="class")
predicted <- predictForesttest
actual <- factor(Test$category)
confusionMatrix(predicted, actual, positive="1") 

# ROC for RF 
Predict_Forest_train = predict(Forest, type="prob", data = Train) # type is crucial!
ROC <- roc(category ~ Predict_Forest_train[, 2], data = Train) 
ROC;plot(ROC)

Predict_Forest_test = predict(Forest, type="prob", newdata = Test)
ROC <- roc(category ~ Predict_Forest_test[, 2], data = Test) 
ROC;plot(ROC)

# Neuralnet ===========================================================
Train = read.csv("Train.csv");Test = read.csv("Test.csv")
Train = Train[, -1];Test = Test[, -1]

set.seed(123)

# model1
model <- neuralnet(category ~ ., data=Train, hidden=c(2, 1), threshold=0.04, linear.output=TRUE, stepmax=1e7)
plot(model, show.weights = FALSE, dimension=6, arrow.length = 0.1, information=TRUE, information.pos=0.1)

# 混同行列　train set
model_results_train<- predict(model, Train)
predicted <- factor(as.numeric((model_results_train > 0.5)))
actual <- factor(Train$category)
confusionMatrix(predicted, actual, positive="1") 

# 混同行列　test set
model_results_test <- predict(model, Test)
predicted <- factor(as.numeric((model_results_test > 0.5)))
actual <- factor(Test$category)
confusionMatrix(predicted, actual, positive="1") 

#model2
model <- neuralnet(category ~ ., data=Train, hidden=c(10, 8, 6, 4, 2), threshold=0.04, linear.output=TRUE, stepmax=1e7)
plot(model, show.weights = FALSE, dimension=6, arrow.length = 0.1, information=TRUE, information.pos=0.1)

# 混同行列　train set
model_results_train<- predict(model, Train)
predicted <- factor(as.numeric((model_results_train > 0.5)))
actual <- factor(Train$category)
confusionMatrix(predicted, actual, positive="1") 

# 混同行列　test set
model_results_test <- predict(model, Test)
predicted <- factor(as.numeric((model_results_test > 0.5)))
actual <- factor(Test$category)
confusionMatrix(predicted, actual, positive="1") 

# ROC 曲線　
par(mfrow= c(1,2)) 
suppressMessages(ROC_test <- roc(category ~ model_results_train, data = Train, plot=TRUE));print(ROC_test$auc)
suppressMessages(ROC_train <- roc(category ~ model_results_test, data = Test, plot=TRUE));print(ROC_train$auc)

dev.off()

#　過学習　分散とバイアスのトレードオフ ==========================
# set.seedを変更して3つの異なる散布図を生成
set.seed(1)
n <- 50
x <- seq(0, 2 * pi, length.out = n)
y1 <- sin(x) + rnorm(n, 0, 1)

set.seed(2)
y2 <- sin(x) + rnorm(n, 0, 1)

set.seed(3)
y3 <- sin(x) + rnorm(n, 0, 1)

# 散布図を描画
plot(x, y1, pch = 20, ylim = c(-1.5, 1.5), col = "black", xlab = "x", ylab = "y")
points(x, y2, pch = 20, col = "black")
points(x, y3, pch = 20, col = "black")

# 回帰曲線の描画
for (i in 1:3) {
  set.seed(i)
  n <- 50;x <- seq(0, 2 * pi, length.out = n);y <- sin(x) + rnorm(n, 0, 1)
  df <- data.frame(x = x, y = y)
  p1 <- lm(y ~ x, data = df)
  p2 <- lm(y ~ x + I(x^2), data = df)
  p3 <- lm(y ~ x + I(x^2) + I(x^3), data = df)
  p10 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = df)
  lines(x, predict(p10, newdata = data.frame(x = x)), col = "purple")　#p1〜p10まで変化させる
} # col ="red", "blue", "brown"など 


# 教師なし学習用データの散布図====================================

data <- read.csv("cluster_plot.csv") 
ggplot(data, aes(x, y)) +
  geom_point(size=3) +
  geom_label_repel(aes(label = label), size=4) 

# k-means　=======================================================

dat1 <- read.csv("cluster_dendrogram.csv") 
dat1 <- dat1[,-1]

k <- 2 
set.seed(1)
km.out <- kmeans(t(dat1),k)
km.out$cluster 

cluster_df <- data.frame(Cluster = km.out$cluster)

#　デンドログラム==================================================

dat1 <- read.csv("cluster_dendrogram.csv") 
dat1 <- dat1[,-1]

test.data <- t(dat1) # key sentence
hc <- hclust(dist(test.data), "ward.D2")

# デンドログラム描画
phylo_tree <- as.phylo(hc)
plot.phylo(phylo_tree, direction = "left", cex = 1.0, label.offset = 0.02)

