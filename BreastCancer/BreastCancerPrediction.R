options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)
dim(brca$x)[1]
dim(brca$x)[2]
mean(brca$y == "M")
which.max(colMeans(brca$x))
which.min(colSds(brca$x))
sweeped<-sweep(brca$x,2,colMeans(brca$x),"-")
sweeped<-sweep(brca$x,2,colSds(brca$x),"/")
sd(sweeped[,1])
median(sweeped[,1])
d_sample <- as.matrix(dist(sweeped))
mean(d_sample[1,1:357])
mean(d_sample[1,358:569])
#or this method
#d_samples <- dist(sweeped)
#dist_BtoB <- as.matrix(d_sample)[1, brca$y == "B"]
#mean(dist_BtoB[2:length(dist_BtoB)])
#dist_BtoM <- as.matrix(d_sample)[1, brca$y == "M"]
#mean(dist_BtoM)
d_features <- dist(t(sweeped))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

clusters <- hclust(d_features)
tree <- cutree(clusters , 5)
split(names(tree), tree)

set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- sweeped[test_index,]
test_y <- brca$y[test_index]
train_x <- sweeped[-test_index,]
train_y <- brca$y[-test_index]

predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

k<- kmeans(train_x,2)
pred <- predict_kmeans(test_x, k)
pred<- ifelse(pred == 1, "B", "M")%>%factor()
sensitivity(factor(pred), test_y, positive = "B")
sensitivity(factor(pred), test_y, positive = "M")

#glm
train_glm <- train(train_x, train_y,
                   method = "glm")
glm_preds <- predict(train_glm, test_x)
mean(glm_preds == test_y)

#lda qda
train_lda <- train(train_x,train_y,method = "lda")
lda_preds <- predict(train_lda, test_x)

train_qda <- train(train_x,train_y,method = "qda")
qda_preds <- predict(train_qda, test_x)
mean(lda_preds==test_y)
#loess
train_loess <- train(train_x,train_y,method = "gamLoess")
loess_preds <- predict(train_loess, test_x)
mean(loess_preds==test_y)

#knn
set.seed(7, sample.kind = "Rounding")
kVals <- seq(3,21,2)
train_knn <- train(train_x,train_y, method = "knn",
                   tuneGrid = data.frame(k=kVals))
train_knn$bestTune
knn_pred <- predict(train_knn,test_x,type = "raw")
mean(knn_pred==test_y)

#Random Forest Model
set.seed(9, sample.kind = "Rounding")
mTryVals<-c(3,5,7,9)
train_rf <- train(train_x,train_y, method = "rf",
                  tuneGrid = data.frame(mtry = mTryVals),
                  importance = TRUE)
train_rf$bestTune
rf_pred <- predict(train_rf, test_x)
mean(rf_pred==test_y)
varImp(train_rf)

# ensemble
models <- c("glm","lda","qda","gamLoess","knn","rf")

fit_list <- lapply(models, function(model){
  print(model)
  train(train_x, train_y, method = model)
})
names(fit_list)=models
predList <- sapply(fit_list, function(fit){
  predict(fit, test_x)
})
y_hat_ensemble <- ifelse(rowMeans(predList=="B")>0.5,"B","M")%>%factor()
mean(y_hat_ensemble==test_y)

mean(predList[,6]==test_y)

ensemble <- cbind("K-means" = pred=="B", "GLM" = glm_preds =="B", "LDA" = lda_preds=="B",
              "QDA" = qda_preds=="B", "Loess" = loess_preds=="B",
              "KNN" = knn_pred=="B", "RF" = rf_pred=="B")
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_preds == test_y)
