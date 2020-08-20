library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)
set.seed(42, sample.kind = "Rounding")
index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2,list = FALSE)
train_set <- titanic_clean[-index,]
test_set <- titanic_clean[index,]

#2
set.seed(3, sample.kind = "Rounding")
guess_y_hat <- sample(c(0,1),length(index), replace = T)%>%factor(levels = levels(test_set$Survived))
confusionMatrix(guess_y_hat, test_set$Survived)

#3a
train_set%>%
  group_by(Sex)%>%
  summarise(prop = mean(Survived==1))

#3b
sex_model <- ifelse(test_set$Sex == "female", 1, 0)    # predict Survived=1 if female, 0 if male
mean(sex_model == test_set$Survived)    # calculate accuracy

#4a
train_set%>% 
  group_by(Pclass)%>%
  summarise(likelyToLive = mean(Survived==1))
#4b
fit<- glm(Survived ~ Pclass, data = test_set, family = "binomial")
p_hat<-predict(fit, test_set, type = "response")
y_hat <- ifelse(p_hat>0.5,1,0)%>%factor(levels = levels(test_set$Survived))
confusionMatrix(y_hat, test_set$Survived)
# or a simpler version
class_model <- ifelse(test_set$Pclass == 1, 1, 0)    # predict survival only if first class
mean(class_model == test_set$Survived)    # calculate accuracy

#4c
train_set%>%
  group_by(Sex,Pclass)%>% 
  summarise(likelyToLive = mean(Survived==1))
#4d
fit<- glm(Survived ~ Pclass+Sex, data = test_set, family = "binomial")
p_hat<-predict(fit, test_set, type = "response")
y_hat <- ifelse(p_hat>0.5,1,0)%>%factor(levels = levels(test_set$Survived))
confusionMatrix(y_hat, test_set$Survived)
#or a simpler version
sex_class_model <- ifelse(test_set$Sex == "female" & test_set$Pclass != 3, 1, 0)
mean(sex_class_model == test_set$Survived)
# 5a, 5b
confusionMatrix(sex_model%>%factor(levels = levels(test_set$Survived)),test_set$Survived)
confusionMatrix(class_model%>%factor(levels = levels(test_set$Survived)),test_set$Survived)
confusionMatrix(sex_class_model%>%factor(levels = levels(test_set$Survived)),test_set$Survived)

#6
F_meas(data = factor(sex_model), reference = test_set$Survived)
F_meas(data = factor(class_model), reference = test_set$Survived)
F_meas(data = factor(sex_class_model), reference = test_set$Survived)

#7
set.seed(1, sample.kind = "Rounding")
fit_lda <- train(Survived~Fare, method = "lda", data = train_set)
y_hat_lda <- predict(fit_lda, test_set)
confusionMatrix(y_hat_lda, test_set$Survived)

fit_lqda <- train(Survived~Fare, method = "qda", data = train_set)
y_hat_qda <- predict(fit_lda, test_set)
confusionMatrix(y_hat_qda, test_set$Survived)
#8
set.seed(1, sample.kind = "Rounding")
fit_glm_1 <- train(Survived~Age,method = "glm", data = train_set)
y_hat_glm_1 <- predict(fit_glm_1, test_set)
confusionMatrix(y_hat_glm_1, test_set$Survived)

fit_glm_2 <- train(Survived~Age+Sex+Pclass+Fare,method = "glm", data = train_set)
y_hat_glm_2 <- predict(fit_glm_2, test_set)
confusionMatrix(y_hat_glm_2, test_set$Survived)

fit_glm_3 <- train(Survived~.,method = "glm", data = train_set)
y_hat_glm_3 <- predict(fit_glm_3, test_set)
confusionMatrix(y_hat_glm_3, test_set$Survived)

#9a,b,c
set.seed(6 , sample.kind = "Rounding")
fit_knn<- train(Survived~.,data = train_set, method = "knn",
            tuneGrid = data.frame(k = seq(3, 51, 2)))
ggplot(fit_knn)
fit_knn$bestTune
y_hat_knn <- predict(fit_knn, test_set)
mean(y_hat_knn==test_set$Survived)
# 10
set.seed(8 , sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = 0.1)
fit_knn_cv <- train(Survived ~., data = train_set,
                    method ="knn",
                    trControl = control,
                    tuneGrid = data.frame(k = seq(3, 51, 2)))
ggplot(fit_knn_cv)
y_hat_knn_cv <- predict(fit_knn_cv, test_set)
mean(y_hat_knn_cv==test_set$Survived)

#11a,b
set.seed(10)
fit_rpart <- train(Survived~., data = train_set,
                   method = "rpart",
                   tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
fit_rpart$bestTune
y_hat_rpart <- predict(fit_rpart, test_set)
confusionMatrix(y_hat_rpart,test_set$Survived)
plot(fit_rpart$finalModel, margin = 0.1)
text(fit_rpart$finalModel)
#12
set.seed(14, sample.kind = "Rounding")
fit_rf <- train(Survived~.,
                data = train_set,
                method = "rf",
                tuneGrid = data.frame(mtry = seq(1,7,1)),
                ntree = 100)
fit_rf$bestTune
y_hat_rf <- predict(fit_rf, test_set)
mean(y_hat_rf == test_set$Survived)
varImp(fit_rf)
