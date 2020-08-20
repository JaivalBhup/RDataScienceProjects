library(dslabs)
library(tidyverse)
data("movielens")
library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
#predicting by just the mean y_u_i = mu 
#fit <- lm(rating ~ as.factor(userId), data = movielens)
mu_hat <- mean(train_set$rating)
naive_rmse<-RMSE(test_set$rating,mu_hat)
#adding to result table
rmse_results <- data.frame(method= "Just the Average", RMSE = naive_rmse)
# prediction with bias which is the average rating for each movie y_u_i = mu + b_i
mu <- mean(train_set$rating)
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predict_ratings <- mu + test_set%>%
  left_join(movie_avgs, by = "movieId")%>%
  .$b_i

model_1_rmse <- RMSE(test_set$rating, predict_ratings)
rmse_results <- bind_rows(rmse_results,
                          data.frame(method ="Movie Effect Model",
                          RMSE = model_1_rmse))
rmse_results%>%knitr::kable()

# lm(rating ~ as.factor(movieId) + as.factor(userId))
# adding user specific effect

user_avgs <- test_set%>%
  left_join(movie_avgs, by = "movieId")%>%
  group_by(userId)%>%
  summarise(b_u = mean(rating - mu - b_i))

predict_ratings <- test_set%>%
  left_join(movie_avgs, by = "movieId")%>%
  left_join(user_avgs, by = "userId")%>%
  mutate(pred = mu + b_i + b_u)%>%.$pred

model_2_rmse <- RMSE(test_set$rating, predict_ratings)
rmse_results <- bind_rows(rmse_results,
                          data.frame(method ="User Specific Model",
                                     RMSE = model_2_rmse))
rmse_results%>%knitr::kable()
#regulariztion
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

predict_ratings <-test_set%>%
  left_join(movie_reg_avgs, by="movieId")%>%
  mutate(pred = mu + b_i)%>%.$pred
model_3_rmse <- RMSE(test_set$rating, predict_ratings)
rmse_results <- bind_rows(rmse_results,
                          data.frame(method = "Movie + User Effects Model",
                                     RMSE = model_3_rmse))
rmse_results%>%knitr::kable()
