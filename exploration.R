data<-read.csv("data.csv")
head(data)

library(rpart)
library(tidyverse)
library(mosaic)

train <- filter(data, shot_made_flag == 0, shot_made_flag == 1)
head(train)
View(train)
test <- filter(data, shot_made_flag !=0, shot_made_flag != 1)

View(train)

 model_formula <- as.formula(Spe ~ Sepal.Length + Sepal.Width)
tree_parameters <- rpart.control(maxdepth = 3)
model_CART <- rpart(model_formula, data = iris, control=tree_parameters)


# Plot
plot(model_CART, margin=0.25)
text(model_CART, use.n = TRUE)
title("Predicting iris species using sepal length & width")
box()

p_hat_matrix <- model_CART %>% 
  predict(type = "prob", newdata = iris)

# Look at a random sample of 5 of them
p_hat_matrix %>% 
  as_tibble() %>% 
  sample_n(5)

MLmetrics::MultiLogLoss(y_true = iris$Species, y_pred = p_hat_matrix)

y_hat <- model_CART %>% 
  predict(newdata=iris, type="class")

# Score/error
MLmetrics::Accuracy(y_true = iris$Species, y_pred = y_hat)
MLmetrics::ConfusionMatrix(y_true = iris$Species, y_pred = y_hat)

