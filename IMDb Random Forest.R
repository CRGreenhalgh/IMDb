#########################
## Random Forest Model ##
#########################

## Libraries
library(tidyverse)
library(caret)
library(e1071)
library(ranger)

## Read in the data
imdb <- read_csv("IMDbClean.csv")

## Split into Test and Train
imdb.train <- imdb %>% filter(!is.na(imdb_score))
imdb.test <- imdb %>% filter(is.na(imdb_score))

## Center and Scaling
trans.cs <- preProcess(x=imdb %>% select(-imdb_score), method=c("center", "scale"))
imdb.cs <- predict(trans.cs, newdata=imdb)
trans01 <- preProcess(x=imdb %>% select(-imdb_score), method="range",
                      rangeBounds=c(0,1))
imdb.01 <- predict(trans01, newdata=imdb)

## Fit Random Forest Model
tictoc::tic()
imdb_rf <- train(form = imdb_score~.,
                 data=(imdb.train %>% select(-Set, -movie_title)),
                 method = "ranger",
                 trControl=trainControl(method="repeatedcv",
                                        number=10, #Number of pieces of your data
                                        repeats=3) #repeats=1 = "cv"
)
tictoc::toc()

tictoc::tic()
imdb_rf <- ranger(formula = imdb_score~.,
                  data=(imdb.train %>% select(-Set, -movie_title)),
                  mtry = 4,
                  min.node.size = 5
)
tictoc::toc()
plot(imdb_rf)
imdb_rf_preds1 <- data.frame(Id=imdb.test$movie_title, Predicted=predict(imdb_rf, newdata=imdb.test))

write_csv(x=imdb_rf_preds1, path="./RandomForestPrediction1Connor")
