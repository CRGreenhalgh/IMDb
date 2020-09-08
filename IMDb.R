##
## Code to analyze IMDb 
##

## Libraries I need
library(tidyverse)


## Read in data
imdb.train <- read_csv("./IMDBTrain.csv")
imdb.test <- read_csv("./IMDBTest.csv")

## Merege the two datasets together so when I clean the 
## training dataset I also treat the test set the same way
names(imdb.test)[names(imdb.test)=="Id"] <- "movie_title"
imdb <- bind_rows(train=imdb.train, test=imdb.test, .id = "Set")

####################################
## Some Exploratory Data Analysis ##
####################################

## Overall summary of the data
summary(imdb)

## Scatterplot of Budget vs Score
ggplot(data = imdb, mapping = aes(x = budget, y=imdb_score)) +
  geom_point()

#####################
## Stuff with Matt ##
#####################

## Create df with just the variables we need
imdb_temp <- imdb %>% select(c(movie_title, actor_1_name, actor_2_name, actor_3_name, actor_1_facebook_likes, actor_2_facebook_likes, actor_3_facebook_likes, cast_total_facebook_likes, movie_facebook_likes))

## Create top actors
#top_actors <- imdb[which(table(imdb$actor_1_name) > 20),]$actor_1_name
top_actors_index <- which(table(imdb$actor_1_name) > 20)
top_actors <- sort(unique(imdb$actor_1_name))[top_actors_index]

imdb_temp$top_actor <- as.numeric(0)
for(i in 1:nrow(imdb)) {
  if (imdb_temp[i,]$actor_1_name %in% top_actors) {
    imdb_temp$top_actor[i] <- 1 + imdb_temp$top_actor[i]
  } 
}

for(i in 1:nrow(imdb)) {
  if (imdb_temp[i,]$actor_2_name %in% top_actors) {
    imdb_temp$top_actor[i] <- 1 + imdb_temp$top_actor[i]
  } 
}

for(i in 1:nrow(imdb)) {
  if (imdb_temp[i,]$actor_3_name %in% top_actors) {
    imdb_temp$top_actor[i] <- 1 + imdb_temp$top_actor[i]
  } 
}

## Popularity
# Clean up columns first
for(i in 1:dim(imdb)[1]) {
  if(is.na(imdb[i, "actor_1_name"])) {imdb[i, "actor_1_name"] = "None"}
  if(is.na(imdb[i, "actor_2_name"])) {imdb[i, "actor_2_name"] = "None"}
  if(is.na(imdb[i, "actor_3_name"])) {imdb[i, "actor_3_name"] = "None"}
  if(is.na(imdb[i, "actor_1_facebook_likes"])) {imdb[i, "actor_1_facebook_likes"] = 0}
  if(is.na(imdb[i, "actor_2_facebook_likes"])) {imdb[i, "actor_2_facebook_likes"] = 0}
  if(is.na(imdb[i, "actor_3_facebook_likes"])) {imdb[i, "actor_3_facebook_likes"] = 0}
}
## For each Actor column, extract just the name and FB likes. Change the COL names so they can be combined.
actor1_likes <- imdb[, c("actor_1_facebook_likes", "actor_1_name")] %>% distinct_all() #1418 distinct actor 1s
colnames(actor1_likes) <- c("likes", "actor_name")
actor2_likes <- imdb[, c("actor_2_facebook_likes", "actor_2_name")] %>% distinct_all() #2108 distinct actor 2s
colnames(actor2_likes) <- c("likes", "actor_name")
actor3_likes <- imdb[, c("actor_3_facebook_likes", "actor_3_name")] %>% distinct_all() #2488 distinct actor 1s
colnames(actor3_likes) <- c("likes", "actor_name")
# Combine all three sets and remove duplicates. This 'popularity' set is all actors and their FB likes.
popularity <- bind_rows(actor1_likes, actor2_likes, actor3_likes) %>% distinct_all()
# Subset 'popularity' by x%. There are 4315 distinct actors in the data, so the top x% should be...
percent <- .01
popularity <- popularity[order(-popularity$likes), ]
popularity <- popularity[1:(ceiling(dim(popularity)[1]*percent)),]
pop_actors <- popularity$actor_name
# These names and likes correspond to cast total likes pretty well, so we could probably scrap that column
# Create an indicator variable for 'popular actor'
for(i in 1:dim(popularity)[1]){
  if(imdb[i, "actor_1_name"] %in% pop_actors | imdb[i, "actor_2_name"] %in% pop_actors |
     imdb[i, "actor_3_name"] %in% pop_actors) {
    imdb[i, "popular_actor"] <- 1
  }
  else {
    imdb[i, "popular_actor"] <- 0
  }
}

