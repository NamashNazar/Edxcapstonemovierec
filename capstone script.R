if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

library(dplyr)
library(ggplot2)
library(caret)
library(knitr)
library(tinytex)
library(lubridate)
library(stringr)
library(gridExtra)

#See dimentions of dataset
dim(edx)
#See structure of dataset
str(edx)
#See first few rows of dataset
head(edx)

edx<- edx %>% mutate(year=str_sub(title,-5,-2), old=2020-as.numeric(year))
head(edx)

# Histogram of count of different ratings. 
a<-ggplot(data=edx, aes(edx$rating))+geom_histogram()+ggtitle("Count of each rating")

# Histogram of count of ratings given each year using the timestamp variable. the time stamp is used to extract date and then year column
b<-edx %>% mutate(date= as_datetime(timestamp),
                  year=as.numeric(format(date,'%Y'))) %>% ggplot(aes(year))+geom_histogram()+ggtitle("Number of ratings given each year by users")

# Histogram of the year of age of movies by number of ratings
c<-ggplot(data=edx, aes(edx$old))+geom_histogram()+ggtitle("Number of ratings received by movies by their age")

# Arranging all graphs in one grid
grid.arrange(a,b,c, nrow=3)

# Set seed before creating partition to get consistent result each time code is evaluated
set.seed(755)

# create partition and save index number of subsets in test_index
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, 
                                  list = FALSE)

#Use test_index to split edx into training and test set
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

#To make sure we don’t include users and movies in the test set that do not appear in the training set, we remove these entries using the semi_join function
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Have a look at the two dataset to ensure all the variable names, columns, data are as expected.
head(test_set)
head(train_set)

# Find the mean rating in the training set

mu_hat <- mean(train_set$rating)

#Apply the mean rating as predicted value for all ratings and compare it to actual ratings in test set to find RMSE
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

# Find the mean rating in the training set
mu <- mean(train_set$rating) 

# Compute least squared estimate using mean of difference of actual rating and average rating
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# Calculate Predicted value using least squared estimate of movie effect
predicted_ratings_movie <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

# Calculate RMSE
movie_effect<-RMSE(predicted_ratings_movie, test_set$rating)

movie_effect

# Find the mean rating in the training set
mu <- mean(train_set$rating) 

# Compute least squared estimate using mean of difference of actual rating, average rating and estimated movie effect
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Calculate Predicted value using least squared estimate of user and movie effects
predicted_ratings_usermovie <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Calculate RMSE
movieuser_effect<-RMSE(predicted_ratings_usermovie, test_set$rating)

movieuser_effect

# Find the mean rating in the training set
mu <- mean(train_set$rating) 

# Compute least squared estimate using mean of difference of actual rating, average rating, estimated movie and user effect
year_avgs <- train_set %>% 
  left_join(user_avgs, by='userId') %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(old) %>% 
  summarize(b_t = mean(rating - mu- b_i - b_u))

# Calculate Predicted value using least squared estimate of user, movie, and time effects
predicted_ratings_umt<-test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs, by='old')%>%
  mutate(pred = mu + b_i + b_u + b_t) %>%
  pull(pred)

# Calculate RMSE
movieusertime_effect<-RMSE(predicted_ratings_umt, test_set$rating)

movieusertime_effect

# Find the mean rating in the training set
mu <- mean(train_set$rating) 

# Compute least squared estimate using mean of difference of actual rating, average rating, estimated movie, user, and time effects
genre_avgs <- train_set %>% 
  left_join(user_avgs, by='userId') %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(year_avgs, by='old') %>%
  group_by(genres) %>% 
  summarize(b_g = mean(rating - mu- b_i - b_u - b_t))

# Calculate Predicted value using least squared estimate of user, movie, time and genre effects
predicted_ratings_umtg<-test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs, by='old')%>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_t) %>%
  pull(pred)

# Calculate RMSE
movieusertimegenre_effect<-RMSE(predicted_ratings_umtg, test_set$rating)

movieusertimegenre_effect


# Set a range of lamba values to find the optimal penalty terms
lambdas <- seq(0, 10, 1)

# Function to find RMSE values using different lamda values
rmses <- sapply(lambdas, function(l){
  
  # Find the mean rating in the training set
  mu <- mean(train_set$rating)
  
  # Compute least squared estimate of estimated movie, user, time, genre effects
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_t <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(old) %>%
    summarize(b_t = sum(rating - b_i - b_u - mu)/(n()+l))
  
  b_g <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_t, by="old") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - b_t - mu)/(n()+l))
  
  # Calculate Predicted value using least squared estimate of user, movie, time and genre effects
  predicted_ratings_regmutg <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_t, by = "old") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_t + b_g) %>%
    pull(pred)
  
  # Calculate RMSE
  return(RMSE(predicted_ratings_regmutg, test_set$rating))
})

# Find optimal lamda
minlambda <- lambdas[which.min(rmses)]

# Find min RMSE
regmovusertimegenre<-min(rmses)

regmovusertimegenre

# Set a range of lamba values to find the optimal penalty terms
lambdas <- seq(0, 10, 0.25)

# Find the mean rating in the training set
mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

# Function to find RMSE values using different lamda values
rmses <- sapply(lambdas, function(l){
  predicted_ratings_regmov <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  
  # Calculate RMSE
  return(RMSE(predicted_ratings_regmov, test_set$rating))
})

# Find optimal lamda
lambda <- lambdas[which.min(rmses)]

# Find min RMSE
regmov<- min(rmses)

regmov

# Set a range of lamba values to find the optimal penalty terms
lambdas <- seq(0, 10, 0.25)

# Function to find RMSE values using different lamda values
rmses <- sapply(lambdas, function(l){
  
  # Find the mean rating in the training set
  mu <- mean(train_set$rating)
  
  # Compute least squared estimate of estimated movie and user effects
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  # Calculate Predicted value using least squared estimate of user and movie effects
  predicted_ratings_regmovuser <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  # Calculate RMSE
  return(RMSE(predicted_ratings_regmovuser, test_set$rating))
})
# Find optimal lamda
lambda <- lambdas[which.min(rmses)]

# Find min RMSE
regmovuser<-min(rmses)

regmovuser

# Set a range of lamba values to find the optimal penalty terms
lambdas <- seq(0, 10, 1)

# Function to find RMSE values using different lamda values
rmses <- sapply(lambdas, function(l){
  
  # Find the mean rating in the training set
  mu <- mean(train_set$rating)
  
  # Compute least squared estimate of estimated movie, user, and time effects
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_t <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(old) %>%
    summarize(b_t = sum(rating - b_i - b_u - mu)/(n()+l))
  
  # Calculate Predicted value using least squared estimate of user, movie and time effects
  predicted_ratings_regmut <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_t, by = "old") %>%
    mutate(pred = mu + b_i + b_u + b_t) %>%
    pull(pred)
  
  # Calculate RMSE
  return(RMSE(predicted_ratings_regmut, test_set$rating))
})

# Find optimal lamda
lambda <- lambdas[which.min(rmses)]

# Find min RMSE
regmovusertime<-min(rmses)

regmovusertime


# Make a table summarizing all results
rmse_results <- tibble(method = c("Just the average","Movie effect","User+ Movie effect","User+Movie+Time effect","User+Movie+Time+genre effect", "Regularized Movie Effect Model","Regularized Movie + User Effect Model","Regularized Movie + User + time Effect Model", "Regularized Movie + User +time + genre Effect Model"), RMSE = c(naive_rmse,movie_effect,movieuser_effect, movieusertime_effect, movieusertimegenre_effect, regmov,regmovuser,regmovusertime, regmovusertimegenre))
rmse_results

# Find the mean rating in the edx set
mu_final <- mean(edx$rating)

# Compute least squared estimate of estimated movie, user, time, genre effects. The minlambda from best performing model on edx training and test dataset is used.
b_i_final <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i_final = sum(rating - mu_final)/(n()+minlambda))

b_u_final <- edx %>% 
  left_join(b_i_final, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u_final = sum(rating - b_i_final - mu_final)/(n()+minlambda))

b_t_final <- edx %>% 
  left_join(b_i_final, by="movieId") %>%
  left_join(b_u_final, by="userId") %>%
  group_by(old) %>%
  summarize(b_t_final = sum(rating - b_i_final - b_u_final - mu_final)/(n()+minlambda))

b_g_final <- edx %>% 
  left_join(b_i_final, by="movieId") %>%
  left_join(b_u_final, by="userId") %>%
  left_join(b_t_final, by="old") %>%
  group_by(genres) %>%
  summarize(b_g_final = sum(rating - b_i_final - b_u_final - b_t_final - mu_final)/(n()+minlambda))

# Add columns of 'year' and 'old' in validation dataset
validation<- validation %>% mutate(year=str_sub(title,-5,-2), old=2020-as.numeric(year))

# Calculate Predicted value using least squared estimate of user, movie and time effects
predicted_ratings_val <- 
  validation %>% 
  left_join(b_i_final, by = "movieId") %>%
  left_join(b_u_final, by = "userId") %>%
  left_join(b_t_final, by = "old") %>%
  left_join(b_g_final, by = "genres") %>%
  mutate(pred = mu_final + b_i_final + b_u_final + b_t_final + b_g_final) %>%
  pull(pred)

# Calculate RMSE
RMSE(predicted_ratings_val, validation$rating)
