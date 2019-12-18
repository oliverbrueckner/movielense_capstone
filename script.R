



if(!exists("edx")){
################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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
}
####################################
# START of my Code
####################################

###
# Inspecting the Data
###

edx_meta<-data.frame(number_of_movies=n_distinct(edx$movieId),number_of_users=n_distinct(edx$userId),number_of_ratings=nrow(edx),average_rating=mean(edx$rating),standard_deviation=sd(edx$rating))

# How many different movies are in the edx dataset?
edx_meta$number_of_movies

# How many different users are in the edx dataset?
edx_meta$number_of_users

# What proportion of the cells are empty?
edx_meta$number_of_ratings/(edx_meta$number_of_movies*edx_meta$number_of_users)

# What is the Distribution of Ratings?
rating_count<-edx%>%dplyr::count(rating)
rating_count%>%
  ggplot(aes(rating, n)) + 
  geom_point()+
  scale_y_log10() +
  ggtitle("Ratings")
# What ist the Distribution of Ratings per Movie?
movie_count<-edx %>% 
  dplyr::count(movieId) 
movie_count%>%
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

# What ist the Distribution of Ratings per User?
user_count<-edx %>%
  dplyr::count(userId) 
user_count%>%
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

# What is the average Rating and its Standard Deviation


###
# Naive Aproach: Guessing allways the Average Rating
###
mu_hat<-mean(mean(edx$rating))
RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
}
naive_rmse <- RMSE(validation$rating, mu_hat)
rmse_results <- data.frame(method = "Just the average", RMSE = naive_rmse)

###
# Model 1: Push the Average Rating in the Direction of the Movie-Average
###
mu<-mean(edx$rating)
movie_avgs <- edx %>% 
     group_by(movieId) %>% 
     summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + validation %>% 
    left_join(movie_avgs, by='movieId') %>%
    .$b_i
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
    data_frame(method="Movie Effect Model",
    RMSE = model_1_rmse ))

###
# Model 2: Encounter the User-Signals
###
user_avgs <- edx %>% 
    left_join(movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- validation %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
    data_frame(method="Movie + User Effects Model",  
    RMSE = model_2_rmse ))

###
# Use Regularisation to avoid too large effects from large small sample sizes
###
lambdas <- seq(4, 6, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

save(movie_count, user_count,rating_count, rmse_results,edx_meta, lambdas, rmses, file = "mydata.rda")
