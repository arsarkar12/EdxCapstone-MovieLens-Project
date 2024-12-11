## HarvardX: PH125.9x - Capstone MovieLens Project 
## https://github.com/arsarkar12/EdxCapstone-MovieLens-Project

#################################################
# MovieLens Rating Prediction Project Code 
################################################

##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#############################################################
#### Explore DataSet, Data Analysis and Generate Plots ####
#############################################################

#Save edx and validation files
save(edx, file="edx.RData")
save(final_holdout_test, file = "final_holdout_test.RData")

#Install Additional packages as needed.

suppressWarnings({
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
  
if (!requireNamespace("tinytex", quietly = TRUE)) {
  install.packages("tinytex")
}
# Install TinyTeX    #Require to generate PDF output from rmarkdown (rmd) file.
if (!tinytex::is_tinytex()) {
  tinytex::install_tinytex()
}
})

# load library
library(tidyverse)
library(ggplot2)
library(dplyr)
library(rmarkdown)
library(knitr)
library(caret)
### Exploratory analysis
  
# explore dataset
str(edx)
str(final_holdout_test)

# Verify data frame using head()
head(edx) %>%
  print.data.frame()

# lookout for any NA value in the column before analysis 
colSums(is.na(edx)) 

#above confirms there are no blank/NA values in the dataset

# show summary of the dataset 
summary(edx)
summary(final_holdout_test)

# Number of unique movies and users in the edx dataset 
edx %>%
  summarize(unique_users = n_distinct(userId), 
            unique_movies = n_distinct(movieId)) %>%
			print.data.frame()

# Count Movies Rated Only Once
edx %>%
  group_by(movieId) %>%
  summarize(count = n()) %>%
  filter(count == 1) %>%
  nrow()

#Exclude Movies Rated Only Once
movies_to_exclude <- edx %>%
  group_by(movieId) %>%
  summarize(count = n(), .groups = "drop") %>%
  filter(count == 1) %>%
  pull(movieId)

# Filter out these movies from the dataset
edx <- edx %>% filter(!movieId %in% movies_to_exclude)

#Add Review Year column to the dataset
edx <- edx %>%
  mutate(review_date = round_date(as_datetime(timestamp), unit = "week"))
 
head(edx)  %>%
  print.data.frame()

# Number of unique movies and users in the edx dataset after cleanup
edx %>%
  summarize(unique_users = n_distinct(userId), 
            unique_movies = n_distinct(movieId)) %>%
  print.data.frame()
			
# show top 3 most ratings sorted by their frequency
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(3) %>%
  arrange(desc(count)) 
			
# Ratings distribution
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "black" , fill= "light blue") +
  scale_x_continuous(breaks = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 1000000))) +
  ggtitle("Distribution of Movie Ratings")
  
# Plot number of ratings per movie
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 25, color = "black" , fill= "light blue") +
  scale_x_log10() +
  xlab("# of Ratings") +
  ylab("# of Movies") +
  ggtitle("Count of Ratings per Movie")


# Plot count of ratings by users
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 20, color = "black", fill= "light blue") +
  scale_x_log10() +
  xlab("# of ratings") + 
  ylab("# of users") +
  ggtitle("Count of Ratings by Users")

# Plot mean movie ratings by users
edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 20, color = "black", fill= "light blue") +
  xlab("Mean Rating") +
  ylab("# of Users") +
  ggtitle("Mean Ratings by Users") +
  scale_x_continuous(breaks = c(seq(1,5,0.5))) +
  theme_light()

# Plot average rating by date of review in the edx dataset
edx %>% group_by(review_date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(review_date, rating)) +
  geom_point() +
  geom_smooth() +
  xlab("Year of Review") +
  ylab("Average Rating") +
  ggtitle("Average Rating by Review Date") +
  theme_light()
  
#####################################
## Start Data Modelling ##
#####################################

#############################################################
# Methods - partition edx into train and test sets
#############################################################

set.seed(1)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_temp_set <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set 

test_set <- test_temp_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
  
# add the removed data to the train set using the anti_join function: 
removed_set <- anti_join(test_temp_set, test_set) 
train_set <- rbind(train_set, removed_set)

# Remove temporary files to tidy environment
rm(test_index, test_temp_set, removed_set)

#####################################################################################
# Methods - develop algorithm to train and test various models
#####################################################################################

#RMSE calculation Function 
#RMSE objective of the project: 0.86490

RMSE <- function(predicted_ratings, true_ratings){
  sqrt(mean((predicted_ratings - true_ratings)^2,na.rm = TRUE))
}

#creating a table to store target and computed RMSE results from all models

rmse_target <- 0.86490

#rmse_computed <- data.frame(Method = character(), RMSE = numeric(), stringsAsFactors = FALSE)
rmse_computed <- tibble(Method = character(), RMSE = numeric())

rmse_computed <- tibble(Method = "Target RMSE", RMSE = rmse_target)
rmse_computed %>% knitr::kable()

###########################
#First Model|Simple Average 
###########################

# Compute the dataset's mean rating
mu_simple_avg <- mean(train_set$rating)
mu_simple_avg 

simple_avg_rmse <- RMSE(test_set$rating, mu_simple_avg)
simple_avg_rmse

#rmse_computed <- data_frame(method = "Simple Average Model", RMSE = simple_avg_rmse)

rmse_computed <- bind_rows(rmse_computed,
                          tibble(Method = "Simple Average Model", RMSE = simple_avg_rmse)
)

# Display the table
rmse_computed %>% knitr::kable()

################################################
#Second model|Estimate for Movie Effect (b_i)
################################################

#Exploratory analysis reveals that count of rating is not same across all movies. 
#Some movies are rated more (or less) than others.
#Augment previous model by adding the term  b_i 

mu_movie_avg <- mean(train_set$rating) 
movie_avg <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu_movie_avg))
movie_avg

#plot variability in the estimate for movie effect
#movie_avg %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = "black", fill= "light blue")   #original code
movie_avg %>% ggplot(aes(b_i)) + 
  geom_histogram(bins = 10, color = "black", fill= "light blue")


#updated prediction 
predicted_ratings_b_i <- mu_movie_avg + test_set %>% 
  left_join(movie_avg, by='movieId') %>%
  pull(b_i)
  
movie_avg_rmse <- RMSE(predicted_ratings_b_i, test_set$rating)
movie_avg_rmse

rmse_computed <- bind_rows(rmse_computed,
                          tibble(Method="Movie Effect Model", RMSE = movie_avg_rmse))
                                     
rmse_computed %>% knitr::kable()

################################################
#Third model|Estimate for Movie+User Effect (b_u)
################################################

#Compute RMSE. Consider the user effects of the users who rated over 20 movies.

mu_user_avg <- mean(train_set$rating) 

# Estimate user effect (b_u)
user_avg <- train_set %>% 
  left_join(movie_avg, by="movieId") %>%
  group_by(userId) %>%
  filter(n()>=20) %>%            
  summarize(b_u = mean(rating - mu_user_avg - b_i))

# plot variability in the estimate considering user effect
user_avg %>% ggplot(aes(b_u)) + 
  geom_histogram(bins = 10, color = "black", fill= "light blue")

#updated prediction and computed rmse for this model

predicted_ratings_b_u <- test_set %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  mutate(pred = mu_user_avg + b_i + b_u) %>%
  pull(pred)

# Calculate RMSE based on user effects model
user_avg_rmse <- RMSE(predicted_ratings_b_u, test_set$rating)
user_avg_rmse

rmse_computed <- bind_rows(rmse_computed,
                           tibble(Method="Movie+User Effect Model", RMSE = user_avg_rmse))

rmse_computed %>% knitr::kable()

################################################
#Fourth model|Estimate for Movie+User+Genre effect (b_g)
################################################

mu_genre_avg <- mean(train_set$rating) 

# Estimate genre effect (b_g)
genre_avg <- train_set %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by="userId") %>%
  group_by(genres) %>%
  filter(n()>=1000) %>%     #where a particular Genre has at least 1000 count in rating
  summarise(b_g = mean(rating - mu_genre_avg - b_i - b_u, na.rm = TRUE), 
            .groups = "drop")   #ensure proper grouping behavior
#  summarise(b_g = mean(rating - mu_genre_avg - b_i - b_u))
genre_avg

#plot variability in the estimate considering Genre effect
genre_avg %>% ggplot(aes(b_g)) +
              geom_histogram(bins = 10, color = "black", fill= "light blue") 
              
#updated prediction and computed rmse for this model

predicted_ratings_b_g <- test_set %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  left_join(genre_avg, by = "genres") %>%
  mutate(pred = mu_genre_avg + b_i + b_u + b_g) %>%
  pull(pred)

# Calculate RMSE based on genre effects model
genre_avg_rmse <- RMSE(predicted_ratings_b_g, test_set$rating)
genre_avg_rmse

rmse_computed <- bind_rows(rmse_computed,
                  tibble(Method="Movie+User+Genre Effect Model", RMSE = genre_avg_rmse))
                                     
rmse_computed %>% knitr::kable()

#################################################################
#Fifth model|Estimate for Movie+Genre+User+Review date effect (b_y)
################################################################

mu_rev_date_avg <- mean(train_set$rating)

# Estimate Review Year effect (b_y)
rev_date_avg <- train_set %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  left_join(genre_avg, by="genres") %>%
  group_by(review_date) %>%
  summarise(b_y = mean(rating - mu_rev_date_avg - b_i - b_u - b_g, na.rm = TRUE), 
            .groups = "drop")    # Handle NA values during summarization
rev_date_avg

#plot variability in the estimate considering date of review effect
rev_date_avg %>% ggplot(aes(b_y)) +
  geom_histogram(bins = 10, color = "black", fill= "light blue")

#updated prediction and computed rmse for this model

predicted_ratings_b_y <- test_set %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(genre_avg, by = "genres") %>%
  left_join(user_avg, by="userId") %>%
  left_join(rev_date_avg, by = "review_date") %>%
  mutate(pred = mu_rev_date_avg + b_i + b_u + b_g + b_y) %>%
  pull(pred)

# Calculate RMSE based on review date effects model
rev_date_avg_rmse <- RMSE(predicted_ratings_b_y, test_set$rating)
rev_date_avg_rmse

rmse_computed <- bind_rows(rmse_computed,
                      tibble(Method="Movie + User + Genre + Review Date Effect Model",RMSE = rev_date_avg_rmse))
                                     
rmse_computed %>% knitr::kable()

#############################################################
# Use Regularization principles to improve on the final model
#############################################################

# Here, we use regularization to take into account the number of ratings per movie
# to diminish the b_i effect of movies with a small number of ratings

# Create a grid for the tuning parameter lambda

lambdas <- seq(0, 8, 0.25)

# For each lambda,find b_i & b_u, followed by rating prediction & testing. 

rmses_lambda <- sapply(lambdas, function(l){
                                                               
mu_lambda <- mean(train_set$rating)
 
b_i <- train_set %>% 
group_by(movieId) %>%
summarize(b_i = sum(rating - mu_lambda)/(n()+l))

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_lambda)/(n()+l))

b_g <- train_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(genres) %>%
  summarise(b_g = sum(rating - b_i - b_u - mu_lambda, na.rm = TRUE)/(n()+l))
	
b_y <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(review_date) %>%
    summarise(b_y = sum(rating - b_i - b_u - b_g - mu_lambda, na.rm = TRUE)/(n()+l))

predicted_ratings_lambda <- test_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_y, by="review_date") %>%
    mutate(pred = mu_lambda + b_i + b_u + b_g + b_y) %>%
    pull(pred)
  return(RMSE(predicted_ratings_lambda, test_set$rating))
})      # This execution might take 3-5 min. So pls be patient! :-) 

# Plot rmses vs lambdas to select the optimal lambda                                                             
qplot(lambdas, rmses_lambda)  
# Assign optimal tuning parameter (lambda)
lambda <- lambdas[which.min(rmses_lambda)]
lambda
# Minimum RMSE achieved using lambdas
regularised_rmse <- min(rmses_lambda) 
regularised_rmse

# Test and save results                                                             
rmse_computed <- bind_rows(rmse_computed,
				tibble(Method="Regularized model", RMSE = regularised_rmse)) 
				

rmse_computed %>% knitr::kable()

###########################################################
##### Apply final model to predict ratings in final_holdout_test set #####
###########################################################

# Mutate final_holdout_test dataset according to the changes made to edx
final_holdout_test <- final_holdout_test %>%
  mutate(review_date = round_date(as_datetime(timestamp), unit = "week"))

final_predicted_ratings <- final_holdout_test %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  left_join(genre_avg, by="genres") %>%
  left_join(rev_date_avg, by="review_date") %>%
  mutate(pred = mu_simple_avg + b_i + b_u + b_g + b_y ) %>%
  pull(pred)
  
## RMSE of the final_holdout_test set

final_set_rmse <- RMSE(final_predicted_ratings, final_holdout_test$rating)
final_set_rmse

#####################################################################
# Results - tabulate result of final hold-out test of algorithm 
#####################################################################

#Save results to data frame
rmse_computed <- bind_rows(rmse_computed, 
                           tibble(Method = "Final Results from Target Data Set" , RMSE = final_set_rmse))
						   
rmse_computed %>% knitr::kable()

################# End of Program ##########################