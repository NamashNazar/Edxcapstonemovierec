# Edxcapstonemovierec
This is my harvard edx capstone submission on movie recommendation

## Introduction/overview/executive summary

This is an analysis to develop a movie recommendation model.
* Dataset
Movielens dataset is used to conduct the analysis and predict ratings for different movies. The dataset
includes a unique ID for both movies and users, it includes the name of the movie, the rating it received by
a user, and the timestamp of the rating. It also includes the genres that apply to the movies.
* Goal of the project
We use this data to predict the rating that a user is likely to provide to a movie and therefore recommend
movies to users that they are likely to rate highly.
* Steps that were performed
We use the data set to develop models that will predict expected ratings given by users for movies. The
models are optimized to minimized the Root Mean Squared Errors (RMSE) between predicted ratings and
actual ratings.
1. Movielens dataset is split into edx and validation data so that models can be developed on edx data
and final performance can be evaluated on validation data.
2. Edx data is explored through visualizations to better understand the data.
3. Edx dataset is split into training and test set to optimise the model
4. Features are added to the model one by one to evaluate improvement in RMSE and then the models
are regularized.
5. The final model is applied to the validation dataset to evaluate the model performance.
We have limited data with few users rating few movies depending on quality of movie, its age, its genre etc.
We therefore use bias associated with users, movies, how old the movie is, and its genre to predict the rating
of particular movie for different users. The assumption is that similar movies receive similar ratings from
similar people in the same amount of time.

## Modeling approach
After splitting the edx dataset into training and testing dataset we start trying different features in the
dataset to make predictions.
1. Our first model will use the average rating in the dataset as the prediction for all missing ratings. By
using the average we calculate the RMSE and use this as the baseline. For any model to be worth
exploring it needs to perform better than this baseline.
2. We first use movie effects to predict ratings. We assume that movies get similar ratings from different
users- blockbuster hits are more likely to get higher ratings than flop movies.
3. We then add user effect to the model since we anticipate similar users give similar ratings to moviesmore
picky users give lower ratings even to blockbuster hits than other users.
4. We then add the age of the movie since our visualization above shows that movies of similar age have
similar count of ratings.
5. We then add the genre of the movies to test if similar kind of movies get similar kind of ratings.
6. We regularize all models to remove any error being added due to few datapoints to make predictions
- for instance very old and very new movies that have very few ratings which may skew their results.
7. We identify the best performing model
8. We then evaluate the performance of the best model using validation data.

## Summary of model optimization:
 A tibble: 9 x 2
method RMSE
## <chr> <dbl>
## 1 Just the average 1.06
## 2 Movie effect 0.944
## 3 User+ Movie effect 0.867
## 4 User+Movie+Time effect 0.866
## 5 User+Movie+Time+genre effect 0.866
## 6 Regularized Movie Effect Model 0.944
## 7 Regularized Movie + User Effect Model 0.866
## 8 Regularized Movie + User + time Effect Model 0.866
## 9 Regularized Movie + User +time + genre Effect Model 0.865

## Evaluate performance of model on validation data: RMSE
[1] 0.8642529

## Conclusion
In summary, we find that the movie recommendations can be improved by using different features while controlling
for any bias introduced by few observations resulting in big predictions. However, it is important to
evaluate the relative predictive power of different variables. In the above model the incremental improvement
using time and genre was less than user and movie IDs. We may be able to improve the predictive power
by genre by separating out all the genre tags that are applied to each movie so that there is a bigger pool
of data per genre. Similarly, for evaluation of models we can use other evaluation metrics (like specificity,
accuracy, recall etc) for the model to test performance. We can also treat ratings as categorical variables
and use more advance models like KNN, random forrest etc however they require higher computation power.
