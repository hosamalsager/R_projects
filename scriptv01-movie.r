
library(randomForest)
library(dplyr)
library(MASS)
library(caret)
require(RCurl)
require(prettyR)
require(ggplot2)

#Importing data
movies <- read.csv("....../movie_metadata.csv")
#to explor data kinds
names(movies)
str(movies)
levels(movies$language)

#All movies that language is English
sum(movies$language == "English")

#Dataset with only movies in English language
by.language <- movies[(movies$language == "English"), ]

#English Movies only in USA, now we have English language and country USA Dataset
Movies.USA.English <- by.language[by.language$country == "USA", ]

#Top 5 budget
tail(sort(Movies.USA.English$budget), 5)

#Visualize some data of movies in 2010
Movies.2010.USA.English = Movies.USA.English[Movies.USA.English$title_year == "2010",]

#some visualization
plot(Movies.2010.USA.English$gross, Movies.2010.USA.English$budget)
plot(Movies.2010.USA.English$budget, Movies.2010.USA.English$imdb_score)

#Data set to be cleaned and process
AfterClean <- Movies.USA.English

#choosing predictors that are meaningful in the model
AfterClean <- AfterClean[c("director_facebook_likes", "actor_1_facebook_likes",
                           "actor_2_facebook_likes", "actor_3_facebook_likes",
                           "budget","gross")]



#Identify and locate NAs in data
which(is.na(AfterClean))
sum(is.na(AfterClean))
colSums(is.na(AfterClean))

#Dealing with missing values NAs, first we have to covert data to numeric
AfterClean <- na.omit(AfterClean)

#We are ready t go!
set.seed(123)

#Taking sample of data to prepare Test and Train data
sampl <- floor(0.7 * nrow(AfterClean))
idx <- sample(seq_len(nrow(AfterClean)), size = sampl)

#Training and Test Data
train <- AfterClean[idx, ]
test <- AfterClean[-idx, ]

#First model using randomForest algorithm
model <- randomForest(gross ~ actor_1_facebook_likes + actor_2_facebook_likes + actor_3_facebook_likes + director_facebook_likes + budget, data = train)

#Second model by using leaner regression algorithm
model2 <- lm(gross ~ actor_1_facebook_likes + actor_2_facebook_likes + actor_3_facebook_likes + director_facebook_likes + budget, data = train)

test$predRandomfores <- predict(model, test)
test$predLeanearRegression <- predict(model2, test)

#Compare 2 models
plot(test$predRandomfores, test$predLeanearRegression)

#Model randomfores vs actual prediction gross
plot(test$gross, test$predRandomfores)

#Model leanear regression vs actual prediction gross
plot(test$gross, test$predLeanearRegression)