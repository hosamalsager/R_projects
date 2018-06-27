# Install the packages 'class' and 'kknn' and load their libraries, which will be needed for their k-nearest neighbors algorithms
install.packages("class")
install.packages("kknn")
library(class)
library(kknn)

# Load the data and view its structure
balance_scale <- read.csv("https://ibm.box.com/shared/static/684jzm7e6fbbssg87yc2v4dy53dgkdew.txt", sep = ",")
str(balance_scale)

# View the first few rows of the data using the head function
# Note: The raw data does not contain any column names
head(balance_scale)

#Clean the data
colnames(balance_scale) <- c("Class_Name","Left_Weight", "Left_Distance", "Right_Weight", "Right_Distance")

# Calculate the products and differences
Right_Product <- balance_scale[,4]*balance_scale[,5]
Left_Product <- balance_scale[,2]*balance_scale[,3]
Differences <- Right_Product-Left_Product
# Add columns for Right_Product, Left_Product and Differences
balance_scale$Right_Product <- Right_Product
balance_scale$Left_Product <- Left_Product
balance_scale$Differences <- Differences


#Separate the data into training and test groups
# Use the sample function to create a vector of indices that will be used
set.seed(1234)
ind <- sample(2, nrow(balance_scale), replace=TRUE, prob=c(0.7, 0.3))


# Create the training and test data from the dataset using ind
bscale.train <- balance_scale[ind==1, 6:8]
bscale.test <- balance_scale[ind==2, 6:8]



# Create the target vectors for the training and test data from the dataset using ind
bscale.trainLabels <- balance_scale[ind==1, 1]
bscale.testLabels <- balance_scale[ind==2, 1]

#Implementation of k-nearest neighbors for classification
# Use the knn command to make predictions on the Class_Name of the test data
knn_class <- knn(train = bscale.train, test = bscale.test, cl = bscale.trainLabels, k=3)

# Find the number of incorrectly classified points
correct <- which(knn_class == bscale.testLabels, arr.ind = TRUE)
incorrect <- which(knn_class != bscale.testLabels, arr.ind = TRUE)
cat("Number of incorrectly classified points:",length(incorrect),"\n")

# Find the proportion of correctly classified points
proportion_correct <- length(correct)/length(bscale.testLabels)
cat("Proportion of correctly classified points", proportion_correct,"\n")


#K-nearest neighbors regression using the kknn command
# Run the knn regression using the kknn command
knn_reg <- kknn(formula = Differences ~ ., train=bscale.train, test=bscale.test, k=3)

# Find the number of incorrectly classified points
incorrect_reg <- which(knn_reg$fitted.values != bscale.test$Differences, arr.ind = TRUE)
cat("Number of incorrectly classified points:", length(incorrect_reg), "\n");

# Find the proportion of correctly classified points
correct_reg <- which(knn_reg$fitted.values == bscale.test$Differences, arr.ind = TRUE)
cat("Proportion of correctly classified points", length(correct_reg)/length(bscale.test$Differences), "\n")


# Display the first few rows of the regression estimates of the differences and their true values
head(cbind(knn_reg$fitted.values,bscale.test$Differences))



#We can also determine the **optimal value of k** using the train.kknn command.
best_reg <- train.kknn(formula = Differences ~ ., data=bscale.train, kmax=8)
best_reg$best.parameters




# Run the knn regression again using the kknn command with k=2
knn_reg2 <- kknn(formula = Differences ~ ., train=bscale.train, test=bscale.test, k=2)

# Find the number of incorrectly classified points
incorrect_reg2 <- which(knn_reg2$fitted.values != bscale.test$Differences, arr.ind = TRUE)
cat("Number of incorrectly classified points:", length(incorrect_reg2),"\n")
# Find the proportion of correctly classified points
correct_reg2 <- which(knn_reg2$fitted.values == bscale.test$Differences, arr.ind = TRUE)
cat("Proportion of correctly classified points",length(correct_reg2)/length(bscale.test$Differences),"\n")

# Display the first few rows of the new regression estimates of the differences and their true values
head(cbind(knn_reg2$fitted.values,bscale.test$Differences))


# The kknn function can also be used for classification
knn_class2 <- kknn(formula = Class_Name ~ ., train=subset(balance_scale, select=c(Class_Name,Right_Product,Left_Product,Differences))[ind==1,], test=subset(balance_scale, select=c(Class_Name,Right_Product,Left_Product,Differences))[ind==2,], k=3)
# Find the number of incorrectly classified points
incorrect_class2 <- which(knn_class2$fitted.values != bscale.testLabels, arr.ind = TRUE)
cat("Number of incorrectly classified points:",length(incorrect_class2),"\n")

best_class <- train.kknn(formula = Class_Name ~ ., data=subset(balance_scale, select=c(Class_Name,Right_Product,Left_Product,Differences))[ind==1,], kmax=8)
best_class$best.parameters

# Using k=1
knn_class3 <- kknn(formula = Class_Name ~ ., train=subset(balance_scale, select=c(Class_Name,Right_Product,Left_Product,Differences))[ind==1,], test=subset(balance_scale, select=c(Class_Name,Right_Product,Left_Product,Differences))[ind==2,], k=1)
# Find the number of incorrectly classified points
incorrect_class3 <- which(knn_class3$fitted.values != bscale.testLabels, arr.ind = TRUE)
cat("Number of incorrectly classified points:", length(incorrect_class3),"\n")
