library(caret)
library(e1071)
library(dplyr)
setwd("C:/Users/pavel/Documents/Škola/R_projects/datasets")
###### data prep ######

data(iris)
iris_subset <- iris[iris$Species != "virginica", ]
iris_subset <- iris_subset[, c("Sepal.Width", "Petal.Width", "Species")]
iris_subset$Species <- factor(iris_subset$Species) # recompute factors
plot(iris_subset[, 1:2], col = iris_subset$Species)

###### first part - svm train ######

model <- svm(Species ~ ., data = iris_subset, kernel = "linear", cost = 500)
plot(model, data = iris_subset)

###### second part ######

# reading benchmark csv with the specific format
read_benchmark_data <- function(filename) {
  read.csv(filename, header = F, sep = "\t", col.names = c("x", "y", "class"))
}
# plot points of the given benchmark dataset with classes distinguished
plot_benchmark_data <- function(data, title = "") {
  plot(y ~ x, data = data, col = class, pch = 19, main = title)
}

# read the data from local disk:
data_jain <- read_benchmark_data("jain.txt")
data_flame <- read_benchmark_data("flame.txt")

# or read the data directly from the web site:
data_jain <- read_benchmark_data("http://www.fi.muni.cz/~xcechak1/IB031/datasets/jain.txt")
data_flame <- read_benchmark_data("http://www.fi.muni.cz/~xcechak1/IB031/datasets/flame.txt")

# plot the data:
plot_benchmark_data(data_jain, "jain.txt")
plot_benchmark_data(data_flame, "flame.txt")

##### second part - model train ########

data_flame$class <- as.factor(data_flame$class)

svm.model <- svm(class ~ ., data = data_flame, gamma = 0.1) # radial kernel by default
# svm.model <- svm(class ~ ., data = data_flame, kernel = "polynomial", degree = 2)

svm.model

plot(svm.model, data_flame)

data_jain$class <- as.factor(data_jain$class)

svm.model <- svm(class ~ ., data = data_jain, kernel = "polynomial")
# svm.model <- svm(class ~ ., data = data_jain) # radial kernel by default

plot(svm.model, data_jain)


######### second part - titanic ##########

titanic_train <- read.csv("titanic_train.csv")
titanic_test <- read.csv("titanic_test.csv")
titanic_train <- select(titanic_train, Survived, Pclass, Sex, Age, Fare)
titanic_test <- select(titanic_test, Survived, Pclass, Sex, Age, Fare)
head(titanic_train)
titanic_train$Survived <- as.factor(titanic_train$Survived)
titanic_test$Survived <- as.factor(titanic_test$Survived)
titanic_test <- na.omit(titanic_test)

titanic_model <- svm(Survived ~ ., data = titanic_train, kernel = "polynomial", gamma = 0.5, cost = 5)

referencies <- titanic_test$Survived

prediction <- predict(titanic_model, newdata = titanic_test)

conf_mat <- table(referencies, prediction)
conf_mat

accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
accuracy

