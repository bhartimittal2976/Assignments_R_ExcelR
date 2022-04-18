library(caret)
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)

# code-1 reading dataset

iris <- read.csv("iris.csv")
View(iris)

# code-2 Scatter plot matrix

transparentTheme(trans = .4)

featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

# code-2 Scatterplot Matrix with Ellipses

featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "ellipse",
            ## Add a key at the top
            auto.key = list(columns = 3))

# code-3 Overlayed Density Plots

transparentTheme(trans = 0.6)
featurePlot(x = iris[, 1:4], 
            y = iris$Species,
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(4, 1), 
            auto.key = list(columns = 3))


#code-4 Box plot

featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(4,1 ), 
            auto.key = list(columns = 2))

# code-5  Identifying Correlated Predictors

descrCor <- cor(iris$Sepal.Length , iris$Petal.Length)
summary(descrCor[upper.tri(descrCor)])

# code-6 Simple Splitting Based on the Outcome

trainIndex <- createDataPartition(iris$Species, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

irisTrain <- iris[ trainIndex,]
irisTest  <- iris[-trainIndex,]

# code-7 Confusion matrix : Class 

xtab <- confusionMatrix(iris$Species, sample(iris$Species))
as.matrix(xtab)

# code-8 Training data set

data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]
knnFit <- train(TrainData, TrainClasses,
                method = "knn",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))
confusionMatrix(knnFit)
confusionMatrix(knnFit, "average")
confusionMatrix(knnFit, "none")

# code-9 preprocessing

sum(is.na(iris))

# code-10 splitting training set

index <- createDataPartition(iris$Sepal.Length, p=0.75, list=FALSE) 
trainSet <- iris[ index,] 
testSet <- iris[-index,]
str(trainSet)

# code-11 Training models using Caret
names(getModelInfo())

fitControl <- trainControl(   method = "repeatedcv",   number = 5,   repeats = 5)
modelLookup(model='gbm')


# code-12 dataframe

df <- data.frame(iris)
print(df)

set.seed(100)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(iris$Petal.Length, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData <- iris[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- iris[-trainRowNumbers,]

# code- 13 dimension of dataset

dim(iris)

# code-14 fit a random forest model (using ranger)

rf_fit <- train(as.factor(Species) ~ ., 
                data = iris, 
                method = "ranger")
rf_fit

# predict the outcome on a test set

iris_rf_pred <- predict(rf_fit, iris)

# compare predicted outcome and true outcome

confusionMatrix(iris_rf_pred, as.factor(iris$Species))

#code-15 Features of a dataset

dataset <- read.csv("iris.csv")
class(dataset)
dim(dataset)
str(dataset)
summary(dataset)
colSums(is.na(dataset))
boxplot(dataset)

# Extract predictions and class probabilities from train objects

knnFit <- train(Species ~ ., data = iris, method = "knn",
                trControl = trainControl(method = "cv"))
rdaFit <- train(Species ~ ., data = iris, method = "rda",
                trControl = trainControl(method = "cv"))
predict(knnFit)
predict(knnFit, type = "prob")
bothModels <- list(knn = knnFit,
                   tree = rdaFit)
predict(bothModels)
extractPrediction(bothModels, testX = iris[1:10, -5])
extractProb(bothModels, testX = iris[1:10, -5])

# Simple linear regression model 

model <- train(Sepal.Length ~ Petal.Length,
               data = iris,
               method = "lm")
# Multiple linear regression model

model <- train(Sepal.Length ~ .,
               data = iris,
               method = "lm")






























