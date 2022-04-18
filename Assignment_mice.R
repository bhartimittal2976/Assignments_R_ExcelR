library(mice)
data <- read.csv("iris.csv")

# code-1 Get summary

summary(iris)

# code-2 Generate 10% missing values at Random

install.packages("missForest")
library(missForest)

iris.mis <- prodNA(iris, noNA = 0.1)

# code-3 Check missing values introduced in the data

summary(iris.mis)

# code-4 remove categorical variables

iris.mis <- subset(iris.mis, select = -c(Species))
summary(iris.mis)

# code-5 returns a tabular form of missing value present in each variable in a data set

md.pattern(iris.mis)

# code-6 Visualization using VIM
install.packages("VIM")
library(VIM)
mice_plot <- aggr(iris.mis, col=c('red','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(iris.mis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))


# code-7 impute the missing values

imputed_Data <- mice(iris.mis, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

# code-8 Multiply imputed data set

AB <- mice(data = iris.mis, m = 5, method = "pmm", maxit = 50, seed = 500)
AB

# code-9 check imputed values

imputed_Data$imp$Sepal.Width

# code-10 get complete data ( 2nd out of 5)

completeData <- complete(imputed_Data,2)

# code-11 build predictive model : linear model

fit <- with(data = iris.mis, exp = lm(Sepal.Width ~ Sepal.Length + Petal.Width))

# code-12 Visualization

marginplot(iris.mis[c(1,2)])

densityplot(iris.mis$Sepal.Length)

stripplot(iris.mis$Sepal.Width, pch = 20, cex = 1.2)

# code-13 keep the continuous variables from imputed missing values and visualization
View(iris)
df <- data.frame(iris.mis)
df
don <- df[, 1:4]
summary(don)

res<-summary(aggr(don, sortVar=TRUE))$combinations









# code-14 matrix plot

matrixplot(don, sortby = 4.0)


# code-15 margin plot of the pairs

marginplot(iris[, c("Sepal.Length", "Petal.Length")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)

marginplot(iris.mis[, c("Sepal.Length", "Petal.Length")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)

marginplot(don[, c("Sepal.Length", "Petal.Length")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)










