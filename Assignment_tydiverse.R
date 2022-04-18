install.packages("viridis")
install.packages("broom")
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(viridis)
library(broom)

# code-1 Data loading and viewing

d_iris <- read.csv("iris.csv")

# code-2 filter() allows you to select a subset of rows in a data frame

iris %>%
  filter(Species=="virginica")


iris %>%
  filter(Species=="virginica",
         Sepal.Length > 6)

# code-3 arrange() sorts the observations in a dataset in ascending or descending order

iris %>%
  arrange(Sepal.Length)

iris %>%
  arrange(desc(Sepal.Length))

# code-4 Combine multiple dplyr verbs in a row with the pipe operator %>%

iris %>%
  filter(Species=="virginica") %>%
  arrange(desc(Sepal.Length))

# code-5 mutate() allows to update or create new columns of a data frame

iris %>%
  mutate(Sepal.Length=Sepal.Length*10) # changed in the same column

iris %>%
  mutate(SLMm=Sepal.Length*10) # created a new column and retained the old column

# code-6 Combine the verbs filter(), arrange(), and mutate()

iris %>%
  summarize(medianSL=median(Sepal.Length))

iris %>%
  filter(Species=="virginica") %>%
  summarize(medianSL=median(Sepal.Length))

# for multiple variables
iris %>%
  filter(Species=="virginica") %>%
  summarize(medianSL=median(Sepal.Length),
            maxSL=max(Sepal.Length))

# code-7 group_by() allows to summarize within groups instead of dataset

iris %>%
  group_by(Species) %>%
  summarize(medianSL=median(Sepal.Length), 
          maxSL=max(Sepal.Length))

iris %>%
  filter(Sepal.Length>6) %>%
  group_by(Species) %>%
  summarize(medianPL=median(Petal.Length),
            maxPL=max(Petal.Length))


# code-8 Scatter plot with ggplot : Compare petal width and length

iris_small <- iris %>%
  filter(Sepal.Length > 5)

ggplot(iris_small, aes(x=Petal.Length,
                       y=Petal.Width)) +
  geom_point()


# code-9 Addition of color to code 8

ggplot(iris_small, aes(x=Petal.Length,
                       y=Petal.Width,
                       color=Species)) +
  geom_point()

# code-10 Sizing to code 9

ggplot(iris_small, aes(x=Petal.Length,
                       y=Petal.Width,
                       color=Species,
                       size=Sepal.Length)) +
  geom_point()

# code-11 Faceting

ggplot(iris_small, aes(x=Petal.Length,
                       y=Petal.Width)) +
  geom_point()+
  facet_wrap(~Species)

# code-12 Bar plot

by_species <- iris %>%
  filter(Sepal.Length>6) %>%
  group_by(Species) %>%
  summarize(medianPL=median(Petal.Length))

ggplot(by_species, aes(x=Species,
                         y=medianPL)) +
  
  geom_col()

# code-13 Histogram

ggplot(iris_small, aes(x=Petal.Length))+
  geom_histogram()

# code-14 Boxplots

ggplot(iris_small, aes(x=Species,
                       y=Sepal.Width))+
  geom_boxplot()

# code-15 counting

iris%>%
  count(Petal.Width) 

iris %>%
  group_by(Petal.Length) %>%
  summarise(count = n())

iris%>%
  count(Petal.Length, sort = TRUE)


iris%>%
  count(Sepal.Length, Species) 




























































