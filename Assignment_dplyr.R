library(dplyr)

data <- read.csv("mtcars.csv")
head(mtcars)

# code-1 # we add a column with model names; which model is better

mtcars$model <- rownames(mtcars) 
which(mtcars$cyl <= 6 & mtcars$mpg < 20)
x <- mtcars[mtcars$cyl <= 6 & mtcars$mpg < 20, ]
x$model
mtcars[mtcars$cyl <= 6 & mtcars$mpg < 20, "model"]

# code-2 

x <- filter(mtcars, cyl <= 6, mpg < 20)
select(x, model)

# code-3

select( filter(mtcars, cyl <= 6, mpg < 20), model)
mtcars %>% filter(cyl <= 6, mpg < 20) %>% select(model) 


# code-4 Selectiong data; sample_n function selects random rows
sample_n(mtcars,3)

# code-5 sample_frac function returns randomly N% of rows
sample_frac(mtcars,0.5)

# code-6 Remove Duplicate Rows based on all the variables (Complete Row)

x1 = distinct(mtcars)

# code-7 Remove Duplicate Rows based on a variable

x2 = distinct(mtcars, mpg, .keep_all= TRUE)

# code-8 Remove Duplicates Rows based on multiple variables

x2A = distinct(mtcars, mpg, cyl, .keep_all= TRUE)


# code-9 Selecting Variables (or Columns)

mtc = select(mtcars, mpg, cyl:wt)

# Dropping Variables
mtc1 = select(mtcars, -mpg, -wt)
# OR
mtc2 = select(mtcars, -c(mpg,wt))

# code-10 Summarize selected variables

summarise(mtcars, mpg_mean = mean(mpg), mpg_med=median(mpg))

# Summarize Multiple Variables

summarise_at(mtcars, vars(mpg, cyl, wt), funs(n(), mean, median)) # will calculate but will show some warnings, solution is below

# solution to the upper code
summarise_at(mtcars, vars(mpg, cyl, wt), list(n=~n(), mean=mean, median=median)) # OR

summarise_at(mtcars, vars(mpg, cyl, wt), list(~n(), ~mean(.), ~median(.)))

# code-11 Summarize all Numeric Variables

summarise_if(mtcars, is.numeric, funs(n(),mean,median)) # OR

numdata = mtcars[sapply(mtcars,is.numeric)] # 1st store data
summarise_all(numdata, funs(n(),mean,median)) # Than summarize

# code-12 Summarize Factor Variable

summarise_all(mtcars["model"], funs(nlevels(.), nmiss=sum(is.na(.)))) # this isnot a factor variable

# code-13 Sort Data by Multiple Variables

arrange(mtcars, mpg, wt)
# sort one variable by descending order and other variable by ascending oder

arrange(mtcars, desc(mpg), wt)

# code-14 Summarise Data by Categorical Variable

t = summarise_at(group_by(mtcars, model), vars(mpg, wt), funs(n(), mean(., na.rm = TRUE)))
t


# Filter Data within a Categorical Variable

t1 = mtcars %>% filter(model %in% c("M", "D","H")) %>% group_by(model) %>%
  do(head( . , 2))
t1

# code-15 Calculate Rank for Variables

mtcars_new = mutate_at(mtcars, vars(mpg:disp), funs(Rank=min_rank(.)))
mtcars_new
