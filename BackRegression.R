library(dplyr)
library(tidyverse)




####-----------SETUP-----------####
set.seed(31415)
folds <- 10

# Using (Linda's) Tanvi's data
#data <- read.csv('data/lm934_housing_extracted.csv')
#data <- read.csv('data/tsy7_housing_extracted.csv')
data <- read.csv('data/finalData.csv')

# Remove extraneous data, set rownames to PID
rownames(data) <- data$pid
data <- data[, !(names(data) %in% c('pid', 'buildings', 'model', 'address', 'grade', 'heatfuel'))]

# Remove outliers
#data <- data[data$land < 5,]
data <- data[data$beds > 0,]
data <- data[data$baths > 0,]

# Formatting the data
data$style <- as.factor(trimws(data$style))
data$heattype <- as.factor(trimws(data$heattype))
data$nghd <- as.factor(trimws(data$nghd))

# Simplify AC data
#data$ac <- trimws(data$ac)
#data$ac <- gsub('&nbsp;', 'None', data$ac)
#data$ac <- data$ac != 'None'
data$ac <- as.factor(data$ac)

# Remove rows that are now identical
data <- unique(data)

# From previous analysis, we find that the log of certain values gives greater linear correlation
# than the values themselves
data$value <- log(data$value)
data$land <- log(data$land)
data$living <- log(data$living)

# After performing the log, we need to remove 'Inf's that may have appeared
data <- filter(data, is.finite(data$land))


####-----------MAIN-----------####
# Randomize the order of checking the columns. Also ignore the value column, as it is the response
# variable, and add '' to ensure that the first loop will use all columns.
selected <- names(data)[sample(length(data))]
selected <- c('', selected[selected != 'value'])

prev.RMSE <- Inf

for(col in selected) {
  avg.RMSE <- 0
  
  fields <- selected[selected != col]
  sub.data <- data[c('value', fields)]
  
  print(fields)
  
  for(i in 1:folds) {
    row.order <- sample(nrow(data))
    train.rows <- sub.data[head(row.order, nrow(data)*0.8),]
    test.rows <- sub.data[tail(row.order, -nrow(data)*0.8),]
    
    # The test.rows DF sometimes gets an NA row for no apparent reason...
    test.rows <- filter(test.rows, !is.na(test.rows$value))
    
    curr.model <- lm(value ~ ., train.rows)
    
    # Fill out the factor levels that were not used during training
    curr.model$xlevels$heattype <- levels(data$heattype)
    curr.model$xlevels$style <- levels(data$style)
    curr.model$xlevels$nghd <- levels(data$nghd)
    curr.model$xlevels$ac <- levels(data$ac)
    
    predictions <- predict.lm(curr.model, test.rows)
    avg.RMSE = avg.RMSE + sqrt(mean((test.rows$value - predictions)^2))
  }
  
  avg.RMSE <- avg.RMSE / folds
  
  print(paste('Average:', avg.RMSE))
  if(avg.RMSE < prev.RMSE) {
    selected <- fields
    prev.RMSE <- avg.RMSE
    print('Updated!')
  }
}

print(paste('Final RMSE:', prev.RMSE))
print(selected)

final.model <- lm(value ~ ., data[c('value', selected)])

summary(final.model)



####-----------PREDICTIONS-----------####
pids <- c('1000', '12824', '12773', '19128')

for(p in pids) {
  interval <- predict.lm(final.model, data[p,], interval = 'predict')
  print(paste('True value:', exp(data[p,]$value)))
  print('Prediction interval:', )
  print(exp(interval))
}







