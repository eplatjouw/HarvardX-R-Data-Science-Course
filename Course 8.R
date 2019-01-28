library(tidyverse)
### Caret Package, training and test sets, and overall accuracy

  ## Caret Package: several usefull functions for building and assessing machine learning methods
install.packages("caret", dependencies = TRUE)
library("caret")

  ## First simple example: Predict sex (male or female) using height
library("dslabs")
data(heights)
head(heights)
y <- heights$sex
x <- heights$height
  # Categorical outcome as y is either male or female and we have only one predictor height
  # Will not be able to predict accurately based on x as male/female heigths are not that different to within group variability
  # But van we do better than guessing?
  #Quantify the definition of better.
  # When developing an algoritm, we know the outcomes
  # To mimic the evalution process we split the data in two and pretend to not know the outcome
  # Only after algoritm is constructed we can evaluate the algoritm with what we know

  # Training set: Group of which we know the outcome
  # Test set: Group we don't know the outcome of

  # Generating training and test sets you can randomly split the data
  ## Function createDataPartition() from caret package helps us generate indexes to do that
    # Argument times in function defines how many random samples of indexes to return
    # Argument p is used to define what proportion of the index represented 
    # Argument list is used if indexes returned as list or not

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list=FALSE)

    # Define training and test set
train_set <- heights[-test_index,]
test_set <- heights[test_index,]

    # Develop algoritm using only the training set
    # After we freeze it and evaluate using test set
    
    # Simplest way to evluate for categorical data is reporting proportion correct
      # referred to as overall accuracy

  # Simplest Machine Learning algoritm guessing the outcome
y_hat <- sample(c("Male", "Female"),
                 length(test_index), replace=TRUE)
        # Completely ignoring the predictor and just guessing the sex

  # Usefull to use factors to represent categorical outcomes
  # Required for R ML function such as those in caret package
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex))

  # Overall Accuracy
mean(y_hat == test_set$sex) # = +/- 50%, we're guessing

  # Can we do better? We should be as exploratory data analysis suggest male are on average slightly taller than females
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

  # How do we make use of this?
  # E.g. predict male if height is within two SD of averag male height
y_hat <- ifelse( x> 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))

     # Overall Accuracy
mean(y == y_hat)

  # Can we do even better?
    # Examine accuracy obtained from other cutoffs (other than 2SD or 62)
    # Only on training set! Test set only for evaluation
    # Evaluation an algortim on the training set can lead to overfitting
    #   which results in dangerously over optimistic assesments
 
  # Other cutoffs, 10 different ones
cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

  # Plot showing accuracies on training set for males and females
help(plot)
plot(cutoff,accuracy,type="b")

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

  ## Test this cutoff on our test set to make sure accuracy is not overly optimistic
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)


### Confusion matrix
## Overall accuracy can be a deceptive measure
  # We predicted Male for height >64 yet avg Female height is 65

  ## Construct a Confusion matrix
    # Tabulates each combination of each prediction and actual value

table(predicted = y_hat, actual = test_set$sex)

    # Compute the accuracy for each sex
test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarize(accuracy = mean(y_hat == sex))
    # Very high accuracy for males but low for females
    # Because of prevelance
      # More males than females in dataset
prev <- mean(y == "Male")
prev

    # Several metrics that we can use to evaluate an algoritm in a ay that prevalence does not cloud assessments
    # All derived from confusion matrix

  ## Study sensitivity and specificity separately
    # To define we need a binary outcome
    # When outcomes are categorical we define these terms for a specific category

    # TP = True Positive; FP= False Positive; FN= False Negative; TN= True Negative
    # Sensitivity = TP/(TP+FN)
      # = True Positive Rate (TPR) or recall
    # Specificity = TN/(TN+FP)
      # = True Negative Rate (TNR)
    ## Another way of quantifying specificity
    # Specificity = TP/(TP+FP)
      # Positive Predictive Value (PPV) or precision
        # Depends on prevalence, implying you can get higher precision with higher prevalence, even when guessing

  ## confusionMatrix() function defines all these metrics
    # Define what a positive is first
    # Function expects factors as input, first one being the positive one or y = 1
install.packages("e1071")
library(e1071)
confusionMatrix(data = y_hat, reference =test_set$sex)



### Balanced accuracy and F1 score
## Balanced accuracy: average between specificity and sensitivity
  # One number metrics preferred over overall accuracy
  # Specificity and sensitivity are raised so more appropriate to use harmonic average of both

  ## F1 score is a widely used one number summary 
    # = harmonic average between precision and recall
    # = 1/[1/2 * (1/recall + 1/precision)]
    # = 2 * [(precision * recall)/ (precision + recall)]
    # Can weigh specificity and sensitivity differently
  
    # F_meas() function computes the summary with beta defaulting to 1

  ## Rebuild prediction but maximizing F-score instead of overall accuracy

cutoff <- seq(61,70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

plot(cutoff, F_1, type ="b")

max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
confusionMatrix(data = y_hat, reference = test_set$sex)


### Prevalence matters in practice


### ROC and precision-recall curves
p <- 0.9 # guess males 90% of the time
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, prob=c(p,1-p)) %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex) # due to bias as more males than females

  ## Evaluating methods by comparing them graphically with plots
  ## Receiver Operating Characteristics (ROC) curve
    # Plots sensitivity (TPR) versus 1-specificity (FPR)
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), length(test_index), replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

      # Construct ROC curve for height based approach
cutoffs <- c(50, seq(60,75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

    # Always nice to add cutoffs used to the points
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text(nudge_y = 0.01)

    # One Weakness: Neither measures plotted depend on prevalence
    # When prevalence matters, use a precision-recall plot
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

    # Precision of guessing is not high because prevalence is low

    # Change positives to mean Male instead of Female
    # ROC curve remains the same but precision recall plot changes
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

### Comprehension Check: Confusion Matrix

library(dslabs)
library(dplyr)
library(lubridate)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

  # Q1
head(dat)
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))
  # Q2
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list=FALSE)

train_set <- dat[-test_index,]
test_set <- dat[test_index,]

y_hat <- ifelse( x== "online", "Male", "Female") %>% factor(levels = levels(y))

mean(y_hat == y)

  # Q3
table(predicted = y_hat, actual = y)

  # Q4
confusionMatrix(data = y_hat, reference =y)
  # Q5
confusionMatrix(data = y_hat, reference =test_set$sex)
  # Q6
confusionMatrix(data = y_hat, reference =test_set$sex)

  ### Comprehension check: Practice with Machine Learning
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
head(y)

  # Q1, Q2, Q3

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

iris %>% group_by(Species) %>% summarize(mean(Sepal.Length), sd(Sepal.Length))
iris %>% group_by(Species) %>% summarize(mean(Sepal.Width), sd(Sepal.Width))
iris %>% group_by(Species) %>% summarize(mean(Petal.Length), sd(Petal.Length))
iris %>% group_by(Species) %>% summarize(mean(Petal.Width), sd(Petal.Width))

    # Sepal.Length
cutoff <- seq(5.0,6.5,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})

plot(cutoff,accuracy,type="b")
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]

y_hat <- ifelse(test$Sepal.Length > best_cutoff, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))

mean(y_hat == test$Species)

    # Sepal.Width
cutoff <- seq(2.0,3.5,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})

plot(cutoff,accuracy,type="b")
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]

y_hat <- ifelse(test$Sepal.Width > best_cutoff, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))

mean(y_hat == test$Species)

    # Petal.Length
cutoff <- seq(3.5,6.0,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})

plot(cutoff,accuracy,type="b")
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))

mean(y_hat == test$Species)

    # Petal.Width
cutoff <- seq(0.5,2.5,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})

plot(cutoff,accuracy,type="b")
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]

y_hat <- ifelse(test$Petal.Width > best_cutoff, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))

mean(y_hat == test$Species)



table(predicted = y_hat, actual = test$Species)

    # Q4

# Sepal.Length
cutoff <- seq(5.0,6.5,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == test$Species)
})

plot(cutoff,accuracy,type="b")
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]

y_hat <- ifelse(train$Sepal.Length > best_cutoff, "virginica", "versicolor") %>%
  factor(levels = levels(train$Species))

mean(y_hat == train$Species)

# Sepal.Width
cutoff <- seq(2.0,3.5,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == test$Species)
})

plot(cutoff,accuracy,type="b")
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]

y_hat <- ifelse(train$Sepal.Width > best_cutoff, "virginica", "versicolor") %>%
  factor(levels = levels(train$Species))

mean(y_hat == train$Species)

# Petal.Length
cutoff <- seq(3.5,6.0,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == test$Species)
})

plot(cutoff,accuracy,type="b")
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
y_hat <- ifelse(train$Petal.Length > best_cutoff, "virginica", "versicolor") %>%
  factor(levels = levels(train$Species))

mean(y_hat == train$Species)

# Petal.Width
cutoff <- seq(0.5,2.5,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == test$Species)
})

plot(cutoff,accuracy,type="b")
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]

y_hat <- ifelse(train$Petal.Width > best_cutoff, "virginica", "versicolor") %>%
  factor(levels = levels(train$Species))

mean(y_hat == train$Species)

    # Q5

# Petal.Length
cutoff <- seq(3.5,6.0,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})

plot(cutoff,accuracy,type="b")
max(accuracy)

best_cutoff_PL <- cutoff[which.max(accuracy)]
y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))

mean(y_hat == test$Species)

# Petal.Width
cutoff <- seq(0.5,2.5,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})

plot(cutoff,accuracy,type="b")
max(accuracy)
best_cutoff_PW <- cutoff[which.max(accuracy)]

y_hat <- ifelse(test$Petal.Width > best_cutoff, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))

mean(y_hat == test$Species)



table(predicted = y_hat, actual = test$Species)


y_hat <- ifelse(test$Petal.Length > best_cutoff_PL | test$Petal.Width > best_cutoff_PW, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))

mean(y_hat == test$Species)



### Conditional Probabilities
