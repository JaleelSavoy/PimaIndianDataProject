#############################################################################



## Session: Pima Indian Diabetes Analysis Logistic Regression

## Created By: Jaleel Savoy         

## Date: 22-Jan-2017                 



#############################################################################

# reading and splitting data and summary statistics

dataset <- read.csv("cleanedPima.csv")

    # split
library(caTools)
set.seed(123)
split <- sample.split(dataset$Class, SplitRatio = 2/3)
training_set <- subset(dataset, split == T)
testing_set <- subset(dataset, split == F)

    # summary statistics
install.packages('Hmisc')
library(Hmisc)

head(training_set)
str(training_set)
summary(training_set)
describe(training_set)

head(testing_set)
str(testing_set)
summary(testing_set)
describe(testing_set)

mean(dataset$Age)
min(dataset$Age)
max(dataset$Age)

###
  ## the dataset has a proportion of about 33% diabetics
  ## mean age is about 31 years old, min of 21, max of 81
###

# two-way contingency tables using Class and BMI
xtabs( ~Class + BMI, data = training_set)


#########################################################
# outlier detection
sapply(training_set[,1:8], function(x) quantile(x, c(.01,.05,.25,.5,.75,.90,.95, .99, 1),na.rm=TRUE) )

#no real outliers


#########################################################
# missing value imputation
sapply(training_set, function(x) sum(is.na(x)) )
#No missing values


#########################################################
# correlation and VIF
correlation_table <- cor(training_set[,1:8])
correlation_table1 <- cor(training_set[, 1:9])

install.packages('usdm')
library(usdm)
vif(training_set[,1:8])
vif(training_set[,1:9])

#########################################################
# create training and validation sets

set.seed(123)
smp_size <- floor(0.7 * nrow(training_set))

train_ind <- sample(seq_len(nrow(training_set)), size = smp_size)
training <- training_set[train_ind, ]
validation <- training_set[-train_ind, ]


#########################################################
# running the logistic model on training set

model1 <- glm(formula = Class ~ ., data = training, family = "binomial")
summary(model1)

  #Model using only variables with p-values less than .1
model2 <- glm(formula = Class ~ Glucose.Level + BMI + Diabetes.Pedigree.Function + Age, data = training, family = "binomial")
summary(model2)

confint(model2, level = .9)


#########################################################
# check performance on the validation set

val <- predict(model2, validation, type = "response")

mydf <- (cbind(validation, val))
mydf$response <- as.factor(ifelse(mydf$val>0.5, 1, 0))

install.packages('ROCR')
library(ROCR)
scores1 <- prediction(predictions = mydf$val, labels = mydf$Class)
print(scores1)

prop.table(table(mydf$response == training$Class))


val2 <- predict(model2, validation, type = "response")

mydf2 <- (cbind(validation, val2))
mydf2$response <- as.factor(ifelse(mydf2$val2>0.5, 1, 0))

install.packages('ROCR')
library(ROCR)
scores2 <- prediction(predictions = mydf$val, labels = mydf$Class)
print(scores2)

prop.table(table(mydf$response == mydf$Class))

prop.table(table(mydf2$response == mydf2$Class))


# Confusion Matrix
library(caret)
confusionMatrix(mydf$response, mydf$Class)
confusionMatrix(mydf2$response, mydf$Class)


# lift chart
## LIFT CHART

lift.obj <- performance(scores2, measure="lift", x.measure="rpp")

plot(lift.obj,
     
     main="Lift Chart",
     
     xlab="% of Sample",
     
     ylab="Lift",
     
     col="blue")

abline(1,0,col="grey")


install.packages("gains")
library(gains)
gains.cross <- gains(actual = mydf2$Class, predicted = mydf2$val2, groups = 10)
print(gains.cross)

y_pred <- predict(model2, testing_set, type = "response")
final <- cbind(testing_set, y_pred)
final$y_pred <- as.factor(ifelse(final$y_pred > 0.5, 1, 0))
final$Class <- as.factor(final$Class)
prop.table(table(final$Class == final$y_pred))
        #   MODEL IS 75.38% ACCURATE


#############################################################

## ADD ME ON LINKEDIN @ linkedin.com/in/jaleelwsavoy