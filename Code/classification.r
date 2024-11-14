# set current working directory
setwd ("C:/Programming/R/Classification")

# read data
mydata <- read.csv("BankChurnDataset.csv")

# give me information about the data
str(mydata)

# convert string to categorical variables
mydata$Geography <- factor(mydata$Geography)
mydata$Gender <- factor(mydata$Gender)

# convert dummy int types to categorical as well
mydata$HasCrCard <- factor(mydata$HasCrCard)
mydata$IsActiveMember <- factor(mydata$IsActiveMember)
mydata$Exited <- factor(mydata$Exited, levels = c(0, 1), labels = c("0", "1"))


# delete rows that contains missing values
mydata <- na.omit(mydata)

# visualize variables
library(ggplot2)
ggplot(mydata, aes(x = CreditScore, fill=Geography))+
  geom_histogram(binwidth = 10,  alpha = 0.5)+
  labs(title = "Credit Score",
       x = "Credit Score",
       y = "Frequency")

ggplot(mydata, aes(x = Gender , y = Age, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age by Gender", x = "Gender", y = "Age") +
  theme_minimal()

ggplot(mydata, aes(x = Exited, y = Tenure, fill = Exited)) +
  geom_boxplot() +
  labs(title = "Tenure by Exited", x = "Exited", y = "Tenure") +
  theme_minimal()


ggplot(mydata, aes(x = Balance, fill=Exited))+
  geom_histogram(binwidth = 10000,  alpha = 0.5, data = subset(mydata, Balance > 0))+
  labs(title = "Positive Balance by Exited",
       x = "Balance",
       y = "Frequency")


ggplot(mydata, aes(x = EstimatedSalary, fill=Exited))+
  geom_histogram(binwidth = 3000,  alpha = 0.5)+
  labs(title = "Estmated Salary",
       x = "Estimated Salary",
       y = "Frequency")

ggplot(mydata, aes(x = NumOfProducts)) +
  geom_bar(stat = "count", fill = 'blue') +
  labs(title = "Num Of Products", x = "Products", y = "Count") +
  theme_minimal()

ggplot(mydata, aes(x = HasCrCard)) +
  geom_bar(stat = "count", fill = 'red') +
  labs(title = "Has Credit Card", x = "Has Credit Card", y = "Count") +
  theme_minimal()

ggplot(mydata, aes(x = IsActiveMember)) +
  geom_bar(stat = "count", fill = 'brown') +
  labs(title = "Is Active Member", x = "Is Active Member", y = "Count") +
  theme_minimal()

ggplot(mydata, aes(x = Exited)) +
  geom_bar(stat = "count", fill = 'yellow') +
  labs(title = "Exited", x = "Exited", y = "Count") +
  theme_minimal()


# load library for sampling
library(caret)

# set seed for random so that each run gives same result
set.seed(321654)

# split data into train and test sets
# 80% for training, 20% for test
v <- createDataPartition(mydata$Exited, p = 0.80, list = FALSE)

# -c(1, 2, 3) exclude id, CustomerId, Surname variables
# postive vector means include those
mydata_train <- mydata[v,-c(1, 2, 3)] 

# negative vector means exclude those
mydata_test <- mydata[-v,-c(1, 2, 3)]


# build logistic regression model
# model uses 80% of the data i.e. train data
model <- glm(Exited ~ ., data = mydata_train, family =  binomial("logit"))

# show model summary
# stars in each variable means each variable is statistically significant
summary(model)

# check model accuracy
train_predictions <- predict (model, newdata = mydata_train, type = "response")

# convert continuous value to binary
train_class_predictions <- as.numeric(train_predictions > 0.5)

# prediction accuracy for train data
mean(train_class_predictions == mydata_train$Exited)

# prediction accuracy for test data
test_predictions <- predict (model, newdata = mydata_test, type = "response")

# convert continuous value to binary
test_class_predictions <- as.numeric(test_predictions > 0.5)

# prediction accuracy for train data
mean(test_class_predictions == mydata_test$Exited)

# calculate confusion matrix
(confusion_matrix <- table(predicted = train_class_predictions, actual = mydata_train$Exited))

# extract TP, TN, FP, FN
TP <- confusion_matrix[2, 2]
TN <- confusion_matrix[1, 1]
FP <- confusion_matrix[1, 2]
FN <- confusion_matrix[2, 1]

# calculate sensitivity and specificity
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)

# show results
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# sensitivity is 70%. It shows how well model predicts exited cases
# specificity is 85%. It shows how well model predicts not exited cases

# now predict new data set from another file

# read data
mydata_new <- read.csv("NewCustomerDataset.csv")

# convert string to categorical variables
mydata_new$Geography <- factor(mydata_new$Geography)
mydata_new$Gender <- factor(mydata_new$Gender)

# convert dummy int types to categorical as well
mydata_new$HasCrCard <- factor(mydata_new$HasCrCard)
mydata_new$IsActiveMember <- factor(mydata_new$IsActiveMember)

# make prediction using the model
predictions <- predict (model, newdata = mydata_new, type = "response")

# convert continuous value to binary
class_predictions <- as.numeric(predictions > 0.5)

# add prediction to new data as new column 
mydata_new$PredictedExited <- class_predictions 

# save new data along with prediction to file
write.csv(mydata_new,"PredictedExited.csv")