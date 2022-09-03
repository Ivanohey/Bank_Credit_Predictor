#Support Vector Machines
library(e1071)
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(caret)

#We do some basic wrangling
data_log = read.csv("./data/GermanCredit.csv", sep = ";", header = TRUE)
data_log <- data.frame(data_log)
data_log["RESPONSE"][data_log["RESPONSE"] == 0] = "Bad Rating"
data_log["RESPONSE"][data_log["RESPONSE"] == 1] = "Good Rating"

#convert all concerned variables to factors
cols = c(2,4:10,12:22,24:32)
data_log[,cols]= lapply(data_log[,cols], as.factor)

#Remove col"OBS."
data_log = data_log[,-1]

data_log[37,8]=1
data_log[234,18]=1
data_log[537,22]=75

str(data_log)

data <- data_log

#We split the data into train (80%) and test (20%)
set.seed(3456)
trainIndex <- createDataPartition(data$RESPONSE, p = .8,
                                  list = FALSE)
train.df <- data[ trainIndex,]
test.df <- data[-trainIndex,]

#We try using the linear Kernel
svm_model_linear <- svm(RESPONSE ~ ., data = train.df, method = "C-classification",
                        kernel = "linear", cost = 10, gamma = 0.1)

predict_linear <- predict(svm_model_linear, test.df)

confusionMatrix(predict_linear, test.df$RESPONSE, positive = "Good Rating")


#We try using the radial Kernel


C <- c(0.1, 1, 10, 100, 1000, 10000)
sigma <- c(0.0001, 0.001, 0.01, 0.1, 1)
gr.radial<-expand.grid(C = C, sigma = sigma)
model_svm_rad<-train(RESPONSE ~ .,
                          data = train.df,
                          method = "svmRadial",
                          trace=FALSE,
                          trControl = trainControl(method = "cv", number = 10, 
                                                   verboseIter = FALSE),
                          tuneGrid=gr.radial)
model_svm_rad

predict_radial <- predict(model_svm_rad, test.df)

confusionMatrix(predict_radial, test.df$RESPONSE, positive = "Good Rating")

#The svm Radial gave a bad result, we finally decided to not include it in the report... 