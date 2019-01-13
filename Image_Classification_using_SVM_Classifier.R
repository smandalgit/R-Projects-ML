
#Image Classification using SVM classifier
#load Packages
library(e1071)
library(jpeg)
library(parallel)

#loading the images from path
setwd("C:/Users/PC/SVM/Images") #Change and Map your working directory
load_images <- dir("C:/Users/PC/SVM/Images",pattern = "jpeg") #Change Path for Images 
#read images
images <- mclapply(load_images,readJPEG,mc.cores = 1) #for windows os core has to be 1
#Data extraction and manipulation
str(load_images) #provide structure of the loaded images
str(images[1]) #provide structure of first image
#suppose if we want to change the name of loaded image. Step 1:extract the name ids from images
name_ids <- lapply(load_images,function(x) as.character(unlist(strsplit(x,"[(0-9)]"))[1]))
#change the name of the name_ids
name_ids[name_ids=="c"] = 1
name_ids[name_ids!= "1"] = 0
#check whether the name_ids were changed correctly
name_ids
#extract the image_ids
image_ids <- lapply(load_images,function(x) as.numeric(unlist(strsplit(x,"[(a-z)]"))[2]))
#Partition the image data into Train and Test data
#specifiy the image_id limit from where the image data shall be considered for test data
length(image_ids)
train_test_data_limit = round(length(image_ids)*0.6)-1
train_img_data <- unlist(images[image_ids < train_test_data_limit])
length(unlist(images[1]))
train_img_num <- sum(image_ids < train_test_data_limit)
train_img_dimension <- c(length(unlist(images[1])),train_img_num)
#standardizing the dimension of training data
train_img_input <- t(array(train_img_data,dim = train_img_dimension))
str(train_img_input)
#labeled train data
train_img_out <- unlist(name_ids[image_ids < train_test_data_limit])
test_img_data <- unlist(images[image_ids >= train_test_data_limit])
test_img_num <- sum(image_ids >= train_test_data_limit)
test_img_dimension <- c(length(unlist(images[1])),test_img_num)
#standardizing the dimension of test data
test_img_input <- t(array(test_img_data,dim = test_img_dimension))
str(test_img_input)
#labeled test data
test_img_out <- unlist(name_ids[image_ids >= train_test_data_limit])

#train the model usng SVM classifier
svm_train_model <- svm(train_img_input,train_img_out,kernel = "radial",type = "C-classification",probability = TRUE)
#svm_train_model$SV
#SVM evaluation with train input data
pred_train <- predict(svm_train_model,train_img_input,probability = TRUE)
pred_train
y <- matrix(pred_train)
y[y == '1'] = 'car'
y[y == '0'] = 'jet'

x <- train_img_out
x[x == 1] = 'car'
x[x == 0] = 'jet'
confusion_table_train <- table(Train_Img_Out = x,Predicted_Train = y)
accuracy <- sum(diag(confusion_table_train))/sum(confusion_table_train)
print(paste("Prediction Accuracy:-",accuracy*100,"%"))
#SVM evaluation with test input data
pred_test <- predict(svm_train_model,test_img_input,probability = TRUE)
pred_test
p <- matrix(pred_test)
p[p == '1'] = 'car'
p[p == '0'] = 'jet'

q <- test_img_out
q[q == 1] = 'car'
q[q == 0] = 'jet'
confusion_table_test <- table(Test_Img_Out = q,Predicted_Test = p)
accuracy_test <- sum(diag(confusion_table_test))/sum(confusion_table_test)
print(paste("Prediction Accuracy:-",accuracy_test*100,"%"))

library(ROCR)
#For Train Data
prediction_Train <- prediction(as.numeric(matrix(pred_train)),matrix(train_img_out))
performance_Train <- performance(prediction_Train,"tpr","fpr")
plot(performance_Train,main = "ROC Curve for Train Data",xlab = "1-Specificity",ylab = "Senstivity")
abline(a=0,b=1, col = "blue")
# For Test Data
prediction_Test <- prediction(as.numeric(matrix(pred_test)),matrix(test_img_out))
performance_Test <- performance(prediction_Test,"tpr","fpr")
plot(performance_Test,main = "ROC Curve for Test Data",xlab = "1-Specificity",ylab = "Senstivity")
abline(a=0,b=1, col = "red")
