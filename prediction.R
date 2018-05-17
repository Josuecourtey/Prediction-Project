#Section 1: Summary
#This model uses decision tree analysis to classify and predict different body movements.  
#Preprocessing and cleaning of the data was mostly visual, inspecting the amount of observations.

rm(list=ls()) #Clears previous variables
library(rpart) #Loads rpart library
filename<-"training.csv" #Loads training data
Dataset<-read.csv(filename) #Reads training data
dim(Dataset) #Analyzes dataset for further division


#Section 2: Model
#A random sample of 7000 of the 19622 observations was used for training data.
#The model is not perfect by any means but it correcting guesses a majority of the movements as seen in the table at the end of section 2.
s<- sample(19622, 7000) #Randomly samples for training and testing data
data_train <- Dataset[s,] #Partitions training data
data_test <- Dataset[-s,] #Partitions testing data 

#Fits the decision tree model
dtm<- rpart(classe~ gyros_forearm_y+accel_forearm_x+ accel_forearm_y+ accel_forearm_z+ gyros_forearm_x+ gyros_forearm_z+ gyros_belt_x+gyros_belt_y+gyros_belt_z+accel_belt_x+accel_belt_y+accel_belt_z+gyros_arm_x+gyros_arm_y+gyros_arm_z+accel_arm_x+accel_arm_y+accel_arm_z+roll_belt+pitch_belt+yaw_belt+total_accel_belt+roll_arm+pitch_arm+yaw_arm+total_accel_arm+roll_forearm+pitch_forearm+yaw_forearm, data_train, method="class")

plot(dtm) #Plots the decision tree
text(dtm) #Adds text to the plot

#Fits model to testing data
p<-predict(dtm, data_test, type="class")

#Creates chart displaying the accuracy
table(data_test[,"classe"], p)


#Section 3: Output of predictive data
#I decided on this model by adding differnt variables to the decision tree model and comparing the results.
filename1<-"testing.csv" #Loads testing data of 20 inputs
Dataset1<-read.csv(filename1) #Reads testing data
p1<-predict(dtm, Dataset1) #Predicts outputs based on fitted model
p1 #Displays output

#The following are the model's predictions of the classes:
##1:D 
##2:A
##3:C
##4:C
##5:A
##6:E
##7:D
##8:B
##9:A
#10:A
#11:C
#12:C
#13:B
#14:A
#15:C
#16:D
#17:A
#18:B
#19:D
#20:B
