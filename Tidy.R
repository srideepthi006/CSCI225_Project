install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot")
install.packages("xlsx")
library(tidyverse)
library(dplyr)
library(xlsx)

#import dataset
data <- read_csv("/Users/qwerty/Desktop/OneDrive - St. Francis Xavier University/4th Year/CSCI225/CSCI 225 Group Project/Stroke Dataset.csv")

test<-filter(data, work_type=="children")

#checking for empty cells => none
filter(data, gender == "" | age== "" | hypertension== "" | heart_disease== "" | ever_married== "" | work_type== "" |Residence_type == "" | avg_glucose_level== "" |bmi == "" | smoking_status== "" | stroke== "" )

#creating new column for Age Category of each observation -> age<=16 = children age>16 = adult  age>=60 = senior
data["Age_Category"] <- c(1)

data$Age_Category[data$age >16] <- "Adult"

data$Age_Category[data$age <=16] <- "Child"

data$Age_Category[data$age >=60] <- "Senior"
#----------------------------------------------------------------------------------------

#Changing "N/A" and "Unknown" to NA object
data$bmi[data$bmi == "N/A"] <- NA

data$smoking_status[data$smoking_status == "Unknown"] <- NA

data$work_type[data$work_type == "children"] <- NA

#changing bmi column from character to numerical
data$bmi<- as.double(data$bmi)

#calculating mean of bmi grouped by gender
the_mean <- data  %>% select(bmi,gender)%>% filter(!is.na(bmi)) %>% group_by(gender) %>% summarise(mean(bmi))

#replace N/A with bmi by average of gender
data$bmi[data$gender == "Female" & is.na(data$bmi)] <- the_mean[1,2]
data$bmi[data$gender == "Male" & is.na(data$bmi)] <- the_mean[2,2]

#changing bmi column to numerical
data$bmi<- as.double(data$bmi)

#Dealing with Outliers of bmi
data <- subset(data, !(bmi < 12 & Age_Category != "Child"))

data <- subset(data, !(bmi > 50))

# Removing the other data in gender
data <- subset(data, (gender != "Other"))

data$stroke[data$stroke == 0] <- "No Stroke"

data$stroke[data$stroke == 1] <- "Stroke"

data$heart_disease[data$heart_disease == 0] <- "No Heart Disease"

data$heart_disease[data$heart_disease == 1] <- "Heart Disease"


#exporting tidy changes to common csv file
write_csv(data,"C:/Users/19028/Documents/Datascience/Health Analytics/CSCI225_Project/NewStrokeDataset.csv")
