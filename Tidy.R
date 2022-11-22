install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot")
install.packages("xlsx")
library(tidyverse)
library(dplyr)
library(xlsx)

#import dataset
data <- read_csv("C:/Users/19028/Documents/Datascience/Health Analytics/CSCI225_Project/Stroke Dataset.csv")

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

<<<<<<< HEAD
#exporting tidy changes to common csv file
write_csv(data,"/Users/qwerty/Desktop/CSCI225_Project/Stroke Dataset.csv")

filter(data)
=======
#changing bmi column to numerical
data$bmi<- as.double(data$bmi)

#Dealing with Outliers of bmi
data <- subset(data, !(bmi < 12 & Age_Category != "Child"))

data <- subset(data, !(bmi > 50))

# Removing the other data in gender
data <- subset(data, (gender != "Other"))

# Changing the (binary) values to appropriate strings

data$stroke[data$stroke == 0] <- "No Stroke"

data$stroke[data$stroke == 1] <- "Stroke"

data$heart_disease[data$heart_disease == 0] <- "No Heart Disease"

data$heart_disease[data$heart_disease == 1] <- "Heart Disease"
#>>>>>>> 5a1c662aa14724959dee87859e4da01aceed0760

data$hypertension[data$hypertension == 0] <- "No Hypertension"

data$hypertension[data$hypertension == 1] <- "Hypertension"

data$smoking_status[data$smoking_status == "formerly smoked"] <- "Formerly Smoked"

data$smoking_status[data$smoking_status == "never smoked"] <- "Never Smoked"

data$smoking_status[data$smoking_status == "smokes"] <- "Smokes"

data$work_type[data$work_type == "Self-employed"] <- "Self Employed"

data$work_type[data$work_type == "Govt_job"] <- "Govt Job"


#exporting tidy changes to common csv file
write_csv(data,"C:/Users/19028/Documents/Datascience/Health Analytics/CSCI225_Project/NewStrokeDataset.csv")



