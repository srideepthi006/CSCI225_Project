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

data$work_type[data$work_type == "children" | data$work_type== "Never_worked"] <- "Not Working"

#changing bmi column from character to numerical
data$bmi<- as.double(data$bmi)

#calculating mean of bmi grouped by gender
the_mean <- data  %>% select(bmi,gender)%>% filter(!is.na(bmi)) %>% group_by(gender) %>% summarise(mean(bmi))

#replace N/A is bmi by average of gender
data$bmi[data$gender == "Female" & is.na(data$bmi)] <-the_mean[1,2]
data$bmi[data$gender == "Male" & is.na(data$bmi)] <-the_mean[2,2]

#exporting tidy changes to common csv file
write_csv(data,"/Users/qwerty/Desktop/RProject - Isaac/CSCI225_Project/Stroke Dataset.csv")



#nkdsnkl