install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot")

library(tidyverse)
library(dplyr)

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


filter(data, work_type == "Not Working")


#nkdsnkl