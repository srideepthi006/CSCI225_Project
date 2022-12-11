<<<<<<< HEAD
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot")
install.packages("xlsx")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(xlsx)


#import data set
data <- read_csv("/Users/qwerty/Desktop/OneDrive - St. Francis Xavier University/4th Year/CSCI225/CSCI 225 Group Project/Stroke Dataset.csv")

str(data)
summary(data)
head(data)
tail(data)

data %>% count(data$gender)
data %>% count(data$hypertension)
data %>% count(data$heart_disease)
data %>% count(data$ever_married)
data %>% count(data$work_type)
data %>% count(data$Residence_type)
data %>% count(data$smoking_status)
data %>% count(data$stroke)

#checking for empty and na cells
filter(data, gender == "" | age== "" | hypertension== "" | heart_disease== "" | ever_married== "" | work_type== "" |Residence_type == "" | avg_glucose_level== "" |bmi == "" | smoking_status== "" | stroke== "" | is.na(data$gender) == "TRUE" | is.na(data$age) == "TRUE" | 
         is.na(data$hypertension) == "TRUE" | is.na(data$heart_disease) == "TRUE" | 
         is.na(data$ever_married) == "TRUE" | is.na(data$work_type) == "TRUE" | 
         is.na(data$Residence_type ) == "TRUE" | is.na(data$avg_glucose_level) == "TRUE" | 
         is.na(data$bmi ) == "TRUE" | is.na(data$smoking_status) == "TRUE" | 
         is.na(data$stroke) == "TRUE" )

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

#replace the unknown values of smoking_status with the most frequent category ‘never smoked’
data <- data %>% mutate(smoking_status = replace(smoking_status, is.na(smoking_status), "never smoked"))

#creating new column for Age Category of each observation -> age<=16 = children, age>16 = adult,  age>=60 = senior

data$Age_Category <- with(data, ifelse(age >= 60, 'Senior',
                                       ifelse(age > 16, 'Adult', 'Child')))


#creating new column for BMI_Category
#Underweight	< 18.5	    ----  0
#Normal Weight	18.5 - 24.9		    ----  1
#Overweight	25.0 - 29.9		    ----  2
#Obese class I	30.0 - 34.9		    ----  3
#Obese class II	35.0 - 39.9		    ----  4
#Obese class III	>= 40.0		    ----  5

data$BMI_Category <- with(data, ifelse(bmi >= 40, 5,
                                       ifelse(bmi > 35.0, 4,
                                              ifelse(bmi > 30.0, 3, 
                                                     ifelse(bmi > 25.0, 2, 
                                                            ifelse(bmi > 18.5, 1, 0))))))

data %>% count(data$BMI_Category)

#creating new column for Glucose_Category
#Diabetes	126 mg/dL or above  --  2
#Prediabetes	100 – 125 mg/dL	  --  1
#Normal	99 mg/dL or below	  --  0

data$Glucose_Category <- with(data, ifelse(avg_glucose_level >= 126.0, 2, 
                                           ifelse(avg_glucose_level >= 100, 1, 0)))

summary(data)
sapply(data, class)

#Dealing with Outliers of bmi
data <- subset(data, !(bmi < 12 & Age_Category != "Child"))

data <- subset(data, !(bmi > 50))

# Removing the other data in gender
data <- subset(data, (gender != "Other"))

# Drop ID column.
data= subset(data, select = -c(id))
summary(data)
sapply(data, class)

stroke_data <- data

# gender: Male for 0, Female for 1
data$gender[data$gender == "Male"] <- 0
data$gender[data$gender == "Female"] <- 1
data$gender <- as.double(data$gender)

# ever_married: No for 0, Yes for 1
data$ever_married <- as.character(data$ever_married)
data$ever_married[data$ever_married == "Yes"] <- 1
data$ever_married[data$ever_married == "No"] <- 0
data$ever_married <- as.double(data$ever_married)

#work_type govtjob - 4, never worked - 3, private - 2, self employed - 1, na - 0
data$work_type[data$work_type == "Govt_job"] <- 4
data$work_type[data$work_type == "Never_worked"] <- 3
data$work_type[data$work_type == "Private"] <- 2
data$work_type[data$work_type == "Self-employed"] <- 1
data <- data %>% mutate(work_type = replace(work_type, is.na(work_type), 0))
data$work_type<- as.double(data$work_type)

# Residence_type: Urban for 0, Rural for 1
data$Residence_type<- as.character(data$Residence_type)
data$Residence_type[data$Residence_type == "Urban"] <- 0
data$Residence_type[data$Residence_type == "Rural"] <- 1
data$Residence_type<- as.double(data$Residence_type)

#smoking status never smoked  - 0, formerly smoked - 1, smokes - 2
data$smoking_status[data$smoking_status == "formerly smoked"] <- 1
data$smoking_status[data$smoking_status == "never smoked"] <- 0
data$smoking_status[data$smoking_status == "smokes"] <- 2
data$smoking_status<- as.double(data$smoking_status)

# Age_category Senior - 2, Adult - 1, Child - 0
data$Age_Category[data$Age_Category == "Senior"] <- 2
data$Age_Category[data$Age_Category == "Adult"] <- 1
data$Age_Category[data$Age_Category == "Child"] <- 0
data$Age_Category<- as.double(data$Age_Category)

# Checking for null values
colSums(is.na(data))

sapply(data, class)

data_copy <- data

sapply(stroke_data, class)

stroke_data$stroke <- as.factor(stroke_data$stroke)
stroke_data$hypertension <- as.factor(stroke_data$hypertension)
stroke_data$heart_disease <- as.factor(stroke_data$heart_disease)


#BMI_Category
#Underweight	< 18.5	    ----  0
#Normal Weight	18.5 - 24.9		    ----  1
#Overweight	25.0 - 29.9		    ----  2
#Obese class I	30.0 - 34.9		    ----  3
#Obese class II	35.0 - 39.9		    ----  4
#Obese class III	>= 40.0		    ----  5
stroke_data$BMI_Category <- as.character(stroke_data$BMI_Category)
stroke_data$BMI_Category[stroke_data$BMI_Category == 5] <- 'Obese Class III'
stroke_data$BMI_Category[stroke_data$BMI_Category == 4] <- 'Obese Class II'
stroke_data$BMI_Category[stroke_data$BMI_Category == 3] <- 'Obese Class I'
stroke_data$BMI_Category[stroke_data$BMI_Category == 2] <- 'Overweight'
stroke_data$BMI_Category[stroke_data$BMI_Category == 1] <- 'Normalweight'
stroke_data$BMI_Category[stroke_data$BMI_Category == 0] <- 'Underweight'

data %>% count(stroke_data$BMI_Category)


#Glucose_Category
#Diabetes	126 mg/dL or above  --  2
#Prediabetes	100 – 125 mg/dL	  --  1
#Normal	99 mg/dL or below	  --  0

stroke_data$Glucose_Category <- as.character(stroke_data$Glucose_Category)
stroke_data$Glucose_Category[stroke_data$Glucose_Category == 2] <- 'Diabetes'
stroke_data$Glucose_Category[stroke_data$Glucose_Category == 1] <- 'Prediabetes'
stroke_data$Glucose_Category[stroke_data$Glucose_Category == 0] <- 'Normal'


data %>% count(stroke_data$Glucose_Category)

sapply(stroke_data, class)

NoStrokeData <- subset(stroke_data, (stroke == 0))

StrokeData <- subset(stroke_data, (stroke == 1))

stroke_cor = round(cor(subset(data_copy, select = -c(age, avg_glucose_level, bmi))),2)
ggplot(data = reshape2::melt(stroke_cor),aes(x=Var1, y=Var2, fill=value)) + geom_tile() +  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) + theme(axis.text.x = element_text(angle = 90))

write.csv(data,"/Users/qwerty/Desktop/CSCI225_Project/data_numeric.csv", row.names = FALSE)
#=======
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot")
install.packages("xlsx")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(xlsx)


#import data set
data <- read_csv("C:/Users/19028/Documents/Datascience/Health Analytics/CSCI225_Project/Stroke Dataset.csv")

str(data)
summary(data)
head(data)
tail(data)

data %>% count(data$gender)
data %>% count(data$hypertension)
data %>% count(data$heart_disease)
data %>% count(data$ever_married)
data %>% count(data$work_type)
data %>% count(data$Residence_type)
data %>% count(data$smoking_status)
data %>% count(data$stroke)

#checking for empty and na cells
filter(data, gender == "" | age== "" | hypertension== "" | heart_disease== "" | ever_married== "" | work_type== "" |Residence_type == "" | avg_glucose_level== "" |bmi == "" | smoking_status== "" | stroke== "" | is.na(data$gender) == "TRUE" | is.na(data$age) == "TRUE" | 
         is.na(data$hypertension) == "TRUE" | is.na(data$heart_disease) == "TRUE" | 
         is.na(data$ever_married) == "TRUE" | is.na(data$work_type) == "TRUE" | 
         is.na(data$Residence_type ) == "TRUE" | is.na(data$avg_glucose_level) == "TRUE" | 
         is.na(data$bmi ) == "TRUE" | is.na(data$smoking_status) == "TRUE" | 
         is.na(data$stroke) == "TRUE" )

#Changing "N/A" and "Unknown" to NA object
data$bmi[data$bmi == "N/A"] <- NA

data$smoking_status[data$smoking_status == "Unknown"] <- NA
le

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

#replace the unknown values of smoking_status with the most frequent category ‘never smoked’
data <- data %>% mutate(smoking_status = replace(smoking_status, is.na(smoking_status), "never smoked"))

#creating new column for Age Category of each observation -> age<=16 = children, age>16 = adult,  age>=60 = senior

data$Age_Category <- with(data, ifelse(age >= 60, 'Senior',
                                       ifelse(age > 16, 'Adult', 'Child')))


#creating new column for BMI_Category

#Underweight	< 18.5	           ----  0
#Normal Weight	18.5 - 24.9		    ----  1
#Overweight	25.0 - 29.9		           ----  2
#Obese class I	30.0 - 34.9		       ----  3
#Obese class II	35.0 - 39.9		     ----  4
#Obese class III	>= 40.0		       ----  5

data$BMI_Category <- with(data, ifelse(bmi >= 40, 5,
                                       ifelse(bmi > 35.0, 4,
                                              ifelse(bmi > 30.0, 3, 
                                                     ifelse(bmi > 25.0, 2, 
                                                            ifelse(bmi > 18.5, 1, 0))))))

data %>% count(data$BMI_Category)

#creating new column for Glucose_Category
#Diabetes	126 mg/dL or above  --  2
#Prediabetes	100 – 125 mg/dL	  --  1
#Normal	99 mg/dL or below	  --  0

data$Glucose_Category <- with(data, ifelse(avg_glucose_level >= 126.0, 2, 
                                           ifelse(avg_glucose_level >= 100, 1, 0)))

summary(data)
sapply(data, class)

#Dealing with Outliers of bmi
data <- subset(data, !(bmi < 12 & Age_Category != "Child"))

data <- subset(data, !(bmi > 50))

# Removing the other data in gender
data <- subset(data, (gender != "Other"))

# Drop ID column.
data= subset(data, select = -c(id))
summary(data)
sapply(data, class)

stroke_data <- data

# gender: Male for 0, Female for 1
data$gender[data$gender == "Male"] <- 0
data$gender[data$gender == "Female"] <- 1
data$gender <- as.double(data$gender)

# ever_married: No for 0, Yes for 1
data$ever_married <- as.character(data$ever_married)
data$ever_married[data$ever_married == "Yes"] <- 1
data$ever_married[data$ever_married == "No"] <- 0
data$ever_married <- as.double(data$ever_married)

#work_type govtjob - 4, never worked - 3, private - 2, self employed - 1, na - 0
data$work_type[data$work_type == "Govt_job"] <- 4
data$work_type[data$work_type == "Never_worked"] <- 3
data$work_type[data$work_type == "Private"] <- 2
data$work_type[data$work_type == "Self-employed"] <- 1
data <- data %>% mutate(work_type = replace(work_type, is.na(work_type), 0))
data$work_type<- as.double(data$work_type)

# Residence_type: Urban for 0, Rural for 1
data$Residence_type<- as.character(data$Residence_type)
data$Residence_type[data$Residence_type == "Urban"] <- 0
data$Residence_type[data$Residence_type == "Rural"] <- 1
data$Residence_type<- as.double(data$Residence_type)

#smoking status never smoked  - 0, formerly smoked - 1, smokes - 2
data$smoking_status[data$smoking_status == "formerly smoked"] <- 1
data$smoking_status[data$smoking_status == "never smoked"] <- 0
data$smoking_status[data$smoking_status == "smokes"] <- 2
data$smoking_status<- as.double(data$smoking_status)

# Age_category Senior - 2, Adult - 1, Child - 0
data$Age_Category[data$Age_Category == "Senior"] <- 2
data$Age_Category[data$Age_Category == "Adult"] <- 1
data$Age_Category[data$Age_Category == "Child"] <- 0
data$Age_Category<- as.double(data$Age_Category)

# Checking for null values
colSums(is.na(data))

sapply(data, class)

data_copy <- data

sapply(stroke_data, class)

stroke_data$stroke <- as.factor(stroke_data$stroke)
stroke_data$hypertension <- as.factor(stroke_data$hypertension)
stroke_data$heart_disease <- as.factor(stroke_data$heart_disease)


#BMI_Category
#Underweight	< 18.5	    ----  0
#Normal Weight	18.5 - 24.9		    ----  1
#Overweight	25.0 - 29.9		    ----  2
#Obese class I	30.0 - 34.9		    ----  3
#Obese class II	35.0 - 39.9		    ----  4
#Obese class III	>= 40.0		    ----  5
stroke_data$BMI_Category <- as.character(stroke_data$BMI_Category)
stroke_data$BMI_Category[stroke_data$BMI_Category == 5] <- 'Obese Class III'
stroke_data$BMI_Category[stroke_data$BMI_Category == 4] <- 'Obese Class II'
stroke_data$BMI_Category[stroke_data$BMI_Category == 3] <- 'Obese Class I'
stroke_data$BMI_Category[stroke_data$BMI_Category == 2] <- 'Overweight'
stroke_data$BMI_Category[stroke_data$BMI_Category == 1] <- 'Normalweight'
stroke_data$BMI_Category[stroke_data$BMI_Category == 0] <- 'Underweight'

data %>% count(stroke_data$BMI_Category)


#Glucose_Category
#Diabetes	126 mg/dL or above  --  2
#Prediabetes	100 – 125 mg/dL	  --  1
#Normal	99 mg/dL or below	  --  0

stroke_data$Glucose_Category <- as.character(stroke_data$Glucose_Category)
stroke_data$Glucose_Category[stroke_data$Glucose_Category == 2] <- 'Diabetes'
stroke_data$Glucose_Category[stroke_data$Glucose_Category == 1] <- 'Prediabetes'
stroke_data$Glucose_Category[stroke_data$Glucose_Category == 0] <- 'Normal'


data %>% count(stroke_data$Glucose_Category)

sapply(stroke_data, class)

NoStrokeData <- subset(stroke_data, (stroke == 0))

StrokeData <- subset(stroke_data, (stroke == 1))

stroke_cor = round(cor(subset(data_copy, select = -c(age, avg_glucose_level, bmi))),2)
ggplot(data = reshape2::melt(stroke_cor),aes(x=Var1, y=Var2, fill=value)) + geom_tile() +  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) + theme(axis.text.x = element_text(angle = 90))


stroke_data$stroke <- as.character(stroke_data$stroke)
stroke_data$heart_disease <- as.character(stroke_data$heart_disease)
stroke_data$hypertension <- as.character(stroke_data$hypertension)
stroke_data$smoking_status <- as.character(stroke_data$smoking_status)
stroke_data$work_type <- as.character(stroke_data$work_type)

stroke_data$stroke[stroke_data$stroke == 0] <- "No Stroke"

stroke_data$stroke[stroke_data$stroke == 1] <- "Stroke"

stroke_data$heart_disease[stroke_data$heart_disease == 0] <- "No Heart Disease"

stroke_data$heart_disease[stroke_data$heart_disease == 1] <- "Heart Disease"

stroke_data$hypertension[stroke_data$hypertension == 0] <- "No Hypertension"

stroke_data$hypertension[stroke_data$hypertension == 1] <- "Hypertension"

stroke_data$smoking_status[stroke_data$smoking_status == "formerly smoked"] <- "Formerly Smoked"

stroke_data$smoking_status[stroke_data$smoking_status == "never smoked"] <- "Never Smoked"

stroke_data$smoking_status[stroke_data$smoking_status == "smokes"] <- "Smokes"

stroke_data$work_type[stroke_data$work_type == "Self-employed"] <- "Self Employed"

stroke_data$work_type[stroke_data$work_type == "Govt_job"] <- "Govt Job"
>>>>>>> 37b66235976dcab29d3f36749fb92aee5115a7b2
