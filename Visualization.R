install.packages('FSinR')
library(FSinR)
library(ggplot2)
library(readr)
library(tidyverse)


#How the marital status of the person impacts the chances of Stroke?
ggplot(data = Sampled, mapping = aes(x=ever_married))

#Will Smoking lead to high probability of having Stroke?

#Does the work type have an impact on bmi and hypertension of the person? Do these influence the chances of having a stroke?
#Does body mass index and glucose levels in a person, propel a stroke?
#As many people say, various preventative steps such as switching to a healthy lifestyle by having a heart healthy diet, aiming for healthy weight, proper stress management, quitting smoking will help us to reduce the risk of having a stroke. Is this statement correct?
  
SelectKBest(NewStrokeDataset, 'stroke', roughsetConsistency, 1)

ggplot(data = Sampled, mapping = aes(x=ever_married, fill=stroke)) + geom_bar()

ggplot(data = Sampled, mapping = aes(x=heart_disease, fill=stroke)) + geom_bar()


#---------------------------------------------------------------------------------------
#Does age have an impact on the abundance of strokes?


#histogram showing distribution of age corresponding to stroke or not
ggplot(filter(stroke_data, stroke=="Stroke"), mapping= aes(x=age, fill=stroke)) +
  geom_histogram(binwidth = 5,color="black")+
  labs(title = "Age Distribution of Recorded Stroke Cases - Left Skewed")

#covariation btwn stroke status and age category (descriptive)
Sampled %>% 
  count(stroke, Age_Category) %>%  
  ggplot(mapping = aes(x = stroke, y = Age_Category)) +
  geom_tile(mapping = aes(fill = n))+
  xlab("Stroke Status")+
  ylab("Age Category")+
  labs(title= "Strokes are Alot More Common Amongst Seniors")

#---------------------------------------------------------------------------------------
#Which gender of people are mostly prone to strokes?


#boxplot to show stroke distribution amongst males and females
ggplot(data=StrokeData, mapping = aes(x=gender, y=age, group=gender))+ 
  geom_boxplot(aes(fill=gender))
#Bar chart to show count of strokes by gender in the samples data
ggplot(StrokeData,mapping =aes(x=gender))+
  geom_bar()


#---------------------------------------------------------------------------------------
#Exploratory data analysis Below to answer further questions

#---------------------------------------------------------------------------------------
#Does the work type have an impact on bmi and hypertension of the person? Do these influence the chances of having a stroke?
  
#Relationship between work type and bmi (no relationship)
ggplot(data = Sampled, mapping = aes(x = bmi,y=..density..)) + 
  geom_freqpoly(mapping = aes(colour = work_type), binwidth = 2)+
  labs(title = "No interesting Pattern Between Work Type and BMI")

#Relationship between work type and hypertension (no relationship)
Sampled %>% 
  count(work_type, hypertension) %>%  
  ggplot(mapping = aes(x = work_type, y = hypertension)) +
  geom_tile(mapping = aes(fill = n))
#covariation btwn hypertension and stoke
Sampled %>% 
  count(stroke, hypertension) %>%  
  ggplot(mapping = aes(x = stroke, y = hypertension)) +
  geom_tile(mapping = aes(fill = n))

#---------------------------------------------------------------------------------------
#Will Smoking lead to high probability of having Stroke?

#covar btwn smoking status & stroke
Sampled %>% 
  count(stroke, smoking_status) %>%  
  ggplot(mapping = aes(x = stroke, y = smoking_status)) +
  geom_tile(mapping = aes(fill = n))






#covariation btwn heart_disease & stroke
Sampled %>% 
  count(stroke, heart_disease) %>%  
  ggplot(mapping = aes(x = stroke, y = heart_disease)) +
  geom_tile(mapping = aes(fill = n))





#covar btwn worktype & stroke(Descriptive)
Sampled %>% 
  count(stroke, work_type) %>%  
  ggplot(mapping = aes(x = stroke, y = work_type)) +
  geom_tile(mapping = aes(fill = n))


#covar btwn martial status & stroke
filter(Sampled, age>30) %>% 
  count(stroke, ever_married) %>%  
  ggplot(mapping = aes(x = stroke, y = ever_married)) +
  geom_tile(mapping = aes(fill = n))

#age vs glucose lvl(descriptive)
ggplot(data = NewStrokeDataset) +
  geom_hex(mapping = aes(x = age, y = avg_glucose_level))

#age vs bmi
ggplot(data = NewStrokeDataset) +
  geom_point(mapping = aes(x = age, y = bmi))


#glucose level vs stroke(descriptive)
ggplot(data = NewStrokeDataset, mapping = aes(x = avg_glucose_level, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = stroke), binwidth = 10)
#Glucose lvl vs stroke with Zoom (descriptive)
ggplot(data = NewStrokeDataset, mapping = aes(x = avg_glucose_level, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = stroke), binwidth = 10)+
  coord_cartesian(xlim = c(30, 160), )+xlab("Average Glucose Level")

#bmi vs stroke(descriptive)
ggplot(data = Sampled, mapping = aes(x = bmi, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = stroke), binwidth = 1)+
  labs(title = "Many Stroke Cases are Occuring to Overweight People")


#age vs stroke
ggplot(data = NewStrokeDataset, mapping = aes(x = age, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = stroke), binwidth = 5)+
  labs(title="Large Frequency of Strokes Amongst Seniors")

#-->for individuals hove have blood glucose levl below 150 and stroke==yes, what are their ages(descriptive)
strokeAndlessThan150<-filter(NewStrokeDataset, avg_glucose_level<150 & stroke=="Stroke")

#--> plot of age distribution of of ppl with normal glucose and have a stroke(descriptive)
ggplot(data=strokeAndlessThan150,mapping = aes(x=age, fill=stroke))+
  geom_histogram(binwidth = 5)+ggtitle("Age distribution of people having strokes with glocose levels<150")

#--> plot of bmi distribution of of ppl with normal glucose and have a stroke(descriptive)
ggplot(data=strokeAndlessThan150,mapping = aes(x=bmi, fill=stroke))+
  geom_histogram(binwidth = 2)+ggtitle("")


#
strokeAndlessThan150 %>% 
  count(stroke, work_type) %>%  
  ggplot(mapping = aes(x = stroke, y = work_type)) +
  geom_tile(mapping = aes(fill = n)) +ggtitle("Stroke Status vs Work Type & normal glucose lvls")


#bmi-glucose lvl correlation
ggplot(data = StrokeData, mapping = aes(x=avg_glucose_level, y=bmi))+geom_bin2d()














#facet of geom point -> age vs bmi showing stroke or not
ggplot(data = stroke_data, mapping = aes(x=age, y=bmi))+geom_point(aes(color=stroke))+
  facet_grid(~stroke)

#facet of geom point -> glucose lvl vs bmi showing stroke or not
ggplot(data = Sampled, mapping = aes(x=smoking_status,fill=stroke))+geom_bar(position = "fill")


#facet of geom point -> age vs bmi showing stroke or not
ggplot(data = filter(Sampled, stroke=="1"), mapping = aes(x=age, y=bmi))+geom_point(aes(color=work_type))


#geom point age vs bmi showing which individuals have hypertension/no hypertension and stroke/no stroke
ggplot(data = StrokeData, mapping = aes(x=age, y=bmi))+geom_point(aes(color=hypertension, shape=stroke))


ggplot(data = newdata, mapping = aes(x=stroke, fill = hypertension))+geom_bar(position = "fill")







  

