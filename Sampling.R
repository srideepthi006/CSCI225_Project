library(ggplot2)
library(xlsx)
library(tidyverse)



newdata <- stroke_data

ggplot(data = newdata, mapping = aes(x=stroke)) + labs(x="Stroke Data", y="Count") + 
  geom_bar(color="white", fill= "darkblue")

NoStrokeData <- subset(newdata, (stroke == 0))

StrokeData <- subset(newdata, (stroke == 1))

rand_ns <- NoStrokeData[sample(nrow(NoStrokeData), size=248), ]

Sampled <- rbind(StrokeData, rand_ns)


ggplot(data = Sampled, mapping = aes(x=stroke)) + labs(x="Stroke Data", y="Count") + 
  geom_bar(color="white", fill= "darkblue")

#exporting sampled data to common csv file
write_csv(Sampled,"C:/Users/19028/Documents/Datascience/Health Analytics/CSCI225_Project/NewStroke_Sample_Dataset.csv")
