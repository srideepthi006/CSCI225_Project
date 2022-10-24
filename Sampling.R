library(ggplot2)
library(xlsx)


Data_Mod <- read_csv("C:/Users/19028/Documents/Datascience/Health Analytics/CSCI225_Project/NewStrokeDataset.csv")

ggplot(data = Data_Mod, mapping = aes(x=stroke)) + labs(x="Stroke Data", y="Count") + 
  geom_bar(color="white", fill= "darkblue")

NoStroke_Data <- subset(Data_Mod, (stroke == "No Stroke"))

Stroke_Data <- subset(Data_Mod, (stroke == "Stroke"))

Rand_NoStroke <- NoStroke_Data[sample(nrow(NoStroke_Data), size=nrow(Stroke_Data)), ]

Sample_Data <- rbind(Stroke_Data, Rand_NoStroke)

ggplot(data = Sample_Data, mapping = aes(x=stroke)) + labs(x="Stroke Data", y="Count") + 
  geom_bar(color="white", fill= "darkblue")

#exporting sampled data to common csv file
write_csv(Sample_Data,"C:/Users/19028/Documents/Datascience/Health Analytics/CSCI225_Project/NewStroke_Sample_Dataset.csv")
