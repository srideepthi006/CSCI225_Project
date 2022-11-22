install.packages("reshape")
library(ggplot2)
library(dplyr)
library(reshape)
library(readr)
library(scales)

str(NewStrokeDataset)

#Converting categorical to numerical values

newdata <- NewStrokeDataset %>% mutate_at(c('gender','hypertension','heart_disease','ever_married',
                                            'work_type','Residence_type','smoking_status','stroke',
                                            'Age_Category'), as.factor) %>% mutate_if(is.factor, as.numeric)

newdate <- newdata (na.rm=TRUE)

#newdata <- sapply(NewStrokeDataset,unclass)

View(newdata)


#Preparing heatmap to show comparison and impact of multiple variables

str(newdata)

#Preparing correlation matrix and data melt and rounding the values to 2 decimal places
data <- round(cor(newdata[sapply(newdata, is.numeric)]),2)
data <- melt(data, na.rm=TRUE)

ggplot(data, aes(x = X1, gender, y = X2, gender, fill = value))+geom_tile()+
  scale_fill_gradient2(low = "orange", high = "red", mid = "white")+
  geom_text(aes(X2, X1, label = value), color = "black")

