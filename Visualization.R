library(ggplot2)
library(tidyverse)
ggplot(data = Sampled, mapping = aes(x=ever_married, fill=stroke)) + geom_bar()

ggplot(data = Sampled, mapping = aes(x=heart_disease, fill=stroke)) + geom_bar()

#histogram showing distribution of age corresponding to stroke or not
ggplot(data=newdata, mapping= aes(x=age, fill=stroke)) +
  geom_histogram(binwidth = 15, color="black")

#facet of geom point -> age vs bmi showing stroke or not
ggplot(data = newdata, mapping = aes(x=age, y=bmi))+geom_point(aes(color=stroke))+
  facet_grid(~stroke)

#facet of geom point -> glucose lvl vs bmi showing stroke or not
ggplot(data = newdata, mapping = aes(x=smoking_status))+geom_bar(aes(color=stroke))


#facet of geom point -> age vs bmi showing stroke or not
ggplot(data = StrokeData, mapping = aes(x=age, y=bmi))+geom_point(aes(color=work_type))
  


