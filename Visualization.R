library(ggplot2)
<<<<<<< HEAD
library(tidyverse)
=======

#How the marital status of the person impacts the chances of Stroke?
ggplot(data = Sample_Data, mapping = aes(x=ever_married))

#Will Smoking lead to high probability of having Stroke?
#Which gender of people are mostly prone to strokes?
#Does the work type have an impact on bmi and hypertension of the person? Do these influence the chances of having a stroke?
#Does age has impact on strokes?
#Does body mass index and glucose levels in a person, propel a stroke?
#As many people say, various preventative steps such as switching to a healthy lifestyle by having a heart healthy diet, aiming for healthy weight, proper stress management, quitting smoking will help us to reduce the risk of having a stroke. Is this statement correct?
  

>>>>>>> 137ed57466a791306e71d11bba737d6236d7d291
ggplot(data = Sampled, mapping = aes(x=ever_married, fill=stroke)) + geom_bar()

ggplot(data = Sampled, mapping = aes(x=heart_disease, fill=stroke)) + geom_bar()

<<<<<<< HEAD
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
  

=======
>>>>>>> 137ed57466a791306e71d11bba737d6236d7d291

