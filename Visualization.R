library(ggplot2)

#How the marital status of the person impacts the chances of Stroke?
ggplot(data = Sample_Data, mapping = aes(x=ever_married))

#Will Smoking lead to high probability of having Stroke?
#Which gender of people are mostly prone to strokes?
#Does the work type have an impact on bmi and hypertension of the person? Do these influence the chances of having a stroke?
#Does age has impact on strokes?
#Does body mass index and glucose levels in a person, propel a stroke?
#As many people say, various preventative steps such as switching to a healthy lifestyle by having a heart healthy diet, aiming for healthy weight, proper stress management, quitting smoking will help us to reduce the risk of having a stroke. Is this statement correct?
  

ggplot(data = Sampled, mapping = aes(x=ever_married, fill=stroke)) + geom_bar()

ggplot(data = Sampled, mapping = aes(x=heart_disease, fill=stroke)) + geom_bar()


