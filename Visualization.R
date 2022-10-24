library(ggplot2)

ggplot(data = Sampled, mapping = aes(x=ever_married, fill=stroke)) + geom_bar()

ggplot(data = Sampled, mapping = aes(x=heart_disease, fill=stroke)) + geom_bar()
