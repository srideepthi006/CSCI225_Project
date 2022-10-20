library(tidyverse)
library(dplyr)
library(ggplot2)

head(data, n=5)
summary(data)

summarize(group_by(data, gender), count = n())


#sdgsdg