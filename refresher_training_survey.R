library(ggplot2)
library(tidyverse)
library(readxl)

survey <- read_excel("Questionaire.xlsx")
names(survey)

# Filtering respondents
table(survey$`Have you attended any training of this sort before?`)

#attended a similar training
attended_similar <- (subset(survey, survey$`Have you attended any training of this sort before?`== "Yes"))

#Never attended a similar training
not_attended_similar <- (subset(survey, survey$`Have you attended any training of this sort before?`== "No"))

ggplot(survey, aes(`Have you attended any training of this sort before?`, y = Designation)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Multiple Bar plots")


ggplot(survey, aes(Designation, `The training session was useful to my work`,  fill=`Have you attended any training of this sort before?`)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Have you attended any training of this sort before? Vs The training session was useful to my work",
       caption = "Source: Survey Response From Refresher Training, August 2022 ")



g <- ggplot(attended_similar, aes(`The internet network does not support effective use of UCCOSIS`)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=`The UCCOSIS supports quality student records management`), 
                   binwidth = .1, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="Engine Displacement across Vehicle Classes")  
g + geom_histogram(aes(fill=class), 
                   bins=5, 
                   col="black", 
                   size=.1) +   # change number of bins
  labs(title="Histogram with Fixed Bins", 
       subtitle="Engine Displacement across Vehicle Classes") 
