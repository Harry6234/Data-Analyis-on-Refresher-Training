library(ggplot2)
library(tidyverse)
library(readxl)
library(janitor)


getwd()

survey <- read_excel("Questionaire.xlsx")

#------------------- Exploring Data ------------------------

# 1. viewing the column labels and column head
colnames(survey)
# checking the number of rows
nrow(survey)

# checking the number of columns
ncol(survey)

#  A quick glance through the data
head(survey, n=10)
tail(survey, n=10)

# looking at the structure of the data
str(survey)

# Summary of the data
summary(survey)
survey[1:10]

#  -------------Lets drop some the columns not needed 
#  since I am working with a copy of the data set -------------

survey$ID <- NULL
survey$`Completion time` <- NULL
survey$Email <- NULL
survey$`Start time` <- NULL
survey$Name <- NULL
survey$`Questionaire Number` <- NULL
# A quick skim through the changes now
skimr::skim(survey)

# ------Filtering respondents who have had similar training
had_similar_training <- survey$`Have you attended any training of this sort before?`== "Yes"
survey[had_similar_training,]

qplot(data=survey, x=`Have you attended any training of this sort before?`, y= )



# ------Filtering respondents who have not had similar training
no_similar_training <- survey$`Have you attended any training of this sort before?`=="No"
survey[no_similar_training,]


#Cross tab analysis with percentages.
tabyl(survey, College, Designation) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1)

CGPfunctions::PlotXTabs2(survey,Designation, College,results.subtitle = FALSE)
CGPfunctions::PlotXTabs2(student_data,Category,Department,results.subtitle = FALSE)

# Filtering respondents
table(survey$`Have you attended any training of this sort before?`)

#attended a similar training
attended_similar <- (subset(survey, survey$`Have you attended any training of this sort before?`== "Yes"))

#Never attended a similar training
not_attended_similar <- (subset(survey, survey$`Have you attended any training of this sort before?`== "No"))


ggplot(survey, aes(Designation, `The training session was useful to my work`,  fill=`Have you attended any training of this sort before?`)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Have you attended any training of this sort before? Vs The training session was useful to my work",
       caption = "Source: Survey Response From Refresher Training,University of Cape Coast; August 2022")


g <- ggplot(survey, aes(`Using the UCCOSIS improves the quality of my task/work`))
g + geom_bar(aes(fill=College), width = 0.5) + 
  theme(axis.text.x = element_text(angle=0, vjust=0.5)) +
  labs(title="Using the UCCOSIS improves the quality of my task/work", 
      subtitle="Note: NA implies No Answer", 
       caption="Source: Survey Response From Refresher Training,University of Cape Coast; August 2022")

b <- ggplot(survey, aes(Designation))
b + geom_bar(aes(fill=College), width = 0.5) + 
  theme(axis.text.x = element_text(angle=0, vjust=0.5)) +
  labs(title="Using the UCCOSIS improves the quality of my task/work", 
       subtitle="Note: NA implies No Answer", 
       caption="Source: Survey Response From Refresher Training,University of Cape Coast; August 2022")


b + geom_bar(aes(fill=College), width = 0.5)+
  theme(axis.text.x = element_text(angle=0, vjust=0.5)) +
  labs(title="Using the UCCOSIS improves the quality of my task/work", 
       subtitle="Note: NA implies No Answer", 
       caption="Source: Survey Response From Refresher Training,University of Cape Coast; August 2022")


c <- ggplot(survey, aes(College))
c + geom_bar(aes(fill=), width = 0.5)+
    theme(axis.text.x = element_text(angle=0, vjust=0.5)) +
  labs(title="Using the UCCOSIS improves the quality of my task/work", 
       subtitle="Note: NA implies No Answer", 
       caption="Source: Survey Response From Refresher Training,University of Cape Coast; August 2022")

