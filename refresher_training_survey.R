library(ggplot2)
library(tidyverse)
library(readxl)
library(janitor)
library("gridExtra")



survey <- read_excel("Questionaire.xlsx")

#------------------- Exploring Data -------------------------

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
similar_training <- survey$`Have you attended any training of this sort before?`== "Yes"
survey[similar_training,]



# ------Filtering respondents who have not had similar training
no_similar_training <- survey$`Have you attended any training of this sort before?`=="No"
survey[no_similar_training,]




# -------------Filtering respondents-----------

#attended a similar training
attended_similar <- (subset(survey, survey$`Have you attended any training of this sort before?`== "Yes"))
similar_training <- ggplot(attended_similar, aes(College, `The training session was useful to my work`, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5) +
  labs(title ="The training session was useful to my work",
       subtitle="Respondents who have attended a similar training") +
  ylab(" ")


#Never attended a similar training
not_attended_similar <- (subset(survey, survey$`Have you attended any training of this sort before?`== "No"))
no_similar_training <- ggplot(not_attended_similar, aes(College, `The training session was useful to my work`, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.8) +
  labs(subtitle="Respondents who have not attended a similar training",
       caption = "Source: August 2022 Refresher Training Survey, University of Cape Coast") +
  ylab("")

#------------
#subplot of attendance of similar training and no similar training
grid.arrange(similar_training, no_similar_training, nrow = 2, ncol=1)
#--------------------------------------------------------------------

similar_training_impact <- ggplot(attended_similar, aes(College, `The training will impact positively on my work `, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5) +
  labs(title="The training will impact positively on my work",
       subtitle="Respondents attended a similar training and believes this training will impact positively on their work") +
  ylab("")

no_similar_training_impact <- ggplot(not_attended_similar, aes(College, `The training will impact positively on my work `, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.9) +
  labs(subtitle="Respondents have not attended a similar training and believes this training will impact positively on their work",
       caption = "Source: August 2022 Refresher Training Survey, University of Cape Coast") +
  ylab("")

#-------
#subplot of attendance of similar training and no similar training Vs Impact
grid.arrange(similar_training_impact, no_similar_training_impact, nrow = 2, ncol=1)
# -----------------

similar_and_regular_training <- ggplot(attended_similar, aes(College,`I need such training regularly `, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5) +
  labs(title="I need such training regularly",
       subtitle="Attended Similar training: Needs such training regularly") +
  ylab("")

no_similar_and_regular_training <- ggplot(not_attended_similar, aes(College,`I need such training regularly `, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.8) +
  labs(subtitle="Has not attendened similar training: Needs such training regularly",
       caption = "Source: August 2022 Refresher Training Survey, University of Cape Coast") +
  ylab("")
#-------------
grid.arrange(similar_and_regular_training, no_similar_and_regular_training, nrow = 2, ncol=1)
#-----------------


# acquired basic skills for UCCOSIS Usage

acquired_basic_knowledge1 <- ggplot(attended_similar, aes(College,`I have acquired the basic knowledge necessary to use the UCCOSIS  `, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.4) +
  labs(title="I have acquired the basic knowledge necessary to use the UCCOSIS",
       subtitle="Perpective: Attended Similar training") +
  ylab("")

acquired_basic_knowledge2 <- ggplot(not_attended_similar, aes(College,`I have acquired the basic knowledge necessary to use the UCCOSIS  `, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.8) +
  labs(subtitle="Perpective: Has not attended Similar training",
       caption = "Source: August 2022 Refresher Training Survey, University of Cape Coast")+
  ylab("")

#-------------
grid.arrange(acquired_basic_knowledge1, acquired_basic_knowledge2, nrow = 2, ncol=1)
# ---------


# Needs a user manual for UCCOSIS

needs_user_manual_1 <- ggplot(attended_similar, aes(College,`I need a user manual for the UCCOSIS `, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.4) +
  labs(title="I need a user manual for the UCCOSIS",
       subtitle="Perpective: Attended Similar training")+
  ylab("")

needs_User_manual_2 <- ggplot(not_attended_similar, aes(College,`I need a user manual for the UCCOSIS `, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.8) +
  labs(subtitle="Perpective: Has not attended Similar training",
       caption = "Source: August 2022 Refresher Training Survey, University of Cape Coast")+
  ylab("")

grid.arrange(needs_user_manual_1, needs_User_manual_2, nrow = 2, ncol=1)

# ------- Insight 1: This insight compares Respondents who have had attended 
# similar training and how relevant the Refresher training was to their work" -----------

ggplot(survey, aes(Designation, `The training session was useful to my work`,  fill=`Have you attended any training of this sort before?`)) +
  geom_bar(stat="identity", position = "dodge", width = 0.9) +
  labs(title="Similar workshop experience and Relevance of this training to respondent's work",
       subtitle="Slide Note: This insight compares Respondents who have had attended similar trainings and how relevant the Refresher training was to their work",
       caption = "Source: August 2022 Refresher Training Survey, University of Cape Coast")+
  xlab("")
# -------------------------------------------------------------------------------------------

# Insight 2: --- Impact of UCCOSIS on work quality ---
g <- ggplot(survey, aes(`Using the UCCOSIS improves the quality of my task/work`))
g + geom_bar(aes(fill=College), width = 0.5) + 
  theme(axis.text.x = element_text(angle=0, vjust=0.1)) +
  labs(title="Impact of UCCOSIS on work quality ", 
      subtitle="Slide Note: Respondents' ratings on 'Using the UCCOSIS improves the quality of my task/work'", 
       caption="Source: August 2022 Refresher Training Survey, University of Cape Coast")+
  ylab("") + xlab("")
# -------------------------------------------------------------------------------------------------


# Insight 3: ---- This training should be regular because it impacts my work
Need_such_training_regularly <- survey$`I need such training regularly `
Training_positively_impacts_my_work <- survey$`The training will impact positively on my work `
ggplot(survey, aes(College, Need_such_training_regularly,  fill=Training_positively_impacts_my_work)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5) +
  labs(title="The need for regular refresher training due to impact on work",
       subtitle="Slide Note: The need to regualarly hold Refresher trainings due to the impact on respondents' work",
       caption = "Source: August 2022 Refresher Training Survey, University of Cape Coast")


# ---------------------------------------------

# Insight 4: Respondents who acquired basic knowledge and their view on obtaining a manual for UCCOSIS
Need_for_Manual <- survey$`I need a user manual for the UCCOSIS `
ggplot(survey, aes(College, survey$`I have acquired the basic knowledge necessary to use the UCCOSIS  `,  fill=Need_for_Manual)) +
  geom_bar(stat="identity", position = "dodge", width = 0.8) +
  labs(title="The Need for a user manual for usage of UCCOSIS software ",
       subtitle="Respondents acquired basic knowledge on the usage of UCCOSIS, However, obtaining a manual for the software is vital",
       caption = "Source: August 2022 Refresher Training Survey, University of Cape Coast") +
  ylab("Acquired Basic Knowledge of UCCOSIS") + xlab("College")



# Insight 5--- Venue suitability Vs Similar event attended
Venue_Suitability <- survey$`Venue Suitability`
insight_5 <- ggplot(survey, aes(College, Venue_Suitability, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.9) +
  labs(title="Venue Suitablity for the Refresher Training", 
       subtitle="Slide Note: Respondents' response on how suitable the venue was for the training", 
       caption="Source: Survey Response From Refresher Training,University of Cape Coast; August 2022")
View(survey)

# Insight 6 ----- How respondents rated each presentation made during the training----
present_1 <- ggplot(survey, aes(College,`How would you rate the presentation 1?`, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.8) +
  labs(title="How did respondents find the Presentations made throughout the training sessions", 
       subtitle="Slide Note: Respondents' judgement on the quality of presentations made for the training")+
  ylab("Presentation 1")+ xlab("College") 
# --
present_2 <- ggplot(survey, aes(College,`How would you rate the presentation 2?`, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.8) +
  labs(caption="Source: August 2022 Refresher Training Survey, University of Cape Coast")+
  ylab("Presentation 2")+ xlab("College") 
# ---
present_3 <- ggplot(survey, aes(College,`How would you rate the presentation 3?`, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.8) +
  labs(caption="Source: August 2022 Refresher Training Survey, University of Cape Coast")+
  ylab("Presentation 3")+ xlab("College") 
#---
present_4 <- ggplot(survey, aes(College,`How would you rate the presentation 4?`, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.8) +
  labs(caption="Source: August 2022 Refresher Training Survey, University of Cape Coast")+
  ylab("Presentation 4")+ xlab("College") 

#working on subplots of two plots per view 

grid.arrange(present_1, present_2, nrow = 2, ncol=1)  
grid.arrange(present_3, present_4, nrow= 2, ncol=1)



#------------------------------------------------- Creating a filter


impact_work_skills1 <- (subset(survey, survey$`I have acquired the basic knowledge necessary to use the UCCOSIS  `=="Strongly Agree"))
impact_work_skills2 <- (subset(survey, survey$`I have acquired the basic knowledge necessary to use the UCCOSIS  `=="Agree"))
impact_work_skills3 <- (subset(survey, survey$`I have acquired the basic knowledge necessary to use the UCCOSIS  `=="NEUTRAL (Neither Agree nor Disagree)"))

chart1 <- ggplot(impact_work_skills1, aes(College,`I can make data entry using the UCCOSIS software `, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.8) + scale_fill_hue(l=50) +
  labs(title = "I have acquired the basic knowledge necessary to use the UCCOSIS & I can make data entry using the UCCOSIS software",
    subtitle="Perpective: Strongly Agree")+
  ylab(" ")

chart2 <- ggplot(impact_work_skills2, aes(College,`I can make data entry using the UCCOSIS software `, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.8) + scale_fill_hue(l=50) +
  labs(subtitle="Perpective: Agree ")+
  ylab("")


chart3 <- ggplot(impact_work_skills3, aes(College,`I can make data entry using the UCCOSIS software `, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.6) + scale_fill_hue(l=50) +
  labs(subtitle="Perpective: NEUTRAL (Neither Agree nor Disagree)")+
  ylab("")

#-------------
grid.arrange(chart1, chart2, chart3, nrow = 3, ncol=1)
#---------------------------------
chart4 <- ggplot(impact_work_skills1, aes(College,`I can make data analysis using the UCCOSIS software `, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.8) + scale_fill_hue(l=50) +
  labs(title = "I have acquired the basic knowledge necessary to use the UCCOSIS & I can make data analysis using the UCCOSIS software",
       subtitle="Perpective: Strongly Agree")+
  ylab("")


chart5 <- ggplot(impact_work_skills2, aes(College,`I can make data analysis using the UCCOSIS software `, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.8) + scale_fill_hue(l=50) +
  labs(subtitle="Perpective: Agree")+
  ylab("")



chart6 <- ggplot(impact_work_skills3, aes(College,`I can make data analysis using the UCCOSIS software `, fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.6) + scale_fill_hue(l=50) +
  labs(subtitle="Perpective: NEUTRAL (Neither Agree nor Disagree)")+
  ylab("")

#-----------
grid.arrange(chart4, chart5, chart6, nrow = 3, ncol=1)
# _____________________-


tiff("test.tiff", units = "in", width = 15, height = 5, res = 10)

ggplot(survey, aes(College, `The internet network does not support effective use of UCCOSIS`,  fill=Designation)) +
  geom_bar(stat="identity", position = "dodge", width = 0.8) + scale_fill_hue(l=50) +
  labs(title="The internet network does not support effective use of UCCOSIS",
       caption = "Source: August 2022 Refresher Training Survey, University of Cape Coast") +
  ylab(" ") 

dev.off()

