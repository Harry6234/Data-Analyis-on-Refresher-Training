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

# ------Filtering respondents who have not had similar training
no_similar_training <- survey$`Have you attended any training of this sort before?`=="No"
survey[no_similar_training,]


#Cross tab analysis with percentages.
tabyl(survey, College, Designation) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1)


# Filtering respondents
table(survey$`Have you attended any training of this sort before?`)

#attended a similar training
attended_similar <- (subset(survey, survey$`Have you attended any training of this sort before?`== "Yes"))

#Never attended a similar training
not_attended_similar <- (subset(survey, survey$`Have you attended any training of this sort before?`== "No"))

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



# Venue suitability Vs Similar event attended
Attended_similar_training <- survey$`Have you attended any training of this sort before?`
Venue_Suitability <- survey$`Venue Suitability`
c <- ggplot(survey, aes(Venue_Suitability))
c + geom_bar(aes(fill=Attended_similar_training, fill=College), width = 0.5)+
    theme(axis.text.x = element_text(angle=0, vjust=0.5)) +
  labs(title="comparisons", 
       subtitle="Note: NA implies No Answer", 
       caption="Source: Survey Response From Refresher Training,University of Cape Coast; August 2022")
View(survey)
