'''{r, installing packages}'''

install.packages("tidyverse")
install.packages("psych")
install.packages("readx1")
install.packages("gt")
install.packages("janitor")
install.packages("gtsummary")

library(tidyverse)
library(psych)
library(gt)
library(janitor)
library(dplyr)
library(gtsummary)
library(quarto)

'''{cleaning and mutating attachment and anxiety data}'''

attachment_anxiety <- Attachment_Anxiety_Data |>
  rename(
    gender = Gender,
    age_range = `Age group`,
    relationship_status = Relationship,
    ethnicity = Ethnicity
  ) |>
  mutate(
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    age_range = case_when(
      age_range == 1 ~ "18-24",
      age_range == 2 ~ "25-34",
      age_range == 3 ~ "35-44",
      age_range == 4 ~ "45-54",
      age_range == 5 ~ "55-64",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    across(SA_1:SEst_10, as.integer)
  )

'''{creating an answer key and scoring items}'''

SA_answer_key <- list(comp = c("SA_1","SA_2","SA_3","SA_4","-SA_5", "SA_6","SA_7","SA_8","-SA_9","SA_10","-SA_11","SA_12","SA_13","SA_14","SA_15","SA_16","SA_17","SA_18","SA_19","SA_20"))
AA_answer_key <- list(comp = c("AA_1","AA_2","AA_3","AA_4","AA_5","AA_6","AA_7","AA_8","AA_9"))
SEst_answer_key <- list(comp = c("SEst_1","-SEst_2","SEst_3","SEst_4","-SEst_5","-SEst_6","SEst_7","-SEst_8","-SEst_9","SEst_10"))

SA_scores <- scoreItems(SA_answer_key, attachment_anxiety, totals = F, min = 0, max = 4)
AA_scores <- scoreItems(AA_answer_key, attachment_anxiety, totals = F, min = 1, max = 7)
SEst_scores <- scoreItems(SEst_answer_key, attachment_anxiety, totals = F, min = 1, max = 4)

SA_scores$alpha
AA_scores$alpha
SEst_scores$alpha

'''{making scores into data frames and combining all scores}'''

SA_scores.df <- as.data.frame(SA_scores$scores)
AA_scores.df <- as.data.frame(AA_scores$scores)
SEst_scores.df <- as.data.frame(SEst_scores$scores)

SA_AA_SEst_scores.df <- cbind(SA_scores.df, AA_scores.df, SEst_scores.df)

attachment_anxiety_outcomes <- cbind(
  gender = attachment_anxiety$gender,
  age_range = attachment_anxiety$age_range,
  SA_AA_SEst_scores.df
)

names(attachment_anxiety_outcomes)[3:5] <- c(
  "social_interaction_anxiety",
  "experiences_in_close_relationships_anxiety",
  "self_esteem"
)

'''{final tables}'''

demographic.table <- attachment_anxiety_outcomes |>
  select(gender, age_range) |>
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  )

demographic.table |>
  modify_caption("Table 1. Demographic Characteristics") |>
  as_gt()

summary <- describe(
  attachment_anxiety_outcomes[, c(
    "social_interaction_anxiety",
    "experiences_in_close_relationships_anxiety",
    "self_esteem"
  )]
)

N <- nrow(attachment_anxiety_outcomes)

summary.table <- summary |>
  mutate(
    Scale = c("Social Interaction Anxiety",
              "Experiences in Close Relationships Anxiety",
              "Self-Esteem"
    )
  ) |>
  relocate(.before = 1)

alphas <- round(c(
  SA_scores$alpha,
  AA_scores$alpha,
  SEst_scores$alpha
), 2)

summary.score.table <- summary.table |>
  select(Scale, mean, median, sd, range)
  
summary.scores <- cbind(summary.score.table, alphas)

summary.scores |>
  gt() |>
  tab_header(
    title = "Table 2. Participant Attachment, Anxiety, & Self-Esteem Outcomes"
    ) 

'''{r, filtering dataset}'''

filtered.summary <- attachment_anxiety_outcomes |>
  filter(gender == "Female")

filtered.summary.table <- describe(
  filtered.summary[, c(
    "social_interaction_anxiety",
    "experiences_in_close_relationships_anxiety",
    "self_esteem"
  )]
)

N <- nrow(filtered.summary.table)

filtered.summary.table <- filtered.summary.table |>
  mutate(
    Scale = c("Social Interaction Anxiety",
              "Experiences in Close Relationships Anxiety",
              "Self-Esteem"
    )
  )

filtered.summary.score.table <- filtered.summary.table |>
  select(Scale, mean, median, sd, range)

filtered.summary.scores |>
  gt() |>
  tab_header(
    title = "Table 2. Participant Attachment, Anxiety, & Self-Esteem Outcomes"
  ) 

###Introduction###
###Methods###
###Participants###
###Measures###
###Data Analysis###
###Results###

