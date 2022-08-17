#### Load the Data ####
library(tidyverse)
survey_data <- readxl::read_excel("Random Survey Data.xlsx")
survey_cols <- c(paste0("design_q", rep(1:5)), paste0("quality_q", rep(1:5)), paste0("pricing_q", rep(1:5)), "id")

# Change column names
colnames(survey_data) <- survey_cols                                                  

#### Recode the Likert responses ####
likert_recode <- function(x) {
  as.numeric(case_when(
    x == "Strongly Disagree" ~ 1,
    x == "Disagree" ~ 2,
    x == "Neutral" ~ 3,
    x == "Agree" ~ 4,
    x == "Strongly Agree" ~ 5,
  ))
}

# Negative Recode 
likert_recode_negative <- function(x) {
  as.numeric(case_when(
    x == "Strongly Disagree" ~ 5,
    x == "Disagree" ~ 4,
    x == "Neutral" ~ 3,
    x == "Agree" ~ 2,
    x == "Strongly Agree" ~ 1,
  ))
}


# Apply the recoding
survey_positive <- survey_data %>%
  select(-id, -design_q3, -quality_q3, -quality_q4, -pricing_q5) %>%
  mutate_all(likert_recode)

survey_negative <- survey_data %>%
  select(design_q3, quality_q3, quality_q4, pricing_q5) %>%
  mutate_all(likert_recode_negative)

#Recombine data
survey_recoded <- cbind(survey_positive, survey_negative) 

#### Calculate Totals ####
# Overall Total
survey_recoded$total <- rowSums(survey_recoded) #Create column of total score per person

survey_recoded <- cbind(id = survey_data$id, survey_recoded) #Recombine data with Respondent IDs

# Total by category
survey_recoded <- survey_recoded %>%
  rowwise() %>%
  mutate(design_total = sum(design_q1, design_q2, design_q3, design_q4, design_q5),
         quality_total = sum(quality_q1, quality_q2, quality_q3, quality_q4, quality_q5),
         pricing_total = sum(pricing_q1, pricing_q2, pricing_q3, pricing_q4, pricing_q5)) %>%
  ungroup()


#### Reshape the data #### 
survey_long <- survey_recoded %>%
  pivot_longer(data = .,
               cols = 2:16,
               names_to = "question",
               values_to = "response") %>%
  select(-total, -design_total, -quality_total, -pricing_total) %>%
  mutate(category = str_match(question, "^[^_]+(?=_)"),
          question_no = str_match(question, "[0-9]$")
  )

#### Aggregate the data ####
survey_long %>%
  group_by(category) %>%
  summarise(avg_response = mean(response), sd_response = sd(response))

#### Visualize the data ####
# Histogram of Total Score
ggplot(survey_recoded, aes(x=total)) +
  geom_bar() 


# Histogram By Category
ggplot(survey_long, aes(x=response)) +
  geom_bar() +
  facet_wrap(~category)

# Histogram By Question
ggplot(survey_long, aes(x=response)) +
  geom_bar() +
  facet_wrap(~question, nrow = 3)

# Average Response per Question
survey_long %>%
  group_by(question) %>%
  ggplot(., aes(x = question_no, y = response/1000)) +
    geom_col() +
    facet_wrap(~category) + 
    ylim(0,5) 
