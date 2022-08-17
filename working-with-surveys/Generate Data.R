library(purrr)
library(writexl)


set.seed(807)
responses <- c(
  rbeta(1000, 0.3,1),
  rbeta(1000, 3,1),
  rbeta(1000, 2.2,3),
  rbeta(1000, 18,20),
  rbeta(1000, 5,9),
  rbeta(1000, 5,2),
  rbeta(1000, 8,15),
  rbeta(1000, 3,2),
  rbeta(1000, 14,30),
  rbeta(1000, 20,30),
  rbeta(1000, 0.5,1),
  rbeta(1000, 7,4),
  rbeta(1000, 3,9),
  rbeta(1000, 7.7,10),
  rbeta(1000, 8,17)
  )


survey_data <- data.frame(split(responses, ceiling(seq_along(responses)/1000)))
likert_code <- function(x){
  dplyr::case_when(x <= 0.2 ~ "Strongly Disagree",
            x <= 0.4 ~ "Disagree",
            x <= 0.6 ~ "Neutral",
            x <= 0.8 ~ "Agree",
            TRUE ~ "Strongly Agree")
}

survey_data <- survey_data %>%
    dplyr::mutate_all(likert_code)

survey_data$id <- paste0("Respondent #", 1:1000)

write_xlsx(survey_data, "Random Survey Data.xlsx")
