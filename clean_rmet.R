library(tidyverse); library(yaml); library(googlesheets4)
creds <- yaml::read_yaml("creds.yaml")
googlesheets4::gs4_auth(email = creds$email)

rmet_responses <- read_sheet(ss = creds$rmet$url, sheet = creds$rmet$responses_sheet) 
rmet_answers <- read_sheet(ss = creds$rmet$url, sheet = creds$rmet$answers) 

rmet_answers <- rmet_answers %>% 
  mutate(question_number = 1:n())

rmet_responses_long <- rmet_responses %>% 
  gather(Question, Answer, -1:-3) %>% 
  mutate(question_number = as.numeric(str_extract(Question, "[0-9]{1,2}")) - 3) %>% 
  left_join(rmet_answers, by = "question_number") %>% 
  group_by(`What is your name? (First then last)`) %>% 
  filter(Timestamp == max(Timestamp)) %>% 
  summarize(`Student ID` = first(`What is your student ID? (Use the email with which we have corresponded with you.)`),
            score = sum(Answer == `Correct Answer`)) %>% 
  ungroup %>% 
  mutate(Percentile = ntile(score, 100), 
         `Student ID` = tolower(`Student ID`))


save(rmet_responses_long, file = paste0(Sys.Date(), "-RMET-results.RData"))
