library(readxl); library(tidyverse); library(glmnet)

load("2020-09-17-ground_truth_estimates.RData")

sheet_names <- readxl::excel_sheets(path = "Peer Review Responses.xlsx")

question_trait <- tribble(~Trait, ~`Question text`,
                          "Intelligence", "Q1. How intelligent does this person seem? [Scaled Feedback]",
                          "Perseverance", "Q1. Does this person seem very persevering? [Scaled Feedback]",
                          "Integrity", "Q1. Would you say that this person has a strong moral compass? [Scaled Feedback]",
                          "Empathy", "Q1. How empathetic does this person seem from their response? [Scaled Feedback]",
                          "Spike", "Q1. How passionate does this person seem about the problem they describe? [Scaled Feedback]",
                          "Boost", "Q1. How underprivileged do you think this person is? [Scaled Feedback]")

results_list <- lapply(sheet_names, function(x) {
  tmp <- read_excel("Peer Review Responses.xlsx", sheet = x)
  if(nrow(tmp)>0) {
    tmp %>% 
      gather(Question, Answer, -Timestamp:-`What is your name?`) %>% 
      mutate(UID = str_extract(Question,"UID: [0-9]{4}"),
             `Question text` = str_extract(Question, "Q[0-9]{1,2}.*"),
             Form = x)
  }
})



peer_review_raw <- bind_rows(results_list) %>% 
  group_by(`What is your name?`, Form) %>%
  mutate(video_number = paste("Video", c(0, cumsum(UID[-1] != lag(UID, 1)[-1])))) %>% 
  ungroup %>% 
  left_join(question_trait, by = "Question text") %>% 
  group_by(Form) %>% 
  mutate(Trait = first(Trait[!is.na(Trait)])) %>% 
  ungroup %>%
  mutate(`What is your email?` = tolower(`What is your email?`),
         `What is your name?` = case_when(`What is your name?`=="Anja Ne," ~ "Anja Nel", 
                                          `What is your name?`=="angela zhong" ~ "Angela Zhong",
                                          `What is your name?`=="Angela Zho" ~ "Angela Zhong",
                                          `What is your name?`=="Joaquim de Moura Camilo Gomes da Cruz" ~ "Joaquim de Moura",
                                          `What is your name?`=="Zoee" ~ "Zoee Robinson",
                                          `What is your name?`=="Aravind challa" ~ "Aravind Challa",
                                          TRUE ~ `What is your name?`))

videos_in_forms <- peer_review_raw %>% 
  filter(!is.na(UID)) %>%
  group_by(Form, video_number) %>% 
  summarise(UID = first(UID)) %>% 
  mutate(UID = str_extract(UID, "(?<=UID: ).*"))
  

peer_review_likert <- peer_review_raw %>% 
  filter(grepl("Scaled Feedback", `Question text`)) %>% 
  mutate(UID2 = str_extract(UID, "(?<=UID: ).*"),
         Question_number = str_extract(Question, "Q[0-9]{1}"),
         numeric_response = parse_number(Answer)) %>% 
  group_by(`What is your name?`, `Question text`) %>% 
  mutate(scaled_numeric = numeric_response - mean(numeric_response, na.rm = T))


peer_review_likert %>% group_by(UID2, Trait) %>% tally() %>% nrow()
peer_review_likert %>% group_by(UID2) %>% tally() %>% nrow()
peer_review_likert %>% group_by(`What is your name?`) %>% tally() %>% nrow()

peer_review_likert %>% 
  left_join(tracker_data_raw %>% select(`Student ID`, `User ID`), by = c("UID2" = "User ID")) %>% 
  left_join(ground_truth_and_interview %>% select(group, `Student ID`, Trait, Score, Interview_score), by = c("Student ID", "Trait")) %>% 
  group_by(UID2,Trait) %>%
  mutate(range = max(scaled_numeric) - min(scaled_numeric)) %>%
  filter(between(scaled_numeric, min(scaled_numeric), max(scaled_numeric))) %>%
  mutate(mean_score = mean(numeric_response)) %>%
  ungroup %>%
  group_by(Trait, UID2, group) %>% 
  summarise(mean_score = mean(scaled_numeric, na.rm =T),
            Score = mean(Score, na.rm = T),
            Interview_score = mean(Interview_score, na.rm =T)) %>%
  group_by(group) %>%
  mutate(mean_score = scale(mean_score),
         Score = scale(Score),
         Interview_score = scale(Interview_score)) %>%
  ggplot(aes(x = mean_score, y = Score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Trait) +
  ggthemes::theme_hc() +
  labs(x = "Naive peer score", y = "Ground truth score", 
       title = "First cut peer scores are not exciting")


predict_score <- function(x) {
  X <- model.matrix( ~ Q1 + Q2 + Q3 + Q4 + missing_q1 + missing_q2 + missing_q3 + missing_q4 + Age + I(Age^2)
                     , data = x)
  y <- x$Score
  cv_fit <- cv.glmnet(X, y, nfolds = nrow(x), alpha = 0.0)
  
  print(x$Trait[1])
  coefs <- coef(cv_fit, s = "lambda.min")
  
  print(coefs)
  x$prediction <- predict(cv_fit, newx =X, s = "lambda.min")
  x
}

peer_review_likert %>% 
  left_join(tracker_data_raw %>% select(`Student ID`, `User ID`), by = c("UID2" = "User ID")) %>% 
  left_join(ground_truth_and_interview, by = c("Student ID", "Trait")) %>% 
  select(demographics_id, ESL, Age, Gender, School_type, Family_income_quintile, Paid_subscriptions, group, UID2, Question_number, Trait, scaled_numeric, Score, Interview_score, `What is your email?`) %>%
  group_by(UID2, Question_number, Trait, group) %>%
  summarise(scaled_numeric = mean(scaled_numeric), 
            Interview_score = mean(Interview_score),
            Score = mean(Score),
            ESL = first(ESL),
            Age = first(Age),
            Gender = first(Gender), 
            School_type = first(School_type),
            Family_income_quintile = first(Family_income_quintile),
            Paid_subscriptions = first(Paid_subscriptions)) %>%
  ungroup %>%
  #select(-`Question text`) %>% 
  filter(complete.cases(.))%>%# & !duplicated(paste(`What is your name?`, group, UID2, Trait, Question_number))) %>% 
  spread(Question_number, scaled_numeric) %>%
  mutate(missing_q1 = is.na(Q1),
         missing_q2 = is.na(Q2),
         missing_q3 = is.na(Q3),
         missing_q4 = is.na(Q4),
         Q4 = ifelse(is.na(Q4), -5, Q4),
         Q3 = ifelse(is.na(Q3), -5, Q3),
         Q2 = ifelse(is.na(Q2), -5, Q2),
         Q1 = ifelse(is.na(Q1), -5, Q1)) %>% 
  group_by(Trait) %>% 
  do(predict_score(.)) %>%
  # group_by(Trait, UID2) %>%
  # summarise(Score = mean(Score),
  #           prediction = mean(prediction)) %>%
  ggplot(aes(x = prediction, y = Score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  #geom_abline(aes(intercept = 0, slope = 1)) +
  facet_wrap(~Trait)+
  ggthemes::theme_hc() +
  labs(x = "Predicted ground truth scores with peer review",
       y = "Ground truth scores", 
       title = "Boost and spike seem possible to learn from peer review",
       subtitle = "Optimal machine-learned peer questions")


peer_review_likert %>% 
  left_join(tracker_data_raw %>% select(`Student ID`, `User ID`), by = c("UID2" = "User ID")) %>% 
  left_join(ground_truth_and_interview, by = c("Student ID", "Trait")) %>% 
  select(demographics_id, ESL, Age, Gender, School_type, Family_income_quintile, Paid_subscriptions, group, UID2, Question_number, Trait, scaled_numeric, Score, Interview_score, `What is your email?`) %>%
  filter(Trait %in% c("Boost", "Spike")) %>%
  group_by(UID2, Question_number, Trait, group) %>%
  summarise(scaled_numeric = mean(scaled_numeric), 
            Interview_score = mean(Interview_score),
            Score = mean(Score),
            ESL = first(ESL),
            Age = first(Age),
            Gender = first(Gender), 
            School_type = first(School_type),
            Family_income_quintile = first(Family_income_quintile),
            Paid_subscriptions = first(Paid_subscriptions)) %>%
  ungroup %>%
  #select(-`Question text`) %>% 
  filter(complete.cases(.))%>%# & !duplicated(paste(`What is your name?`, group, UID2, Trait, Question_number))) %>% 
  filter(case_when(Trait=="Spike" & Question_number %in% c("Q1","Q2") ~ TRUE, 
                   Trait == "Boost" & Question_number %in% c("Q2","Q4") ~ TRUE, 
                   Trait == "Intelligence" & Question_number %in% c("Q2") ~ TRUE, 
                   TRUE ~ FALSE)) %>% 
  # spread(Question_number, scaled_numeric) %>%
  # mutate(missing_q1 = is.na(Q1),
  #        missing_q2 = is.na(Q2),
  #        missing_q3 = is.na(Q3),
  #        missing_q4 = is.na(Q4),
  #        Q4 = ifelse(is.na(Q4), -5, Q4),
  #        Q3 = ifelse(is.na(Q3), -5, Q3),
  #        Q2 = ifelse(is.na(Q2), -5, Q2),
  #        Q1 = ifelse(is.na(Q1), -5, Q1)) %>% 
  group_by(Trait, UID2, group) %>%
  summarise(Score = mean(Score),
            prediction = mean(scaled_numeric[between(scaled_numeric, min(scaled_numeric), max(scaled_numeric))]))%>%
  ggplot(aes(x = prediction, y = Score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  #geom_abline(aes(intercept = 0, slope = 1)) +
  facet_wrap(~Trait)+
  ggthemes::theme_hc() +
  labs(x = "Simple peer review scores",
       y = "Ground truth scores", 
       title = "Boost and spike seem possible to learn from peer review",
       subtitle = "Without controlling for demographics")



# Can we measure everything with Spike? -----------------------------------

tmp <- peer_review_likert %>% 
  left_join(tracker_data_raw %>% select(`Student ID`, `User ID`), by = c("UID2" = "User ID")) %>% 
  left_join(ground_truth_and_interview, by = c("Student ID", "Trait")) %>% 
  select(demographics_id, ESL, Age, Gender, School_type, Family_income_quintile, Paid_subscriptions, group, UID2, Question_number, Trait, scaled_numeric, Score, Interview_score, `What is your email?`) %>%
  group_by(UID2, Question_number, Trait, group) %>%
  summarise(scaled_numeric = mean(scaled_numeric), 
            Interview_score = mean(Interview_score),
            Score = mean(Score),
            ESL = first(ESL),
            Age = first(Age),
            Gender = first(Gender), 
            School_type = first(School_type),
            Family_income_quintile = first(Family_income_quintile),
            Paid_subscriptions = first(Paid_subscriptions)) %>%
  ungroup %>%
  #select(-`Question text`) %>% 
  filter(complete.cases(.))%>%# & !duplicated(paste(`What is your name?`, group, UID2, Trait, Question_number))) %>% 
  mutate(relevant = case_when(Trait=="Spike" & Question_number %in% c("Q1","Q2") ~ TRUE, 
                   Trait == "Boost" & Question_number %in% c("Q2","Q4") ~ TRUE, 
                   Trait == "Intelligence" & Question_number %in% c("Q2") ~ TRUE, 
                   TRUE ~ FALSE)) %>% 
  group_by(Trait, UID2, group) %>%
  summarise(Score = median(Score),
            prediction = median(scaled_numeric[relevant]))

tmp %>% 
  group_by(UID2) %>%
  mutate(tt = "Spike" %in% Trait) %>% 
  filter(tt) %>%
  mutate(Spike = prediction[Trait=="Spike"]) %>%
  ungroup %>%
  ggplot(aes(x = Spike, y = Score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  #geom_abline(aes(intercept = 0, slope = 1)) +
  facet_wrap(~Trait)+
  ggthemes::theme_hc() +
  labs(x = "Simple peer review scores",
       y = "Ground truth scores", 
       title = "Does Spike measure everything?",
       subtitle = "Peer review Spike against ground truth everything")


# Controlling errors for demographics -------------------------------------

pr_errors <- peer_review_likert %>% 
  left_join(tracker_data_raw %>% select(`Student ID`, `User ID`), by = c("UID2" = "User ID")) %>% 
  left_join(ground_truth_and_interview, by = c("Student ID", "Trait")) %>% 
  select(demographics_id, ESL, Age, Gender, School_type, Family_income_quintile, Paid_subscriptions, group, UID2, Question_number, Trait, scaled_numeric, Score, Interview_score, `What is your email?`) %>%
  filter(Trait %in% c("Boost", "Spike")) %>%
  group_by(UID2, Question_number, Trait, group) %>%
  summarise(scaled_numeric = mean(scaled_numeric), 
            Interview_score = mean(Interview_score),
            Score = mean(Score),
            ESL = first(ESL),
            Age = first(Age),
            Gender = first(Gender), 
            School_type = first(School_type),
            Family_income_quintile = first(Family_income_quintile),
            Paid_subscriptions = first(Paid_subscriptions)) %>%
  ungroup %>%
  #select(-`Question text`) %>% 
  filter(complete.cases(.))%>%# & !duplicated(paste(`What is your name?`, group, UID2, Trait, Question_number))) %>% 
  filter(case_when(Trait=="Spike" & Question_number %in% c("Q1","Q2") ~ TRUE, 
                   Trait == "Boost" & Question_number %in% c("Q2","Q4") ~ TRUE, 
                   Trait == "Intelligence" & Question_number %in% c("Q2") ~ TRUE, 
                   TRUE ~ FALSE)) %>% 
  group_by(Trait, UID2, group) %>%
  mutate(Score = mean(Score, na.rm = T),
         prediction = mean(scaled_numeric[between(scaled_numeric, min(scaled_numeric), max(scaled_numeric))], na.rm = T)) %>% 
  slice(1L)


x_fit <- lm(prediction ~ Trait + Paid_subscriptions+ Family_income_quintile + ESL + Age + I(Age^2) + Gender + School_type, data = pr_errors)

summary(x_fit)

y_fit <- lm(Score ~ Trait + Paid_subscriptions+ Family_income_quintile + ESL + Age + I(Age^2) + Gender + School_type, data = pr_errors)
summary(y_fit)

tibble(Trait =pr_errors$Trait, x = resid(x_fit), y = resid(y_fit)) %>%
  ggplot(aes(x = x, y= y)) +
  facet_wrap(~Trait) + 
  geom_point() +
  geom_smooth(method = "lm")
# Next --------------------------------------------------------------------




# Correlation between traits ----------------------------------------------

ground_truth_and_interview %>%
  select(Score, Trait, `User ID`, group) %>%
  filter(!duplicated(paste0(`User ID`, Trait))) %>%
  spread(Trait, Score) %>%
  select(-`User ID`) %>%
  ggplot(aes(x = Boost, y = Spike)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Ground truth boost vs spike") +
  ggthemes::theme_hc()

ground_truth_and_interview %>%
  select(Score, Trait, `User ID`, group) %>%
  filter(!duplicated(paste0(`User ID`, Trait))) %>%
  group_by(`User ID`) %>%
  mutate(Boost = Score[Trait=="Boost"]) %>%
  filter(Trait != "Boost") %>%
  select(-`User ID`) %>%
  ggplot(aes(x = Boost, y = Score)) +
  geom_point() +
  facet_wrap(~Trait) +
  geom_smooth(method = "lm") +
  labs(title = "Boost vs others, ground truth") +
  ggthemes::theme_hc()

ground_truth_and_interview %>%
  select(Interview_score, Trait, `User ID`, group) %>%
  filter(!duplicated(paste0(`User ID`, Trait))) %>%
  spread(Trait, Interview_score) %>%
  select(-`User ID`) %>%
  ggplot(aes(x = Boost, y = Spike)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Interview boost vs spike") +
  ggthemes::theme_hc()

pr_errors %>% 
  select(UID2, prediction, Trait) %>% 
  spread(Trait, prediction)  %>%
  ggplot(aes(x = Boost, y = Spike)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Peer review boost vs spike") +
  ggthemes::theme_hc()


# Next --------------------------------------------------------------------


peer_review_orderd <- peer_review_raw %>% 
  group_by(`What is your email?`) %>%
  filter(Timestamp==max(Timestamp)) %>%
  ungroup %>%
  filter(is.na(UID) & !grepl("Do you have any feedback for us", Question)) %>% 
  select(Trait, `What is your name?`, `What is your email?`, Answer, Question, Form) %>%
  left_join(videos_in_forms %>% rename(UID_reviewee= UID), by = c("Form", "Answer" = "video_number")) %>% 
  group_by(Form, `What is your email?`) %>% 
  mutate(score = 6:1)

peer_review_orderd %>% 
  group_by(UID_reviewee, Trait) %>% 
  summarise(score = mean(score)) %>% 
  left_join(tracker_data_raw %>% select(`Student ID`, `User ID`), by = c("UID_reviewee" = "User ID")) %>% 
  left_join(ground_truth_3, by = c("Student ID", "Trait")) %>% 
  ggplot(aes(x = score, y = Score)) +
  geom_point() +
  facet_wrap(~Trait)

peer_review_orderd %>% 
  group_by(UID_reviewee, Trait) %>% 
  summarise(score = mean(score, na.rm = T)) %>% 
  ungroup %>%
  spread(Trait, score) %>% 
  select(-UID_reviewee) %>%
  plot()

peer_review_email_addresses <- peer_review_likert %>% 
  group_by(Email = `What is your email?`, peer_revewer_name = `What is your name?`) %>% 
  tally() %>% 
  ungroup

demogs_lookup <- demographics_2 %>% 
  mutate(demographics_id = 1:n()) %>%
  select(Name, Student.ID, Alternate_email, demographics_id) %>% 
  gather(Key, Email, -Name, -demographics_id)

rmet_responses_long_2 <- rmet_responses_long %>% 
  left_join(demogs_lookup %>% select(Email, demographics_id), by = c("Student ID" = "Email"))

peer_review_demogs_lookup <- peer_review_email_addresses %>% 
  left_join(demogs_lookup, by = "Email")

candidate_demogs_lookup <- tracker_data_raw %>% 
  filter(!is.na(`Student ID`)) %>%
  select(`User ID`, `Student ID`) %>% 
  left_join(demogs_lookup, by = c("Student ID" = "Email")) %>% 
  rename(reviewee_demographics_id = demographics_id) %>% 
  select(`User ID`, reviewee_demographics_id)


peer_review_model <- peer_review_likert %>% 
  filter(!is.na(numeric_response)) %>% 
  ungroup %>% 
  mutate(item_index = as.numeric(factor(`Question text`)),
         reviewer_index = as.numeric(factor(tolower(`What is your name?`))),
         reviewee_index = as.numeric(factor(UID2)),
         concept_index = as.numeric(factor(Trait)))




peer_review_model %>% 
  group_by(`What is your name?`) %>% 
  summarise(N = n(),
            unique_responses = length(unique(numeric_response))) %>% 
  arrange(unique_responses)


peer_review_model_2 <- peer_review_model %>% 
  left_join(tracker_data_raw %>% select(Name, `User ID`), by  = c("UID2" = "User ID")) %>%
  left_join(ground_truth_3 %>% select(group, Name, Trait, Interview_score, Score), by = c("Name", "Trait"))


peer_review_with_demogs <- peer_review_model %>% 
  mutate(Email = tolower(`What is your email?`)) %>% 
  left_join(peer_review_demogs_lookup %>% select(Email, demographics_id), by = "Email") %>% 
  left_join(candidate_demogs_lookup, by = c("UID2" = "User ID")) %>% 
  left_join(demographics_2 , by = c("reviewee_demographics_id" = "demographics_id")) %>% 
  left_join(demographics_2 , by = c("demographics_id" = "demographics_id")) %>% 
  left_join(proximity_lookup, by = c("demographics_id", "reviewee_demographics_id")) %>% 
  left_join(ground_truth_3 %>% select(group, Trait, Score, Interview_score, `User ID`), by = c("UID2" = "User ID", "Trait")) %>% 
  left_join(rmet_responses_long_2 %>% select(demographics_id, score), by = c("reviewee_demographics_id" = "demographics_id"))%>% 
  left_join(rmet_responses_long_2 %>% select(demographics_id, score) %>% rename(reviewer_score = score), by = c("demographics_id" = "demographics_id")) %>%
  filter(!is.na(proximity) & !is.na(Score) & !is.na(reviewer_score) & !is.na(score)) 
  



library(lme4)

lme_fit <- lmer(Score ~ 
                  numeric_response +
                    I(score/10) +
                    I(Education_country.x == Education_country.y) +
                    Scholarship.x +
                    ESL.x + 
                    I(ESL.y==ESL.x) +
                    Guardian_one_education.x +
                    English_speaking_household.x +
                    I(English_speaking_household.x==English_speaking_household.y) +
                    Mobile_plan_type.x +
                    I(School_type.x == School_type.y) +
                    Gender.x + 
                    I(proximity*10) +
                    Family_income_quintile.x +
                    I(0.1*Age.x) +
                    I(0.1*(Age.x - Age.y)^2) +
                    I(Gender.x == Gender.y) + 
                    Trait  +
                    (1 + numeric_response| `What is your name?`) + 
                    (1 | `Question text`), data = peer_review_with_demogs)

lattice::dotplot(ranef(lme_fit, condVar = T))

lme_fit_y <- lmer(Score ~ 
                  I(score/10) +
                  I(Education_country.x == Education_country.y) +
                  Scholarship.x +
                  ESL.x + 
                  I(ESL.y==ESL.x) +
                  Guardian_one_education.x +
                  English_speaking_household.x +
                  I(English_speaking_household.x==English_speaking_household.y) +
                  Mobile_plan_type.x +
                  I(School_type.x == School_type.y) +
                  Gender.x + 
                  I(proximity*10) +
                  Family_income_quintile.x +
                  I(0.1*Age.x) +
                  I(0.1*(Age.x - Age.y)^2) +
                  I(Gender.x == Gender.y) + 
                  (1 | Trait)  + (1 | group) +
                  (1 | demographics_id) + 
                  (1 | `Question text`) + 
                  (1 | UID2:Trait), data = peer_review_with_demogs)

lme_fit_x <- lmer(numeric_response ~ 
                    I(score/10) +
                    I(Education_country.x == Education_country.y) +
                    Scholarship.x +
                    ESL.x + 
                    I(ESL.y==ESL.x) +
                    Guardian_one_education.x +
                    Education_country.x +
                    English_speaking_household.x +
                    I(English_speaking_household.x==English_speaking_household.y) +
                    Mobile_plan_type.x +
                    School_type.x +
                    I(School_type.x == School_type.y) +
                    Gender.x + 
                    I(proximity*10) +
                    Family_income_quintile.x +
                    I(0.1*Age.x) +
                    I(0.1*(Age.x - Age.y)^2) +
                    I(Gender.x == Gender.y) + (1 | group)  + 
                    (1 | demographics_id) + (1 | Trait) +
                    (1 | `Question text`) + 
                    (1 | UID2:Trait), data = peer_review_with_demogs)




y_vals <- ranef(lme_fit_y)$`UID2:Trait`
x_vals <- ranef(lme_fit_x)$`UID2:Trait`
score_data <- tibble(names = rownames(y_vals), 
                    y = y_vals[[1]],
                    x = x_vals[[1]]) %>% 
  separate(names, sep = "[:]", into = c("UID", "Trait")) %>% 
  left_join(tracker_data_raw %>% select(`User ID`, `Student ID`), by = c("UID" = "User ID")) %>% 
  left_join(demographics_2, by = c("Student ID" = "Student.ID"))


score_data %>% ggplot(aes(x = x, y = y)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  facet_wrap(~Trait)


plot(peer_review_with_demogs$numeric_response, predict(lme_fit_x))

peer_review_with_demogs$resid_lme_y <- resid(lme_fit_y)
peer_review_with_demogs$resid_lme_x <- resid(lme_fit_x)


summary(lm(resid_lme_y ~ resid_lme_x*Trait +
                    reviewer_score:resid_lme_x +
                    School_type.y:resid_lme_x +
                    proximity:resid_lme_x +
                    Family_income_quintile.y:resid_lme_x +
                    Age.y:resid_lme_x +
                    Gender.y:resid_lme_x +
                    Paid_subscriptions.y:resid_lme_x, data = peer_review_with_demogs))

peer_review_with_demogs %>% 
  ggplot(aes(x = resid_lme_x, y = resid_lme_y)) +
  geom_point() +
  facet_wrap(~Trait)


simple_peer_scores <- peer_review_with_demogs %>% 
  group_by(Trait, UID2) %>% 
  summarise(mean_resid = mean(resid_felm))


reviewee_index = peer_review_model %>% 
  group_by(reviewee_index) %>%
  summarise(UID2 = first(UID2),
            n = length(unique(reviewer_index)))

item_index <- peer_review_model %>% 
  group_by(item_index) %>%
  summarise(`Question text` = first(`Question text`))

reviewer_index <- peer_review_model %>% 
  group_by(reviewer_index) %>%
  summarise(`What is your name?` = first(`What is your name?`))

trait_index <- peer_review_model %>% 
  group_by(concept_index) %>%
  summarise(Trait = first(Trait))

peer_review_mod_compiled <- cmdstan_model("peer_review_model.stan")

peer_review_data_list <- list(N = nrow(peer_review_model),
                              R = max(reviewer_index$reviewer_index),
                              J = max(reviewee_index$reviewee_index),
                              I = max(item_index$item_index),
                              C = max(trait_index$concept_index),
                              y = peer_review_model$numeric_response,
                              reviewer = peer_review_model$reviewer_index,
                              reviewee = peer_review_model$reviewee_index,
                              item = peer_review_model$item_index,
                              concept = peer_review_model$concept_index)

peer_review_fit <- peer_review_mod_compiled$sample(data = peer_review_data_list, parallel_chains = 4, iter_warmup = 500, iter_sampling = 500)

peer_review_fit$summary("latent_scores")
# cut_points <- peer_review_fit$summary("cut_points_2")
# matrix(cut_points$mean, peer_review_data_list$R, max(peer_review_data_list$y-1))

peer_review_df <- as_draws_df(peer_review_fit$draws("latent_scores"))
peer_review_df <- peer_review_df[,1:(ncol(peer_review_df)-3)]

peer_review_scores_post <- as.data.frame(matrix(colMeans(peer_review_df), max(reviewee_index$reviewee_index), 6))
names(peer_review_scores_post) <- trait_index$Trait

ground_truth_3 <- ground_truth_and_interview2 %>% 
  left_join(tracker_data_raw %>% select(`User ID`, Name), by = "Name")

simple_peer_scores %>% 
  left_join(ground_truth_3 %>% select(group, Trait, Score, Interview_score, `User ID`), by = c("UID2" = "User ID", "Trait"))%>%
  group_by(group) %>% 
  mutate(Score = scale(Score),
         mean_resid = scale(mean_resid)) %>%
  ggplot(aes(x = mean_resid, y = Score)) +
  geom_point() +
  facet_wrap(~Trait) +
  geom_smooth(method = "lm", aes(colour = group)) +
  lims(y = c(-3, 3))

peer_review_likert %>% 
  group_by(`What is your name?`, Trait) %>%
  mutate(scaled_numeric = numeric_response - mean(numeric_response, na.rm = T)) %>%
  group_by(`Question text`) %>%
  mutate(error_scale = sd(scaled_numeric, na.rm = T)) %>%
  group_by(UID2, Trait) %>% 
  summarise(simple_score = weighted.mean(scaled_numeric, 1/error_scale)) %>%
  left_join(posterior_mean_peer_review_scores) %>%
  ggplot(aes(x = simple_score, y = `Peer review score`)) +
  geom_point() +
  facet_wrap(~Trait) +
  geom_smooth(method = "lm") +
  geom_abline(aes(intercept = 0, slope = 1))

plot(peer_review_scores_post)

posterior_mean_peer_review_scores <- peer_review_scores_post %>% 
  bind_cols(reviewee_index) %>% 
  gather(Trait, `Peer review score`, -reviewee_index:-n) %>%
  left_join(ground_truth_3, by = c("UID2" = "User ID", "Trait"))

posterior_mean_peer_review_scores_2 <- posterior_mean_peer_review_scores %>% 
  filter(!is.na(Score) & !is.na(`Peer review score`) & !is.na(Trait))

fit_lm <- lm(`Peer review score` ~ Score*Trait + 
               `In what country have you had most of your education?` +
               `Highest level of education completed by guardian 1 (optional)` +
               `Is English the language spoken within your home?` +
               `What kind of school do you attend?` +
               `Imagine that your country is divided into five income quintiles. Where would you place yourself and your family in terms of income?`, data = posterior_mean_peer_review_scores_2)

broom::tidy(fit_lm) %>%
  filter(grepl("Score", term))

posterior_mean_peer_review_scores_2$resid_2 <- resid(fit_lm)

posterior_mean_peer_review_scores_2 %>% 
  ggplot(aes(x = `What kind of school do you attend?`, y = resid_2)) +
  geom_boxplot() +
  facet_wrap(~Trait) +
  coord_flip()


posterior_complete <- posterior_mean_peer_review_scores %>%
  filter(!is.na(Name), !is.na(immigrant_family), !is.na(Gender),  
           !is.na(`If you have a mobile phone, is your account pre-paid or post-paid?`), 
           !is.na(`Is English the language spoken within your home?`),
         !is.na(`What kind of school do you attend?`), 
         !is.na(`Highest level of education completed by guardian 1 (optional)`),
         !is.na(`Highest level of education completed by guardian 2 (optional)`))


fit_y <- lm(`Score` ~ immigrant_family + 
              Gender + 
              group +
              `If you have a mobile phone, is your account pre-paid or post-paid?`+ 
              `Is English the language spoken within your home?` +
              `Imagine that your country is divided into five income quintiles. Where would you place yourself and your family in terms of income?` +
              `What kind of school do you attend?` + 
              `Highest level of education completed by guardian 1 (optional)` +
              `Highest level of education completed by guardian 2 (optional)`
            , data = posterior_complete)

fit_x <- lm(`Peer review score` ~ immigrant_family + 
                group +
                Gender + 
                `If you have a mobile phone, is your account pre-paid or post-paid?`+ 
                `Is English the language spoken within your home?` +
                `Imagine that your country is divided into five income quintiles. Where would you place yourself and your family in terms of income?` +
                `What kind of school do you attend?` + 
                `Highest level of education completed by guardian 1 (optional)` +
                `Highest level of education completed by guardian 2 (optional)`
              , data = posterior_complete)

resid_x <- resid(fit_x)
resid_y <- resid(fit_y)
tibble(resid_x, resid_y, Trait = posterior_complete$Trait) %>% 
  ggplot(aes(x = resid_x, y = resid_y)) +
  facet_wrap(~Trait) +
  geom_point()


posterior_mean_peer_review_scores %>%
  ggplot(aes(x = `Peer review score`, y = Score, colour =group)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~Trait) +
  geom_smooth(method = "lm", aes(weight = n), se = F) +
  geom_abline(aes(intercept = 0, slope = 1))




peer_review_likert %>% 
  group_by(UID2, Trait, `Question text`) %>% 
  summarise(mean_score = mean(scaled_numeric, na.rm = T)) %>%
  left_join(tracker_data_raw %>% select(Name, `User ID`), by = c("UID2" = "User ID")) %>%
  left_join(ground_truth_and_interview2, by = c("Name", "Trait")) %>%
  ggplot(aes(x = mean_score, y = Interview_score)) +
  geom_point() +
  facet_wrap(Trait ~`Question text`) +
  geom_smooth(method = "lm")



ground_truth_and_interview2 %>% 
  left_join(tracker_data_raw %>% select(Name, `User ID`), by = "Name") %>%
  left_join(peer_review_likert %>%
              group_by(UID2, Trait) %>% 
              summarise(mean_score = mean(scaled_numeric, na.rm = T)), 
            by = c("User ID" = "UID2", "Trait")) %>% 
  filter(!is.na(mean_score)) %>% 
  group_by(group) %>% 
  mutate(Score = scale(Score),
         mean_score = scale(mean_score)) %>%
  ggplot(aes(x = mean_score, y = Score)) +
  geom_point() +
  facet_wrap(~Trait) +
  geom_smooth(method = "lm")

#   
# lmer(Score ~ numeric_response +
#        reviewer_score:numeric_response +
#        School_type.y:numeric_response + 
#        proximity:numeric_response + 
#        Family_income_quintile.y:numeric_response + 
#        Age.y:numeric_response +
#        Gender.y:numeric_response + 
#        Paid_subscriptions.y:numeric_response + 
#        score +
#        I(Education_country.x == Education_country.y) +
#        Scholarship.x +
#        Guardian_one_education.x +
#        English_speaking_household.x +
#        I(English_speaking_household.x==English_speaking_household.y) +
#        Mobile_plan_type.x +
#        School_type.x +
#        I(School_type.x == School_type.y) +
#        Gender.x + 
#        proximity +
#        Family_income_quintile.x +
#        Age.x +
#        I((Age.x - Age.y)^2) +
#        I(Family_income_quintile.x == Family_income_quintile.y) +
#        I(Gender.x == Gender.y) + (1 | group)  + 
#        (1 | demographics_id) + (1 + numeric_response| Trait) + 
#        (1 | `Question text`), data = peer_review_with_demogs)