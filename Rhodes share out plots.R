# Load libraries and credentials----
library(flexdashboard);  library(tidyverse); library(googlesheets4); library(yaml); library(cmdstanr); library(posterior); library(recipes); library(ggridges); library(glmnet)
creds <- yaml::read_yaml("creds.yaml")
googlesheets4::gs4_auth(email = creds$email)

date_of_analysis <- Sys.Date()

# Load tracker data  ------------------------------------------------------

tracker_data_raw <- read_sheet(ss = creds$tracker$url, sheet = creds$tracker$tracking_sheet, skip = 1, col_types = "cnccccnlllllllnlllc") %>% 
  mutate(`Student ID` = tolower(`Student ID`))

tracker_data <- tracker_data_raw %>% 
  filter(!is.na(Name)) %>% 
  select(`1. Consent`:`7. Feedback`)

# Load RMET data  ---------------------------------------------------------

if(paste0(date_of_analysis, "-RMET-results.RData") %in% dir()) {
  load(paste0(date_of_analysis, "-RMET-results.RData"))
} else {
  source("clean_rmet.R")
}


# Load clean teacher nominations data ----

if(paste0(date_of_analysis, "-teacher_scores.RData") %in% dir()) {
  load(paste0(date_of_analysis, "-teacher_scores.RData"))
} else {
  source("clean_teacher_scores.R")
}

# Load clean peer nominations data ---- 

if(paste0(date_of_analysis, "-peer_nominations_clean.RData") %in% dir()) {
  load(paste0(date_of_analysis, "-peer_nominations_clean.RData"))
} else {
  source("clean_peer_nominations.R")
}

# Load clean peer review data ---- 

if(paste0(date_of_analysis, "-peer_review_clean.RData") %in% dir()) {
  load(paste0(date_of_analysis, "-peer_review_clean.RData"))
} else {
  source("clean_peer_review.R")
}

# Load demographics data  -------------------------------------------------

if(paste0(date_of_analysis, "-demographics-and-proximity.RData") %in% dir()) {
  load(paste0(date_of_analysis, "-demographics-and-proximity.RData"))
} else {
  source("clean_demographics.R")
}

# Load interview data ----

interview_data <- read_sheet(ss = creds$interview_sheet$url, sheet = creds$interview_sheet$responses_sheet, col_types = "cccccccccccccccccccccc") %>% 
  select(`Candidate's Student ID (This is SOMETIMES their email! check the excel sheet)`, `Interviewer Name (you, the interviewer)`, Intelligence:Boost) %>% 
  gather(Trait, Rating, -`Candidate's Student ID (This is SOMETIMES their email! check the excel sheet)`, -`Interviewer Name (you, the interviewer)`) %>% 
  rename(`Student ID` = `Candidate's Student ID (This is SOMETIMES their email! check the excel sheet)`,
         Interviewer = `Interviewer Name (you, the interviewer)`) %>%
  mutate(`Interview Score` = as.numeric(readr::parse_number(Rating)),
         `Interview Score` = ifelse(`Interview Score` == -9, NA, `Interview Score`),
         `Student ID` = tolower(`Student ID`)) %>%
  ungroup %>% 
  select(Interviewer, `Student ID`, Trait, `Interview Score`) %>% 
  left_join(tracker_data_raw %>% select(`Student ID`, Name, `User ID`), by = "Student ID") %>% 
  filter(!is.na(`User ID`))

# De-identify and save data  ----------------------------------------------


# Create model data for ground truth, interviews and peer review ---- 


# Compile models  ---------------------------------------------------------

# Fit models for interviews ---- 

# Fit model for ground truth ----  

# Which concepts result in greatest agreement between peers and teachers  -----------------------------------------------

# Which concepts result in greatest agreement between interviewers? -----

interview_data %>% 
  filter(!is.na(`Interview Score`)) %>%
  group_by(Interviewer) %>% 
  mutate(score = scale(`Interview Score`)) %>%
  group_by(Trait, `User ID`) %>% 
  mutate(mean_score = mean(score)) %>% 
  ungroup %>% 
  mutate(uidt = paste0(Trait, `User ID`), 
         uidt = reorder(uidt, mean_score)) %>%
  group_by(Trait) %>%
  mutate(SD_individuals = sd(mean_score),
            SD = sd(score-mean_score)) %>% 
  ungroup %>% 
  mutate(signal_to_noise = SD_individuals/SD,
         Trait = reorder(Trait, -signal_to_noise)) %>% 
  ggplot(aes(x = uidt)) +
  geom_point(aes(y = score), colour = "red", alpha = 0.2) +
  geom_point(aes(y = mean_score)) +
  facet_wrap(~Trait) +
  theme(axis.text.x = element_blank(),axis.line.x = element_blank()) +
  labs(title = "High levels of agreement across all traits for interviews",
       subtitle = "Normalized interviewer (red) and average scores (black)",
       y = "Normalized interview scores",
       x = "Candidate",
       caption = "Traits ordered by signal to noise ratio")


# Which traits to teachers and peers agree on?  ---------------------------

possible_students <- teacher_ratings_long %>% 
  group_by(group) %>% 
  summarise(students = list(unique(Name[!is.na(Name)])))

student_numbers <- possible_students %>%
  unnest(cols = c(students)) %>% 
  arrange(students) %>% 
  mutate(student_index = 1:n()) %>% 
  rename(Name = students) %>%
  left_join(tracker_data_raw %>% select(Name, `User ID`, `Student ID`), by = "Name")

peer_noms_raw <- peer_noms_raw %>% 
  mutate(Name = ifelse(Name==creds$names_to_replace$name_1$wrong_name, creds$names_to_replace$name_1$correct_name, Name))

peer_nominations_modeling_data <- peer_noms_raw %>% 
  group_by(`Nominator ID`, Trait, nomination_number) %>% 
  slice(1L) %>%
  group_by(`Nominator ID`, Trait) %>%
  mutate(`First choice` = Name[nomination_number ==1],
         `Second choice` = Name[nomination_number == 2]) %>%
  left_join(possible_students, by = c("group_peer" = "group")) %>% 
  unnest(cols = c(students)) %>% 
  ungroup %>% 
  mutate(in_comparison = case_when(nomination_number == 1 ~ 1, 
                                   (nomination_number == 2 & students == `First choice`) ~ 0, 
                                   (nomination_number == 3 & (students == `First choice` | students == `Second choice`)) ~ 0, 
                                   TRUE ~ 1)) %>% 
  select(-`First choice`, -`Second choice`) %>%
  left_join(student_numbers, by = c("group_peer" = "group", "Name")) %>%
  mutate(trait_index = as.numeric(factor(Trait)),
         group_index = as.numeric(factor(group_peer))) %>% 
  arrange(`Nominator ID`, Trait) %>%
  left_join(student_numbers %>% select(group, Name, student_index) %>% rename(possibles = student_index), by = c("group_peer" = "group", "students" = "Name")) %>% 
  ungroup %>% 
  mutate(row = 1:n()) %>% 
  group_by(`Nominator ID`, Trait, nomination_number) %>% 
  mutate(possibles_lower = min(row),
         possibles_upper = max(row))

attributes <- peer_nominations_modeling_data %>% 
  group_by(trait_index) %>% 
  summarise(Trait = first(Trait))

groups <- peer_nominations_modeling_data %>% 
  group_by(group_index) %>% 
  summarise(group = first(group_peer))

peer_noms_n_long <- peer_nominations_modeling_data %>% 
  slice(1L)

student_school_index <- peer_nominations_modeling_data %>% 
  group_by(possibles) %>% 
  summarise(group = first(group_index),
            school = first(group_peer))

compiled_noms_model <- cmdstan_model("peer_nominations.stan")

data_for_peer_noms <- list(N = nrow(peer_noms_n_long), 
                           N_possibles = nrow(peer_nominations_modeling_data),
                           I = max(peer_nominations_modeling_data$possibles),
                           C = max(peer_nominations_modeling_data$trait_index),
                           S = max(peer_nominations_modeling_data$group_index),
                           school = peer_noms_n_long$group_index,
                           concept = peer_noms_n_long$trait_index,
                           winner = peer_noms_n_long$student_index,
                           possibles_lower = peer_noms_n_long$possibles_lower,
                           possibles_upper = peer_noms_n_long$possibles_upper,
                           possibles = peer_nominations_modeling_data$possibles,
                           individual_school = student_school_index$group)

if(! paste0(date_of_analysis, "-peer_noms_fit.RDS") %in% dir()) {
  fit_peer_nominations <- compiled_noms_model$sample(data = data_for_peer_noms, parallel_chains = 4, iter_warmup = 500, iter_sampling = 300, output_dir = ".")
  
  fit_peer_nominations$save_object(file = paste0(date_of_analysis, "-peer_noms_fit.RDS"))
} else {
  fit_peer_nominations <- readRDS(paste0(date_of_analysis, "-peer_noms_fit.RDS"))
}


student_nominations_thetas <- fit_peer_nominations$summary("theta")

student_nominations_thetas %>% 
  mutate(concept = parse_number(str_extract(variable, ",[0-9]{1,2}")),
         interior_90range = q95-q5,
         group = rep(student_school_index$school,  6)) %>% 
  ggplot(aes(x = median, y = interior_90range, colour = group)) +
  geom_point() +
  facet_wrap(~concept)


# Teacher scores ----------------------------------------------------------

teacher_ratings_for_model <- teacher_ratings_long %>% 
  filter(Trait %in% unique(peer_nominations_modeling_data$Trait) & `Teacher Score Raw`>0) %>% 
  left_join(attributes, by = "Trait") %>% 
  left_join(student_numbers, by = c("Name", "group")) %>% 
  left_join(groups, by = "group") 


compiled_teacher_score_model <- cmdstan_model("teacher_ratings_model.stan")

data_list_teacher_ratings <- list(N = nrow(teacher_ratings_for_model),
                                  I = max(teacher_ratings_for_model$student_index),
                                  C = max(teacher_ratings_for_model$trait_index),
                                  S = max(teacher_ratings_for_model$group_index),
                                  score = teacher_ratings_for_model$`Teacher Score Raw`,
                                  student = teacher_ratings_for_model$student_index,
                                  concept = teacher_ratings_for_model$trait_index,
                                  school = teacher_ratings_for_model$group_index,
                                  individual_school = student_school_index$group)
  
if(!paste0(date_of_analysis, "-teacher_model.RDS") %in% dir()) {
  teacher_scores_fit <- compiled_teacher_score_model$sample(data = data_list_teacher_ratings, parallel_chains = 4, iter_warmup = 500, iter_sampling = 300, output_dir = ".")
  teacher_scores_fit$save_object(file = paste0(date_of_analysis, "-teacher_model.RDS"))
} else {
  teacher_scores_fit <- readRDS(paste0(date_of_analysis, "-teacher_model.RDS"))
}


teacher_thetas <- teacher_scores_fit$summary("theta")

# Teacher vs peer scores --------------------------------------------------

tibble(var = teacher_thetas$variable, 
       teacher_theta = teacher_thetas$mean, 
       student_theta = student_nominations_thetas$mean,
       trait_index = parse_number(str_extract(var, ",[0-9]")),
       student_index = parse_number(str_extract(var, "[0-9]{1,3},"))) %>% 
  left_join(student_numbers) %>%
  left_join(attributes) %>% 
  group_by(Trait, group) %>%
  mutate(teacher_theta = scale(teacher_theta),
         student_theta = scale(student_theta)) %>%
  ggplot(aes(x = teacher_theta, y = student_theta)) +
  facet_wrap(~Trait) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(aes(intercept = 0, slope = 1)) +
  ggthemes::theme_hc() +
  labs(x = "Standardized teacher score", y = "Standardized classmate score", 
       title = "Teachers and peers tend to agree on students' qualities")



ground_truth_ingredients <- tibble(var = teacher_thetas$variable, 
                       teacher_theta = teacher_thetas$mean, 
                       student_theta = student_nominations_thetas$mean,
                       trait_index = parse_number(str_extract(var, ",[0-9]")),
                       student_index = parse_number(str_extract(var, "[0-9]{1,3},"))) %>% 
  left_join(student_numbers) %>%
  left_join(attributes) %>% 
  left_join(groups) %>%
  group_by(Trait, group) %>%
  mutate(teacher_theta = scale(teacher_theta),
         student_theta = scale(student_theta)) 


peer_teacher_scores <- ground_truth_ingredients %>% 
  ungroup %>% 
  select(student_index, group_index, Trait, student_theta, teacher_theta)
  

# Fit ground truth model --------------------------------------------------
compiled_ground_truth <- cmdstan_model("nomination_scores_to_ground_truth.stan")

ground_truth_data_list <- list(N = nrow(ground_truth_ingredients),
                               I = max(ground_truth_ingredients$student_index),
                               C = max(ground_truth_ingredients$trait_index),
                               S = max(ground_truth_ingredients$group_index),
                               school = ground_truth_ingredients$group_index,
                               concept = ground_truth_ingredients$trait_index,
                               student = ground_truth_ingredients$student_index,
                               teacher_score = ground_truth_ingredients$teacher_theta,
                               peer_score = ground_truth_ingredients$student_theta)


if(!paste0(date_of_analysis, "-ground_truth_model.RDS") %in% dir()) {
  ground_truth_fit <- compiled_ground_truth$sample(data = ground_truth_data_list, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 300, output_dir = ".")
  ground_truth_fit$save_object(file = paste0(date_of_analysis, "-ground_truth_model.RDS"))
} else {
  ground_truth_fit <- readRDS(paste0(date_of_analysis, "-ground_truth_model.RDS"))
}


ground_truth_thetas <- ground_truth_fit$summary("theta")

tibble(var = ground_truth_thetas$variable, 
       theta = ground_truth_thetas$mean, 
       trait_index = parse_number(str_extract(var, ",[0-9]")),
       student_index = parse_number(str_extract(var, "[0-9]{1,3},"))) %>%
  left_join(student_numbers) %>%
  left_join(attributes) %>% 
  left_join(groups) %>%
  select(student_index, Trait, theta, group_index) %>%
  spread(Trait, theta) %>% 
  select(-student_index) %>% 
  group_by(group_index) %>% 
  do({
    tt <- function(x) {
      tmp <- x %>% select(Boost:Spike) %>% as.matrix() %>% cov %>% eigen
      data.frame(t(tmp$values))
    }
    tt(.)
  }) %>% 
  ungroup %>%
  gather(Component, Eigenvalue, -group_index) %>% 
  arrange(group_index) %>% 
  group_by(group_index) %>% 
  mutate(cumprop = cumsum(Eigenvalue)/sum(Eigenvalue)) %>% 
  ggplot(aes(x = Component, y = cumprop, group = group_index, colour = factor(group_index) )) +
  geom_point()+
  geom_line() +
  ylim(0, 1) +
  labs("Principal components",
       title = "Most variation in 'ground truth' is explained by two unrelated factors",
       y = NULL,
       subtitle = "Cumulative proportion of variation explained",
       colour = "Class group") +
  ggthemes::theme_hc()
  


# Ground truth by demogs --------------------------------------------------

ground_truth_scores <- tibble(var = ground_truth_thetas$variable, 
                              theta = ground_truth_thetas$mean, 
                              trait_index = parse_number(str_extract(var, ",[0-9]")),
                              student_index = parse_number(str_extract(var, "[0-9]{1,3},"))) %>%
  left_join(student_numbers) %>%
  left_join(attributes) %>% 
  left_join(groups) %>% 
  select(student_index, group_index, Trait, `User ID`,theta)

tibble(var = ground_truth_thetas$variable, 
       theta = ground_truth_thetas$mean, 
       trait_index = parse_number(str_extract(var, ",[0-9]")),
       student_index = parse_number(str_extract(var, "[0-9]{1,3},"))) %>%
  left_join(student_numbers) %>%
  left_join(attributes) %>% 
  left_join(groups) %>% 
  left_join(demographics_2, by = c("Student ID" = "Student.ID")) %>% 
  filter(!is.na(Family_income_quintile)) %>%
  mutate(Income_quintile = case_when(Family_income_quintile == "Fifth quintile (Bottom 20%)" ~ 1, 
                                     Family_income_quintile == "Fourth quintile" ~ 2, 
                                     Family_income_quintile == "Third quintile" ~ 3,
                                     Family_income_quintile == "Second quintile" ~ 4,
                                     Family_income_quintile == "Top quintile (Top 20%)" ~ 5,
                                     TRUE ~ NA_real_)) %>%
  group_by(Trait, Income_quintile) %>% 
   summarise(N = n(),
             theta = mean(theta)) %>% 
  ggplot(aes(x = Income_quintile, y = theta, group = Trait)) +
  geom_point() +
  geom_line() +
  ylim(-1,1)+
  facet_wrap(~Trait) +
  ggthemes::theme_hc() +
  labs(x="Household income quintile",subtitle = "Average normalized score for students within each income quintile", y = NULL)


# Interview model ---------------------------------------------------------


interview_data_model <- interview_data %>% 
  filter(!is.na(`Interview Score`) & `Interview Score`>0) %>% 
  left_join(student_numbers %>% select(Name, student_index, group), by = "Name") %>% 
  left_join(attributes, by = "Trait") %>%
  ungroup %>%
  mutate(interviewer_index = as.numeric(factor(Interviewer))) %>% 
  filter(!is.na(group)) %>% 
  mutate(student_index_2 = as.numeric(ordered(student_index)))


interviewer_mod_compiled <- cmdstan_model("interview_model.stan")

interview_model_data <- list(N= nrow(interview_data_model), 
                             R = max(interview_data_model$interviewer_index),
                             J = max(interview_data_model$student_index_2),
                             C = max(interview_data_model$trait_index),
                             y = interview_data_model$`Interview Score`, 
                             reviewer = interview_data_model$interviewer_index,
                             reviewee = interview_data_model$student_index_2,
                             concept =  interview_data_model$trait_index)


if(!paste0(date_of_analysis, "-interview_model.RDS") %in% dir()) {
  interview_fit <- interviewer_mod_compiled$sample(data = interview_model_data, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 300, output_dir = ".")
  interview_fit$save_object(file = paste0(date_of_analysis, "-interview_model.RDS"))
} else {
  interview_fit <- readRDS(paste0(date_of_analysis, "-interview_model.RDS"))
}

interview_student_indexes <- interview_data_model %>% 
  group_by(student_index_2) %>% 
  summarise(group = first(group), 
            student_index = first(student_index),
            `User ID` = first(`User ID`))

interview_thetas <- interview_fit$summary("latent_scores")

interview_scores <- tibble(var = interview_thetas$variable, 
                           interview_score = interview_thetas$mean, 
                           trait_index = parse_number(str_extract(var, ",[0-9]")),
                           student_index_2 = parse_number(str_extract(var, "[0-9]{1,3},"))) %>%
  left_join(interview_student_indexes) %>%
  left_join(attributes) %>% 
  left_join(groups) %>%
  select(Trait, group_index, student_index, `User ID`, interview_score)

interview_scores %>% 
  left_join(ground_truth_scores) %>% 
  left_join(peer_teacher_scores) %>% 
  group_by(Trait, group_index) %>% 
  mutate(interview_score = scale(interview_score),
         teacher_theta = scale(teacher_theta)) %>%
  ggplot(aes(x = interview_score, y = teacher_theta)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~Trait) +
  geom_smooth(method = "lm") +
  labs(x = "Interview score", y = "Teacher score",
       title = "Interviewers did not agree with teachers") +
  ggthemes::theme_hc()

interview_scores %>% 
  left_join(ground_truth_scores) %>% 
  left_join(peer_teacher_scores) %>% 
  group_by(Trait, group_index) %>% 
  mutate(interview_score = scale(interview_score),
         student_theta = scale(student_theta)) %>%
  ggplot(aes(x = interview_score, y = student_theta)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~Trait) +
  geom_smooth(method = "lm") +
  labs(x = "Interview score", y = "Peer score", title ="Agreement with peers was better") +
  ggthemes::theme_hc()


# Interviews vs ground truth plot, no schools -----------------------------

interview_scores %>% 
  left_join(ground_truth_scores) %>% 
  left_join(peer_teacher_scores) %>% 
  group_by(Trait, group_index) %>% 
  mutate(interview_score = scale(interview_score),
         theta = scale(theta)) %>%
  ggplot(aes(x = interview_score, y = theta)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~Trait) +
  geom_smooth(method = "lm") +
  labs(x = "Interview score", y = "'Ground truth' score", title = "So some agreement on 'ground truth'") +
  ggthemes::theme_hc()




# Plots of ground truth vs academics  -------------------------------------

interview_scores %>% 
  left_join(ground_truth_scores) %>% 
  left_join(peer_teacher_scores) %>% 
  left_join(student_numbers %>% select(`Student ID`, student_index)) %>%
  mutate(`Student ID` = tolower(`Student ID`)) %>%
  group_by(Trait, group_index) %>% 
  left_join(demographics_2, by = c("Student ID" = "Student.ID")) %>% 
  filter(!is.na(Family_income_quintile)) %>%
  mutate(Income_quintile = case_when(Family_income_quintile == "Fifth quintile (Bottom 20%)" ~ 1, 
                                     Family_income_quintile == "Fourth quintile" ~ 2, 
                                     Family_income_quintile == "Third quintile" ~ 3,
                                     Family_income_quintile == "Second quintile" ~ 4,
                                     Family_income_quintile == "Top quintile (Top 20%)" ~ 5,
                                     TRUE ~ NA_real_)) %>%
  group_by(Trait, Income_quintile) %>% 
  summarise(N = n(),
            theta = mean(theta),
            student_theta = mean(student_theta),
            teacher_theta = mean(teacher_theta)) %>% 
  ggplot(aes(x = Income_quintile, y = theta, group = Trait)) +
  geom_point() +
  geom_line() +
  ylim(-1.5,1.5)+
  facet_wrap(~Trait) +
  ggthemes::theme_hc() +
  labs(x="Household income quintile",title = "On most traits, evidence of a 'scholarship student effect'",subtitle = "Normalized 'ground truth' score for students within each income quintile", y = NULL)

interview_scores %>% 
  left_join(ground_truth_scores) %>% 
  left_join(peer_teacher_scores) %>% 
  left_join(student_numbers %>% select(`Student ID`, student_index)) %>%
  mutate(`Student ID` = tolower(`Student ID`)) %>%
  group_by(Trait, group_index) %>% 
  left_join(demographics_2, by = c("Student ID" = "Student.ID")) %>% 
  filter(!is.na(Family_income_quintile)) %>%
  mutate(Income_quintile = case_when(Family_income_quintile == "Fifth quintile (Bottom 20%)" ~ 1, 
                                     Family_income_quintile == "Fourth quintile" ~ 2, 
                                     Family_income_quintile == "Third quintile" ~ 3,
                                     Family_income_quintile == "Second quintile" ~ 4,
                                     Family_income_quintile == "Top quintile (Top 20%)" ~ 5,
                                     TRUE ~ NA_real_)) %>%
  group_by(Trait, Income_quintile) %>% 
  summarise(N = n(),
            theta = mean(theta),
            student_theta = mean(student_theta),
            teacher_theta = mean(teacher_theta)) %>% 
  ggplot(aes(x = Income_quintile, y = student_theta, group = Trait)) +
  geom_point() +
  geom_line() +
  ylim(-1.5,1.5)+
  facet_wrap(~Trait) +
  ggthemes::theme_hc() +
  labs(x="Household income quintile",title = "Peer scores drive the trend",subtitle = "Average score assigned by peers to classmates from different household incomes", y = NULL)


interview_scores %>% 
  left_join(ground_truth_scores) %>% 
  left_join(peer_teacher_scores) %>% 
  left_join(student_numbers %>% select(`Student ID`, student_index)) %>%
  mutate(`Student ID` = tolower(`Student ID`)) %>%
  group_by(Trait, group_index) %>% 
  left_join(demographics_2, by = c("Student ID" = "Student.ID")) %>% 
  filter(!is.na(Family_income_quintile)) %>%
  mutate(Income_quintile = case_when(Family_income_quintile == "Fifth quintile (Bottom 20%)" ~ 1, 
                                     Family_income_quintile == "Fourth quintile" ~ 2, 
                                     Family_income_quintile == "Third quintile" ~ 3,
                                     Family_income_quintile == "Second quintile" ~ 4,
                                     Family_income_quintile == "Top quintile (Top 20%)" ~ 5,
                                     TRUE ~ NA_real_)) %>%
  group_by(Trait, Income_quintile) %>% 
  summarise(N = n(),
            theta = mean(theta),
            student_theta = mean(student_theta),
            teacher_theta = mean(teacher_theta),
            interview_theta = mean(interview_score)) %>% 
  ggplot(aes(x = Income_quintile, y = teacher_theta, group = Trait)) +
  geom_point() +
  geom_line() +
  ylim(-1.5,1.5)+
  facet_wrap(~Trait) +
  ggthemes::theme_hc() +
  labs(x="Household income quintile",title = "And teachers gave much lower scores to students from richer backgrounds",subtitle = "Average score assigned by teachers to classmates from different household incomes", y = NULL)

interview_scores %>% 
  left_join(ground_truth_scores) %>% 
  left_join(peer_teacher_scores) %>% 
  left_join(student_numbers %>% select(`Student ID`, student_index)) %>%
  mutate(`Student ID` = tolower(`Student ID`)) %>%
  group_by(Trait, group_index) %>% 
  left_join(demographics_2, by = c("Student ID" = "Student.ID")) %>% 
  filter(!is.na(Family_income_quintile)) %>%
  mutate(Income_quintile = case_when(Family_income_quintile == "Fifth quintile (Bottom 20%)" ~ 1, 
                                     Family_income_quintile == "Fourth quintile" ~ 2, 
                                     Family_income_quintile == "Third quintile" ~ 3,
                                     Family_income_quintile == "Second quintile" ~ 4,
                                     Family_income_quintile == "Top quintile (Top 20%)" ~ 5,
                                     TRUE ~ NA_real_)) %>%
  group_by(Trait, Income_quintile) %>% 
  summarise(N = n(),
            theta = mean(theta),
            student_theta = mean(student_theta),
            teacher_theta = mean(teacher_theta),
            interview_theta = mean(interview_score)) %>% 
  ggplot(aes(x = Income_quintile, group = Trait)) +
  geom_point(aes(y = interview_theta), alpha= 0.3) +
  geom_line( aes(y = interview_theta), alpha= 0.3) +
  ylim(-1.5,1.5)+
  facet_wrap(~Trait) +
  ggthemes::theme_hc() +
  labs(x="Household income quintile",title = "Yet interviewers did not",subtitle = "Average score assigned by interviewers to candidates from different household incomes", y = NULL)


interview_scores %>% 
  left_join(ground_truth_scores) %>% 
  left_join(peer_teacher_scores) %>% 
  left_join(student_numbers %>% select(`Student ID`, student_index)) %>%
  mutate(`Student ID` = tolower(`Student ID`)) %>%
  group_by(Trait, group_index) %>% 
  left_join(demographics_2, by = c("Student ID" = "Student.ID")) %>% 
  filter(!is.na(Family_income_quintile)) %>%
  mutate(Income_quintile = case_when(Family_income_quintile == "Fifth quintile (Bottom 20%)" ~ 1, 
                                     Family_income_quintile == "Fourth quintile" ~ 2, 
                                     Family_income_quintile == "Third quintile" ~ 3,
                                     Family_income_quintile == "Second quintile" ~ 4,
                                     Family_income_quintile == "Top quintile (Top 20%)" ~ 5,
                                     TRUE ~ NA_real_)) %>%
  group_by(Trait, Income_quintile) %>% 
  summarise(N = n(),
            theta = mean(theta),
            student_theta = mean(student_theta),
            teacher_theta = mean(teacher_theta),
            interview_theta = mean(interview_score)) %>% 
  mutate(help_hurt = ifelse(theta>interview_theta, "1", "2")) %>%
  ggplot(aes(x = Income_quintile, group = Trait)) +
  geom_point(aes(y = interview_theta), alpha= 0.4) +
  geom_line( aes(y = interview_theta), alpha= 0.4) +
  geom_point(aes(y = theta)) +
  geom_line(aes(y = theta)) +
  geom_text(data = tribble(~Trait, ~x, ~y, ~label,
                           "Boost", 1.5, -0.6, "Interview",
                           "Boost", 4, 0.7, "Ground Truth"), 
            aes(x = x, y = y, label = label, alpha = label)) +
  scale_alpha_manual(values = c(1, .4)) +
  geom_linerange(aes(ymin = theta, ymax = interview_theta, colour = help_hurt)) +
  ylim(-1.5,1.5)+
  facet_wrap(~Trait) +
  ggthemes::theme_hc() +
  guides(alpha= F, colour = F) +
  labs(alpha = NULL, colour = NULL, 
       x="Household income quintile",
       title = "Interviewers boosted richer candidates, and penalized poorer students",subtitle = "Average scores", y = NULL)


# Plots of interviewability vs interviews ---------------------------------



# Bias plots --------------------------------------------------------------



# Double residualization plots, no school ID  -----------------------------



# Create "ground truth plus" ---- 

# Fit model for peer review ----

# 


