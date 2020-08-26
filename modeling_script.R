
library(flexdashboard);  library(tidyverse); library(googlesheets4); library(yaml); library(rstan)
creds <- yaml::read_yaml("creds.yaml")


googlesheets4::gs4_auth(email = creds$email)

tracker_data_raw <- read_sheet(ss = creds$tracker$url, sheet = creds$tracker$tracking_sheet, skip = 1, col_types = "cnccccnlllllllnlllc") %>% 
  mutate(`Student ID` = tolower(`Student ID`))

tracker_data <- tracker_data_raw %>% 
  filter(!is.na(Name)) %>% 
  select(`1. Consent`:`7. Feedback`)

rmet_responses <- read_sheet(ss = creds$rmet$url, sheet = creds$rmet$responses_sheet) 
rmet_answers <- read_sheet(ss = creds$rmet$url, sheet = creds$rmet$answers) 

rmet_answers <- rmet_answers %>% 
  mutate(question_number = 1:n())

rmet_responses_long <- rmet_responses %>% 
  gather(Question, Answer, -1:-3) %>% 
  mutate(question_number = as.numeric(str_extract(Question, "[0-9]{1,2}")) - 3) %>% 
  left_join(rmet_answers, by = "question_number") %>% 
  group_by(`What is your name? (First then last)`) %>% 
  filter(Timestamp == min(Timestamp)) %>% 
  summarize(`Student ID` = first(`What is your student ID? (This is a unique numeric ID  associated with you at your school; if you do not have one, use the email with which we have corresponded with you.)`),
            score = sum(Answer == `Correct Answer`)) %>% 
  ungroup %>% 
  mutate(Percentile = ntile(score, 100), 
         `Student ID` = tolower(`Student ID`))


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
  left_join(tracker_data_raw %>% select(`Student ID`, Name), by = "Student ID")

teacher_ratings <- read_sheet(creds$teacher_interviews$url, sheet = creds$teacher_interviews$group_1, col_types = "ccnnnnnnnnnc") %>% 
  mutate(group = creds$teacher_interviews$group_1)

for(s in creds$teacher_interviews[3:length(creds$teacher_interviews)]) {
  teacher_ratings <- bind_rows(teacher_ratings,
                               read_sheet(creds$teacher_interviews$url, sheet = s, col_types = "ccnnnnnnnnnc") %>% 
                                 mutate(group = s))
}

teacher_ratings <- teacher_ratings %>% 
  mutate(`Student ID` = tolower(`Student ID`))

teacher_ratings_long <- teacher_ratings %>%   
  select(Student:`Interview-ability`, group) %>%
  gather(Trait, `Teacher Score`, -Student:-`Student ID`, -group) %>% 
  group_by(Trait, group) %>% 
  mutate(`Teacher Score Raw` = `Teacher Score`,
         `Teacher Score` = scale(`Teacher Score`)) %>% 
  ungroup %>% 
  group_by(Student) %>% 
  mutate(Interviewability = `Teacher Score`[Trait == "Interview-ability"], 
         Creativity = `Teacher Score`[Trait == "Creativity"]) %>% 
  rename(Name = Student)


peer_nominations <- read_sheet(creds$peer_nominations$url, creds$peer_nominations$group_1) %>%
  mutate(`Student ID` = as.character(`Student ID`), 
         group_peer = creds$peer_nominations$group_1)

for(s in creds$peer_nominations[3:length(creds$peer_nominations)]) {
  peer_nominations <- bind_rows(peer_nominations,
                                read_sheet(creds$peer_nominations$url, sheet = s)%>% 
                                  mutate(group_peer = s)) 
}

peer_nominations <- peer_nominations %>% 
  mutate(`Student ID` = tolower(`Student ID`))

self_ratings <- peer_nominations %>% 
  select(`Student ID`, `Where would you place yourself in terms of intelligence?`:`Where would you place yourself relative to your peers in terms of need? If given more resources, how much more likely would you be to accomplish your dreams?`, group_peer)

self_ratings_long <- self_ratings %>% 
  rename(Intelligence = `Where would you place yourself in terms of intelligence?`, 
         Integrity = `Where would you place yourself in terms of integrity?`, 
         Perseverance = `Where would you place yourself in terms of perseverance?`, 
         Empathy = `Where would you place yourself in terms of empathy?`, 
         Spike = `Where would you place yourself relative to your peers in terms of how driven and passionate you are? How likely are you to dream a project and implement it?`, 
         Boost = `Where would you place yourself relative to your peers in terms of need? If given more resources, how much more likely would you be to accomplish your dreams?`) %>% 
  gather(Trait, Response, -group_peer, -`Student ID`) %>% 
  mutate(Score = parse_number(Response),
         `Student ID` = tolower(`Student ID`)) %>% 
  group_by(group_peer, Trait) %>% 
  mutate(scaled_score = scale(Score)) %>% 
  left_join(tracker_data_raw %>% select(Name, `Student ID`) %>% 
              mutate(`Student ID` = tolower(`Student ID`)), by = "Student ID") 

peer_noms_raw <- peer_nominations %>% 
  select(`Student ID`, contains("classmate"), group_peer) %>% 
  rename(`Nominator ID` = `Student ID`) %>%
  gather(nomination, Name, -group_peer, -`Nominator ID`) %>% 
  mutate(nomination_number = readr::parse_number(nomination),
         nomination = gsub(" [0-9]", "", nomination)) %>% 
  mutate(Trait = case_when(nomination == "Boostable classmate" ~ "Boost",
                           nomination == "Intelligent classmate" ~ "Intelligence",
                           nomination == "Persevering classmate" ~ "Perseverance",
                           nomination == "High integrity classmate" ~ "Integrity",
                           nomination == "Empathetic classmate" ~ "Empathy",
                           TRUE ~ "Spike")) %>% 
  select(-nomination) %>% 
  left_join(tracker_data_raw %>% select(Name, `Student ID`), by = "Name") %>% 
  arrange(`Nominator ID`, Trait, nomination_number)

possible_students <- teacher_ratings_long %>% 
  group_by(group) %>% 
  summarise(students = list(unique(Name)))

student_numbers <- possible_students %>%
  unnest(cols = c(students)) %>% 
  arrange(students) %>% 
  mutate(student_index = 1:n()) %>% 
  rename(Name = students)

peer_noms_raw <- peer_noms_raw %>% 
  mutate(Name = ifelse(Name==creds$names_to_replace$name_1$wrong_name, creds$names_to_replace$name_1$correct_name, Name))

modeling_df <- peer_noms_raw %>% 
  #filter(group_peer=="Beacon") %>% 
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
  spread(students, in_comparison, fill = 0) %>% 
  arrange(`Nominator ID`, Trait, nomination_number)


attributes <- modeling_df %>% 
  group_by(trait_index) %>% 
  summarise(Trait = first(Trait))

groups <- modeling_df %>% 
  group_by(group_index) %>% 
  summarise(group = first(group_peer))
  
student_numbers_2 <- student_numbers %>% 
  left_join(groups, by = c("group" = "group"))

teacher_ratings_for_model <- teacher_ratings_long %>% 
  #filter(group == "Beacon") %>%
  filter(Trait %in% unique(modeling_df$Trait) & `Teacher Score Raw`>0) %>% 
  left_join(attributes, by = "Trait") %>% 
  left_join(student_numbers, by = c("Name", "group")) %>% 
  left_join(groups, by = "group")

compiled_model <- stan_model("measurement_model_w_teacher_rankings_multischool.stan")

data_list <- list(N = nrow(modeling_df), 
                  N2 = nrow(teacher_ratings_for_model),
                  S = max(student_numbers$student_index),
                  A = max(modeling_df$trait_index),
                  C = max(groups$group_index),
                  student = modeling_df$student_index,
                  student_2 = teacher_ratings_for_model$student_index,
                  attribute = modeling_df$trait_index, 
                  attribute_2 = teacher_ratings_for_model$trait_index,
                  comparisons = as.matrix(modeling_df %>% select(`Abhay Sarkate`:`Vivek Jadhav`)),
                  teacher_score = teacher_ratings_for_model$`Teacher Score Raw`,
                  group_2 = teacher_ratings_for_model$group_index,
                  group_3 = student_numbers_2$group_index,
                  n_cut = max(teacher_ratings_for_model$`Teacher Score Raw`)-1)

test_run <- sampling(compiled_model, data = data_list, iter = 1000, cores = 4, chains = 4)


eigenvalues <- matrix(get_posterior_mean(test_run, "eigenvalues")[,5], 6, 3)
proportions_of_variance <- as.data.frame(apply(eigenvalues[6:1,], 2, function(x) x/sum(x)))

names(proportions_of_variance) <- groups$group
proportions_of_variance %>% 
  mutate(Component = 1:n()) %>% 
  gather(School, `Proportion of variance explained`, -Component) %>% 
  ggplot(aes(x = Component, y = `Proportion of variance explained`, colour = School)) +
  geom_line() +
  geom_point() +
  ggthemes::theme_hc() +
  labs(title = "Scree plot for explanation of variance")



eigenvectors <- array(get_posterior_mean(test_run, "eigenvectors")[,5], dim(6, 6), byrow =T)


peer_scores <- matrix(get_posterior_mean(test_run, "theta")[,5], max(student_numbers$student_index), ncol = max(modeling_df$trait_index), byrow = T) %>% 
  as.data.frame()

names(peer_scores) <- attributes$Trait

bind_cols(student_numbers, peer_scores) %>% View()
  write_sheet(ss =creds$tracker$url, sheet = "Ground truth scores")

