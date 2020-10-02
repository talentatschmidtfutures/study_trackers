library(glmnet)
library(flexdashboard);  library(tidyverse); library(googlesheets4); library(yaml); library(cmdstanr); library(posterior); library(recipes); library(ggridges)
creds <- yaml::read_yaml("creds.yaml")


googlesheets4::gs4_auth(email = creds$email)

if(paste0(Sys.Date(), "-demographics-and-proximity.RData") %in% dir()) {
  load(paste0(Sys.Date(), "-demographics-and-proximity.RData"))
} else {
  source("clean_demographics.R")
}


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
  filter(Timestamp == max(Timestamp)) %>% 
  summarize(`Student ID` = first(`What is your student ID? (Use the email with which we have corresponded with you.)`),
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
                                  mutate(`Student ID` = as.character(`Student ID`),
                                         group_peer = s)) 
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
  filter(Trait %in% unique(modeling_df$Trait) & `Teacher Score Raw`>0) %>% 
  left_join(attributes, by = "Trait") %>% 
  left_join(student_numbers, by = c("Name", "group")) %>% 
  left_join(groups, by = "group") 

compiled_model <- cmdstan_model("measurement_model_w_teacher_rankings_multischool.stan")

data_list <- list(N = nrow(modeling_df), 
                  N2 = nrow(teacher_ratings_for_model),
                  S = max(student_numbers$student_index),
                  A = max(modeling_df$trait_index),
                  C = max(groups$group_index),
                  student = modeling_df$student_index,
                  student_2 = teacher_ratings_for_model$student_index,
                  attribute = modeling_df$trait_index, 
                  attribute_2 = teacher_ratings_for_model$trait_index,
                  comparisons = as.matrix(modeling_df %>% select(-`Nominator ID`:-group_index)),
                  teacher_score = teacher_ratings_for_model$`Teacher Score Raw`,
                  group_2 = teacher_ratings_for_model$group_index,
                  group_3 = student_numbers_2$group_index,
                  n_cut = max(teacher_ratings_for_model$`Teacher Score Raw`)-1)

lapply(data_list, function(x) mean(is.na(x)))

test_run <- compiled_model$sample(data = data_list, parallel_chains = 2, iter_warmup = 300, iter_sampling = 300)

eigenvalues_draws <- as_draws_df(test_run$draws("eigenvalues"))



eigenvalues <- matrix(colMeans(eigenvalues_draws)[1:(6*length(unique(modeling_df$group_index)))], 6, data_list$C, byrow = T)
proportions_of_variance <- as.data.frame(apply(eigenvalues[6:1,], 2, function(x) x/sum(x)))

names(proportions_of_variance) <- groups$group

proportions_of_variance %>% 
  mutate(Component = 1:n()) %>% 
  gather(School, `Proportion of variance explained`, -Component) %>% 
  group_by(School) %>% 
  mutate(cs = cumsum(`Proportion of variance explained`)) %>%
  ungroup %>% 
  mutate(School = paste("Class", as.numeric(as.factor(School)))) %>%
  ggplot(aes(x = Component, y = cs*100, colour = School)) +
  geom_line() +
  geom_point() +
  ggthemes::theme_hc() +
  ylim(0, 100) +
  labs(y = "% of total variation explained",title = "Cumulative proportion of total variance explained by # of uncorrelated traits", x = "Number of uncorrelated traits")


theta_draws <- as_draws_df(test_run$draws("theta"))
theta_draws <- theta_draws[,1:(ncol(theta_draws)-3)]

peer_scores <- matrix(colMeans(theta_draws), max(student_numbers$student_index), ncol = max(modeling_df$trait_index)) %>% 
  as.data.frame()

names(peer_scores) <- attributes$Trait

bind_cols(student_numbers, peer_scores) %>% 
  left_join(tracker_data_raw %>% select(`User ID`, `Student ID`, Name), by = "Name") %>% 
  write_sheet(ss =creds$tracker$url, sheet = "Ground truth scores")

bind_cols(student_numbers, peer_scores) %>% 
  select(group, Name, Boost:Spike) %>% 
  gather(Trait, Score, -group, -Name) %>%
  ggplot(aes(x = Score, fill = group)) + geom_density(alpha = 0.4) +
  facet_wrap(~Trait)

ground_truth <- bind_cols(student_numbers, peer_scores) %>% 
  select(group, Name, Boost:Spike) %>% 
  gather(Trait, Score, -group, -Name) %>%
  left_join(tracker_data_raw %>% select(`User ID`, `Student ID`, Name), by = "Name") 

teacher_ratings_long %>% 
  left_join(peer_noms_raw %>% 
              group_by(Name, Trait) %>% 
              summarise(`# Peer nominations` = n()), by = c("Name", "Trait")) %>%
  group_by(group, Trait) %>% 
  mutate(`Proportion of peer nominations` = `# Peer nominations`/sum(`# Peer nominations`, na.rm = T)) %>%
  ggplot(aes(x = `Proportion of peer nominations`, y = `Teacher Score Raw`, colour = group)) +
  geom_point() +
  facet_wrap(~Trait) +
  geom_smooth(method = "lm", aes(group = 1)) +
  ggthemes::theme_hc() +
  labs(title = "Peers and teachers tend to agree on traits")

all_measurements <- interview_data %>%
  group_by(Interviewer) %>% 
  mutate(scaled_score = scale(`Interview Score`)) %>% 
  group_by(`Student ID`, Name, Trait) %>% 
  summarise(`Interview score` = mean(scaled_score, na.rm = T)) %>%
  left_join(ground_truth, by = c("Student ID", "Name", "Trait")) %>% 
  group_by(group) %>% 
  mutate(`Interview score` = scale(`Interview score`)) %>% 
  filter(!is.na(`Interview score`) & !is.na(Score)) %>% 
  ungroup


interview_data_model <- interview_data %>% 
  filter(!is.na(`Interview Score`) & `Interview Score`>0) %>% 
  left_join(student_numbers_2, by = "Name") %>% 
  left_join(attributes, by = "Trait") %>%
  ungroup %>%
  mutate(interviewer_index = as.numeric(factor(Interviewer))) %>% 
  filter(!is.na(group))



interviewer_mod_compiled <- cmdstan_model("interview_model.stan")

interview_model_data <- list(N= nrow(interview_data_model), 
                             R = max(interview_data_model$interviewer_index),
                             J = max(student_numbers$student_index),
                             C = max(interview_data_model$trait_index),
                             y = interview_data_model$`Interview Score`, 
                             reviewer = interview_data_model$interviewer_index,
                             reviewee = interview_data_model$student_index,
                             concept =  interview_data_model$trait_index)
  

interview_fit <- interviewer_mod_compiled$sample(data = interview_model_data, parallel_chains = 4, iter_warmup = 500, iter_sampling = 500)



interview_scores_df <- as_draws_df(interview_fit$draws("latent_scores"))
interview_scores_df <- interview_scores_df[,1:(ncol(interview_scores_df)-3)]

interview_scores <- as.data.frame(matrix(colMeans(interview_scores_df), max(student_numbers$student_index), 6))
names(interview_scores) <- attributes$Trait

ground_truth_and_interview <- bind_cols(student_numbers,interview_scores) %>% 
  filter(student_index %in% interview_data_model$student_index) %>% 
  gather(Trait, Interview_score, -Name, -group, -student_index) %>% 
  left_join(ground_truth) %>% 
  left_join(demographics_2, by = c("Student ID" = "Student.ID")) %>% 
  select(-Alternate_email) %>%
  filter(complete.cases(.))

ground_truth_and_interview2 <- bind_cols(student_numbers,interview_scores) %>% 
  filter(student_index %in% interview_data_model$student_index) %>% 
  gather(Trait, Interview_score, -Name, -group, -student_index) %>% 
  left_join(ground_truth) %>% 
  left_join(demographics, by = c("Student ID" = "Student ID")) %>% 
  filter(!is.na(Score), !is.na(Interview_score))



receta_1 <- recipe(Score + Interview_score ~ Age + ESL + Gender + English_speaking_household + Education_country + Scholarship + School_type + Guardian_one_education + Guardian_two_education + Family_income_quintile + Mobile_plan_type + Paid_subscriptions, data= ground_truth_and_interview) %>%
  step_dummy(all_nominal()) %>% 
  step_normalize(Age, Paid_subscriptions)

prepped_1 <- prep(receta_1, new_data = ground_truth_and_interview)
juiced_1 <- bake(prepped_1, new_data = ground_truth_and_interview)



y_y <- juiced_1$Score
y_x <- juiced_1$Interview_score

X <- juiced_1 %>% 
  select(-Score, -Interview_score) %>% 
  as.matrix()



fit_y <- cv.glmnet(X, y_y)
fit_x <- cv.glmnet(X, y_x)

resid_y <- y_y - as.numeric(predict(fit_y, newx = X, s = "lambda.min"))
resid_x <- y_y - as.numeric(predict(fit_x, newx = X, s = "lambda.min"))

length(unique(ground_truth_and_interview$`User ID`))

ground_truth_and_interview %>% 
  group_by(class = group, Trait) %>% 
  mutate(Interview_score = scale(Interview_score),
         Score = scale(Score)) %>%
  ggplot(aes(x = Interview_score, y = Score, colour = class)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~Trait) +
  ggthemes::theme_hc() +
  geom_smooth(method = "lm", aes(group = 1)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  labs(title = "Interviewer scores were weakly related to our ground truth measures", 
       subtitle = "Within-class scores")

tibble(Trait = ground_truth_and_interview$Trait, 
       class = ground_truth_and_interview$group, 
       `Interview residual` = resid_x, 
       `Ground truth residual` = resid_y) %>% 
  group_by(class, Trait) %>% 
  mutate(`Interview residual` = scale(`Interview residual`),
         `Ground truth residual` = scale(`Ground truth residual`)) %>%
  ggplot(aes(x = `Interview residual`, y = `Ground truth residual`, colour = class)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~Trait) +
  ggthemes::theme_hc() +
  geom_smooth(method = "lm", aes(group = 1)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  labs(title = "After controlling for systematic bias, interviewer scores are extremely valuable", 
       subtitle = "Within-class double residual plot after regressing scores on demographics")

ground_truth_and_interview2$resid <- resid(lm(Score ~ Interview_score*Trait, data = ground_truth_and_interview2))

save(ground_truth_and_interview, file = paste0(Sys.Date(), "-ground_truth_estimates.RData"))

ground_truth_and_interview2 %>% 
  rename(quintile = `Imagine that your country is divided into five income quintiles. Where would you place yourself and your family in terms of income?`) %>%
  mutate(`Income quintile` = case_when(quintile =="Fifth quintile (Bottom 20%)" ~ 1,
                                       quintile == "Fourth quintile" ~ 2,
                                       quintile == "Third quintile" ~ 3,
                                       quintile == "Second quintile" ~ 4,
                                       quintile == "Top quintile (Top 20%)" ~ 5,
                                       TRUE ~ NA_real_)) %>%
  filter(!is.na(`Income quintile`)) %>%
  select(`Income quintile`, Score, Interview_score, Trait, group) %>%
  group_by(Trait) %>%
  mutate(Interview_score = scale(Interview_score),
         Score = scale(Score)) %>%
  group_by(`Income quintile`, Trait) %>%
  mutate(`Interviews` = mean(Interview_score),
         `Ground truth` = mean(Score),
         N = n(),
         `Interview overestimate` = ifelse(`Interviews` > `Ground truth`, "Help", "Hurt")) %>%
  ggplot(aes(x = `Income quintile`, group = group)) +
  geom_point(aes( y = `Ground truth`, group = 1, size = N), colour = "black") +
  geom_line(aes( y = `Ground truth`, group = 1), size = 1, colour = "black") +
  facet_wrap(~Trait) +
  geom_text(data = tibble(Trait = c("Boost"), 
                          x = c(1.7), y = c(-0.2),
                          label = ordered(c("Teacher/peer\nscore"))), 
            aes(x = x, y = y, label = label,  group = 1)) +
  scale_size_area(max_size = 3) +
  guides(alpha = F, size = F) +
  labs(x = "Self-reported household income quintile within country",
       y = "Scores",
       title = "Within class, 'distance travelled' SES negatively correlates with peer/teacher scores") +
  ggthemes::theme_hc() +
  ylim(-1.5, 1.5) +
  theme(axis.text.x = element_text(angle = -90))

ground_truth_and_interview2 %>% 
  rename(quintile = `Imagine that your country is divided into five income quintiles. Where would you place yourself and your family in terms of income?`) %>%
  mutate(`Income quintile` = case_when(quintile =="Fifth quintile (Bottom 20%)" ~ 1,
                                       quintile == "Fourth quintile" ~ 2,
                                       quintile == "Third quintile" ~ 3,
                                       quintile == "Second quintile" ~ 4,
                                       quintile == "Top quintile (Top 20%)" ~ 5,
                                       TRUE ~ NA_real_)) %>%
  filter(!is.na(`Income quintile`)) %>%
  select(`Income quintile`, Score, Interview_score, Trait, group) %>%
  group_by(Trait) %>%
  mutate(Interview_score = scale(Interview_score),
         Score = scale(Score)) %>%
  group_by(`Income quintile`, Trait) %>%
  mutate(`Interviews` = mean(Interview_score),
         `Ground truth` = mean(Score),
         N = n(),
         `Interview overestimate` = ifelse(`Interviews` > `Ground truth`, "Help", "Hurt")) %>%
  ggplot(aes(x = `Income quintile`, group = group)) +
  geom_point(aes( y = Interviews, group = 1, size = N), colour = "black", alpha = 0.4) +
  geom_line(aes( y = Interviews, group = 1), size = 1, colour = "black", alpha = 0.4) +
  geom_point(aes( y = `Ground truth`, group = 1, size = N), colour = "black") +
  geom_line(aes( y = `Ground truth`, group = 1), size = 1, colour = "black") +
  scale_colour_manual(values = c("green", "red")) +
  geom_segment(aes(y = `Ground truth`, yend = Interviews,xend = `Income quintile`, colour = `Interview overestimate`), arrow = arrow(length = unit(0.03, "npc")))+
  facet_wrap(~Trait) +
  geom_text(data = tibble(Trait = c("Boost", "Boost"), 
                          x = c(1.7, 3.5), y = c(-0.2, 0.75),
                          label = ordered(c("Teacher/peer\nscore", "Interview score"))), 
            aes(x = x, y = y, label = label, alpha = rev(label), group = 1)) +
  scale_alpha_manual(values = c(1, .4)) +
  scale_size_area(max_size = 3) +
  guides(alpha = F, size = F, colour = F) +
  labs(x = "Self-reported household income quintile within country",
       y = "Scores",
       title = "Interviewers tended to underestimate poorer kids' qualities relative to teachers and peers") +
  ggthemes::theme_hc() +
  ylim(-1.5, 1.5) +
  theme(axis.text.x = element_text(angle = -90))



ground_truth_and_interview2 %>% 
  rename(Edu = `Highest level of education completed by guardian 1 (optional)`) %>%
  group_by(1:n()) %>%
  mutate(Edu = paste(strwrap(Edu, width = 30), collapse = "\n")) %>%
  group_by(group, Trait) %>% 
  mutate(Score = scale(Score)) %>%
  ungroup %>%
  ggplot(aes(x = Edu, y = Score, group = Edu)) +
  geom_hline(aes(yintercept = 0)) +
  geom_boxplot() +
  facet_wrap(~Trait) +
  labs(y = "Ground truth score",x = "Highest education of interviewee's Guardian 1?",
       title = "Interviewer overestimate by guardian education") +
  coord_flip()

library(glmnet)

ground_truth_and_interview$resid <- resid(lm(Score ~ Interview_score*Trait, data = ground_truth_and_interview))

y <- ground_truth_and_interview$resid
X <- ground_truth_and_interview %>% 
  select(immigrant_family_FALSE.:Is.English.the.language.spoken.within.your.home._Missing) %>% 
  as.matrix()

cvfit <- cv.glmnet(X, y, nfolds = 20)

nocv_fit <- glmnet(X, y)

tmp <- as.matrix(coef(cvfit, s = "lambda.1se")) %>%
  as.data.frame() %>% 
  mutate(coef = rownames(as.matrix(coef(cvfit, s = "lambda.1se")) )) %>%
  arrange(-`1`)
  
ground_truth_and_interview$Interview_score_updated <- ground_truth_and_interview$Interview_score + as.numeric(predict(cvfit, s = "lambda.1se", newx = X))

ground_truth_and_interview %>% 
  group_by(group, Trait) %>% 
  mutate(Score = scale(Score),
         Interview_score_updated = scale(Interview_score_updated)) %>%
  ggplot(aes(y = Score, x = Interview_score_updated, colour = group)) +
  geom_point() +
  facet_wrap(~Trait) +
  ggthemes::theme_hc() +
  geom_smooth(aes(group = 1), method = "lm") +
  geom_abline(aes(intercept = 0, slope = 1), alpha = 0.3) +
  labs(title = "Interview scores vs ground truth",subtitle = "With ML adjustment to interview scores", x = "Interview score", y = "Ground truth score")

ground_truth_and_interview %>% 
  group_by(group, Trait) %>% 
  mutate(Score = scale(Score),
         Interview_score = scale(Interview_score)) %>%
  ggplot(aes(y = Score, x = Interview_score, colour = group)) +
  geom_point() +
  facet_wrap(~Trait) +
  ggthemes::theme_hc() +
  geom_smooth(aes(group = 1), method = "lm") +
  geom_abline(aes(intercept = 0, slope = 1), alpha = 0.3) +
  labs(title = "Interview scores vs ground truth", 
       subtitle = "Only bias + noise adjustments, within-class scores", 
       x = "Interview score", y = "Ground truth score")


bind_cols(student_numbers,interview_scores) %>% 
  filter(student_index %in% interview_data_model$student_index) %>% 
  gather(Trait, Interview_score, -Name, -group, -student_index) %>% 
  left_join(ground_truth) %>% 
  group_by(Trait) %>% 
  mutate(Score = scale(Score),
         Interview_score = scale(Interview_score)) %>%
  ggplot(aes(y = Score, x = Interview_score, colour = group)) +
  geom_point() +
  facet_wrap(~Trait) +
  ggthemes::theme_hc() +
  geom_smooth(aes(group = 1), method = "lm") +
  geom_abline(aes(intercept = 0, slope = 1)) +
  labs(title = "Interview scores vs ground truth", x = "Interview score", y = "Ground truth score")


first_pc2 <- function(x) {
  interview <- rowSums(x %>% select(Boost:Spike) %>% as.matrix())
  
  gt <- rowSums(x %>% select(GT_Boost:GT_Spike) %>% as.matrix())
  tibble(interview = interview, ground_truth = gt)
}
  

interview_data %>%
  group_by(Interviewer) %>% 
  mutate(scaled_score = scale(`Interview Score`)) %>% 
  group_by(`Student ID`, Name, Trait) %>% 
  summarise(`Interview score` = mean(scaled_score)) %>%
  left_join(ground_truth, by = c("Student ID", "Name", "Trait")) %>% 
  group_by(group) %>% 
  mutate(`Interview score` = scale(`Interview score`)) %>% 
  ggplot(aes(y = Score, x = `Interview score`, colour = group)) +
  geom_point() +
  facet_wrap(~Trait) +
  geom_smooth(method = "lm", se = F)
    

simple_model_data <- interview_data %>%
  group_by(Interviewer) %>% 
  mutate(scaled_score = scale(`Interview Score`)) %>% 
  group_by(`Student ID`, Name, Trait) %>% 
  summarise(`Interview score` = mean(scaled_score)) %>%
  left_join(ground_truth, by = c("Student ID", "Name", "Trait")) #%>% 
  # group_by(group) %>% 
  # mutate(`Interview score` = scale(`Interview score`)) 


library(rstanarm)  

lme_fit <- stan_lmer(Score ~ `Interview score` + (1+`Interview score` | Trait) + (1  +`Interview score`| group:Trait), data = simple_model_data)
lattice::dotplot(ranef(lme_fit, condVars = T))
hist(as.data.frame(lme_fit, pars = "`Interview score`")[,1])

