library(randomForest); library(tidyverse); library(googlesheets4); library(yaml); library(recipes)
creds <- yaml::read_yaml("creds.yaml")


googlesheets4::gs4_auth(email = creds$email)

demographics <- read_sheet(ss = creds$demographics$url, sheet = creds$demographics$sheet) %>% 
  mutate(`Student ID` = tolower(`Student ID`))

tracker_data_raw <- read_sheet(ss = creds$tracker$url, sheet = creds$tracker$tracking_sheet, skip = 1, col_types = "cnccccnlllllllnlllc") %>% 
  mutate(`Student ID` = tolower(`Student ID`))

peer_reviewers <- read_sheet(ss = creds$peer_review$url, sheet = creds$peer_review$sheet) %>%
  rename(`Student ID` = Recipient) %>% 
  mutate(Name = ifelse(is.na(`Last name`), `First name`, paste(`First name`, `Last name`)))

rhodies <- read_sheet(ss = creds$interview_sheet$url, sheet = creds$interview_sheet$responses_sheet)

interviewers <- rhodies$`Interviewer Name (you, the interviewer)`

demographics_2 <- demographics %>% 
  mutate(`Date of birth` = as.Date(`Date of birth`)) %>% 
  mutate(demographics_id = 1:n()) %>%
  rename(g1_ed = `Highest level of education completed by guardian 1 (optional)`,
         g2_ed = `Highest level of education completed by guardian 2 (optional)`,
         quintile = `Imagine that your country is divided into five income quintiles. Where would you place yourself and your family in terms of income?`) %>%
  transmute(demographics_id = demographics_id, 
            Name = `Name (First then last)`,
            `Student ID` = `Student ID`,
            ESL = ifelse(is.na(`If not, what language is spoken at home?`), "No", "Yes"),
            Age = as.numeric((Sys.Date() - `Date of birth`)/365.25),
            Alternate_email = `Alternate Email`,
            Gender = case_when(Gender=="Female" ~"Female", 
                               Gender=="Male" ~ "Male",
                               TRUE ~ "Other"),
            English_speaking_household = `Is English the language spoken within your home?`,
            Education_country = `In what country have you had most of your education?`,
            Scholarship = ifelse(is.na(`Do you receive need-based scholarships or similar funding to pay for school?`),"Unanswered", `Do you receive need-based scholarships or similar funding to pay for school?`),
            School_type = ifelse(is.na(`What kind of school do you attend?`),"Unanswered", `What kind of school do you attend?`),
            Guardian_one_education = case_when(is.na(g1_ed) ~ "-9. Prefer not to say",
                                               g1_ed == "Graduate Degree (PhD, MD, JD...)" ~ "4. Grad school",
                                               g1_ed == "Bachelor’s Degree (finished university)" ~ "3. Bachelor",
                                               g1_ed %in% c("Not been to school", 
                                                            "They terminated school at or before age thirteen", 
                                                            "They terminated school before age eighteen") ~ "0. No high school",
                                               g1_ed == "They completed high school, or their primary education" ~ "1. High school",
                                               TRUE ~ "2. Some post-school education"),
            Guardian_two_education = case_when(is.na(g2_ed) ~ "-9. Prefer not to say",
                                               g2_ed == "Graduate Degree (PhD, MD, JD...)" ~ "4. Grad school",
                                               g2_ed == "Bachelor’s Degree (finished university)" ~ "3. Bachelor",
                                               g2_ed %in% c("Not been to school", 
                                                            "They terminated school at or before age thirteen", 
                                                            "They terminated school before age eighteen") ~ "0. No high school",
                                               g2_ed == "They completed high school, or their primary education" ~ "1. High school",
                                               TRUE ~ "2. Some post-school education"),
            Family_income_quintile = case_when(is.na(quintile) ~ "Prefer not to say", 
                                 TRUE ~ quintile),
            Mobile_plan_type = case_when(is.na(`If you have a mobile phone, is your account pre-paid or post-paid?`) ~ "Unanswered",
                                         TRUE ~ `If you have a mobile phone, is your account pre-paid or post-paid?`),
            Paid_subscriptions = case_when(is.na(`Please check all of the following to which you have paid subscriptions.`) ~ "Unanswered",
                                           TRUE ~ `Please check all of the following to which you have paid subscriptions.`)
            
            ) %>% 
  filter(!duplicated(Name)) %>%
  mutate(Paid_subscriptions = str_split(Paid_subscriptions, ", ")) %>% 
  unnest(cols = "Paid_subscriptions") %>% 
  group_by(demographics_id) %>% 
  mutate(Paid_subscriptions = n()) %>%
  slice(1L) %>%
  ungroup %>%
  mutate(Type = case_when(Name %in% interviewers ~ "Interviewer",
                          tolower(`Student ID`) %in% tolower(peer_reviewers$`Student ID`) | Name %in% peer_reviewers$Name ~ "Peer reviewer",
                          tolower(`Student ID`) %in% tolower(tracker_data_raw$`Student ID`) ~ "Study participant", 
                          TRUE ~ "Other")) %>% 
  ungroup 
  
names(demographics_2) <- make.names(names(demographics_2))

rf_fit <- randomForest(~ ., data = demographics_2 %>% select(-Name, -`Student.ID`, -Alternate_email, -demographics_id) %>% mutate_if(is.character, as.factor), proximity = T, trees = 5000, oob.prox = TRUE)

proximity_lookup <- bind_cols(demographics_id = demographics_2$demographics_id, as.data.frame(rf_fit$proximity)) %>% 
  gather(column, proximity, -demographics_id) %>% 
  mutate(reviewee_demographics_id = rep(demographics_2$demographics_id,each = max(parse_number(column)))) %>% 
  filter(demographics_id != reviewee_demographics_id)


save(proximity_lookup, demographics_2, file = paste0(Sys.Date(), "-demographics-and-proximity.RData"))
