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
  select(Student:`Interview-ability`, group, `Standardized Test`) %>%
  gather(Trait, `Teacher Score`, -Student:-`Student ID`, -group, -`Standardized Test`) %>% 
  group_by(Trait, group) %>% 
  mutate(`Teacher Score Raw` = `Teacher Score`,
         `Teacher Score` = scale(`Teacher Score`),
         `Standardized Test` = scale(`Standardized Test`)) %>% 
  ungroup %>% 
  group_by(Student) %>% 
  mutate(Interviewability = `Teacher Score`[Trait == "Interview-ability"], 
         Creativity = `Teacher Score`[Trait == "Creativity"]) %>% 
  rename(Name = Student)

save(teacher_ratings_long, file = paste0(Sys.Date(), "-teacher_scores.RData"))


