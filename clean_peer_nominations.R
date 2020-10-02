library(tidyverse); library(googlesheets4); library(yaml)
creds <- yaml::read_yaml("creds.yaml")

googlesheets4::gs4_auth(email = creds$email)


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

peer_noms_raw <- peer_noms_raw %>% 
  mutate(Name = ifelse(Name==creds$names_to_replace$name_1$wrong_name, creds$names_to_replace$name_1$correct_name, Name))

save(peer_noms_raw, file = paste0(Sys.Date(), "-peer_nominations_clean.RData"))

