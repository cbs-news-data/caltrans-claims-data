library(tidyverse)
library(stringr)
library(lubridate)
library(janitor)

by_status <- caltrans_data_clean %>% 
  group_by(`STATUS CLEAN`) %>% 
  summarise(count = n()) %>% 
  mutate(total = sum(count)) %>% 
  mutate(percent_of_total = round((((count/total)*100)),digits=1))

by_status_pothole <- caltrans_data_clean %>% 
  group_by(`STATUS CLEAN`, claim_pothole) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = `STATUS CLEAN`, values_from = count) %>%
  mutate(`Transfer to diff office` = replace_na(`Transfer to diff office`, 0)) %>% 
  adorn_totals("row") %>% 
  mutate(total = Approve + Deny + `Deny-NSH` + `Small Claims Court` + Tendered + `Transfer (unclear)` + `Transfer to diff office` + other + `unclear/unknown/pending`) %>% 
  mutate(total_not_unknown_pending = Approve + Deny + `Deny-NSH`+ `Small Claims Court` + Tendered + `Transfer (unclear)` + `Transfer to diff office` + other) %>% 
  mutate(percent_approved_of_total = round(((Approve/total)*100),digits=1)) %>% 
  mutate(percent_approved_of_not_unk_pending = round(((Approve/total_not_unknown_pending)*100),digits=1)) 



all_claims_by_year <- caltrans_data_clean %>% 
  filter(INCIDENT_YEAR == "2018" | INCIDENT_YEAR == "2019" | INCIDENT_YEAR == "2020" | INCIDENT_YEAR == "2021" | INCIDENT_YEAR == "2022" |INCIDENT_YEAR == "2023" | INCIDENT_YEAR == "Total") %>% 
  group_by(`STATUS CLEAN`, INCIDENT_YEAR) %>%
  summarise(count = n()) %>% 
  pivot_wider(names_from = `STATUS CLEAN`, values_from = count) %>% 
  replace(is.na(.), 0) %>% 
  adorn_totals("row") %>%
  mutate(INCIDENT_YEAR = as.character(INCIDENT_YEAR)) %>% 
  mutate(total = Approve + Deny + `Deny-NSH` + `Small Claims Court` + Tendered + `Transfer (unclear)` + `Transfer to diff office` + other + `unclear/unknown/pending`) %>% 
  mutate(total_not_unknown_pending = Approve + Deny + `Deny-NSH`+ `Small Claims Court` + Tendered + `Transfer (unclear)` + `Transfer to diff office` + other) %>%
  mutate(percent_approved_of_total = round(((Approve/total)*100),digits=1)) %>%
  mutate(percent_approved_of_not_unk_pending = round(((Approve/total_not_unknown_pending)*100),digits=1))

write.csv(all_claims_by_year, "output/all_claims_by_year.csv", row.names = FALSE)  


all_claims_by_district <- caltrans_data_clean %>% 
  filter(INCIDENT_YEAR == "2018" | INCIDENT_YEAR == "2019" | INCIDENT_YEAR == "2020" | INCIDENT_YEAR == "2021" | INCIDENT_YEAR == "2022" |INCIDENT_YEAR == "2023" | INCIDENT_YEAR == "Total") %>% 
  group_by(`STATUS CLEAN`, DISTRICT) %>%
  summarise(count = n()) %>% 
  pivot_wider(names_from = `STATUS CLEAN`, values_from = count) %>% 
  replace(is.na(.), 0) %>% 
  mutate(DISTRICT = as.character(DISTRICT)) %>% 
  mutate(DISTRICT = case_when(DISTRICT == "0" ~ "NA",
                              TRUE ~ DISTRICT)) %>% 
  adorn_totals("row") %>%
  mutate(total = Approve + Deny + `Deny-NSH` + `Small Claims Court` + Tendered + `Transfer (unclear)` + `Transfer to diff office` + other + `unclear/unknown/pending`) %>% 
  mutate(total_not_unknown_pending = Approve + Deny + `Deny-NSH`+ `Small Claims Court` + Tendered + `Transfer (unclear)` + `Transfer to diff office` + other) %>%
  mutate(percent_approved_of_total = round(((Approve/total)*100),digits=1)) %>%
  mutate(percent_approved_of_not_unk_pending = round(((Approve/total_not_unknown_pending)*100),digits=1))

all_claims_by_district_pops <- merge(all_claims_by_district, caltrans_dist_pops, by.x="DISTRICT", by.y = "District") %>% 
  mutate(total_claims_per100k = (total/Total_Population)*100000)

write.csv(all_claims_by_district_pops, "output/all_claims_by_district_pops.csv", row.names = FALSE)


all_claims_by_county <- caltrans_data_clean %>% 
  filter(INCIDENT_YEAR == "2018" | INCIDENT_YEAR == "2019" | INCIDENT_YEAR == "2020" | INCIDENT_YEAR == "2021" | INCIDENT_YEAR == "2022" |INCIDENT_YEAR == "2023" | INCIDENT_YEAR == "Total") %>% 
  group_by(INCIDENT_YEAR, COUNTY) %>%
  summarise(count = n()) %>% 
  filter(COUNTY != "?") %>% 
  filter(COUNTY != "405") %>% 
  filter(COUNTY != "57") %>% 
  filter(COUNTY != "73") %>% 
  filter(COUNTY != "NSH") %>% 
  pivot_wider(names_from = COUNTY, values_from = count) %>% 
  replace(is.na(.), 0)
  
write.csv(all_claims_by_county, "output/all_claims_by_county.csv", row.names = FALSE)

all_claims_by_county_approve <- caltrans_data_clean %>% 
  filter(INCIDENT_YEAR == "2018" | INCIDENT_YEAR == "2019" | INCIDENT_YEAR == "2020" | INCIDENT_YEAR == "2021" | INCIDENT_YEAR == "2022" |INCIDENT_YEAR == "2023" | INCIDENT_YEAR == "Total") %>% 
  group_by(`STATUS CLEAN`, COUNTY) %>%
  summarise(count = n()) %>% 
  filter(COUNTY != "?") %>% 
  filter(COUNTY != "405") %>% 
  filter(COUNTY != "57") %>% 
  filter(COUNTY != "73") %>% 
  filter(COUNTY != "NSH") %>%
  pivot_wider(names_from = `STATUS CLEAN`, values_from = count) %>% 
  replace(is.na(.), 0) %>% 
  mutate(total = Approve + Deny + `Deny-NSH` + `Small Claims Court` + Tendered + `Transfer (unclear)` + `Transfer to diff office` + other + `unclear/unknown/pending`) %>% 
  mutate(total_not_unknown_pending = Approve + Deny + `Deny-NSH`+ `Small Claims Court` + Tendered + `Transfer (unclear)` + `Transfer to diff office` + other) %>%
  mutate(percent_approved_of_total = round(((Approve/total)*100),digits=1)) %>%
  mutate(percent_approved_of_not_unk_pending = round(((Approve/total_not_unknown_pending)*100),digits=1)) %>% 
  select(COUNTY, Approve, total, total_not_unknown_pending, percent_approved_of_not_unk_pending) %>% 
  rename(County = COUNTY, 
         Approved = Approve, 
         `Total claims` = total, 
         `Total w/ known status` = total_not_unknown_pending,
         `% Approved` = percent_approved_of_not_unk_pending)

write.csv(all_claims_by_county_approve, "output/all_claims_by_county_approve.csv", row.names = FALSE)

pothole_claims_by_year <- caltrans_data_clean %>% 
  filter(INCIDENT_YEAR == "2018" | INCIDENT_YEAR == "2019" | INCIDENT_YEAR == "2020" | INCIDENT_YEAR == "2021" | INCIDENT_YEAR == "2022" |INCIDENT_YEAR == "2023") %>% 
  filter(claim_pothole == 'pothole') %>% 
  group_by(`STATUS CLEAN`, INCIDENT_YEAR) %>%
  summarise(count = n()) %>% 
  pivot_wider(names_from = `STATUS CLEAN`, values_from = count) %>% 
  replace(is.na(.), 0) %>% 
  adorn_totals("row") %>%
  mutate(INCIDENT_YEAR = as.character(INCIDENT_YEAR)) %>% 
  mutate(total = Approve + Deny + `Deny-NSH` + `Small Claims Court` + Tendered + `Transfer (unclear)` + other + `unclear/unknown/pending`) %>% 
  mutate(total_not_unknown_pending = Approve + Deny + `Deny-NSH`+ `Small Claims Court` + Tendered + `Transfer (unclear)` + other) %>%
  mutate(percent_approved_of_total = round(((Approve/total)*100),digits=1)) %>%
  mutate(percent_approved_of_not_unk_pending = round(((Approve/total_not_unknown_pending)*100),digits=1)) 

write.csv(pothole_claims_by_year, "output/pothole_claims_by_year.csv", row.names = FALSE)  


claims_by_pothole_by_year <- caltrans_data_clean %>% 
  filter(INCIDENT_YEAR == "2018" | INCIDENT_YEAR == "2019" | INCIDENT_YEAR == "2020" | INCIDENT_YEAR == "2021" | INCIDENT_YEAR == "2022" | INCIDENT_YEAR == "2023") %>% 
  group_by(INCIDENT_YEAR, claim_pothole) %>%
  summarise(count = n()) %>% 
  pivot_wider(names_from = claim_pothole, values_from = count) %>% 
  replace(is.na(.), 0) %>% 
  adorn_totals("row") %>%
  mutate(INCIDENT_YEAR = as.character(INCIDENT_YEAR)) %>% 
  mutate(total = other + pothole) %>% 
  mutate(percent_pothole = round(((pothole/total)*100),digits=1)) 

write.csv(claims_by_pothole_by_year, "output/claims_by_pothole_by_year.csv", row.names = FALSE)  


#dollar amount- exluding weird $$$ ones and excluding ones that do not have status info

dollar_amount_by_year <- caltrans_data_clean %>% 
  filter(is_confusing == "no") %>% 
  filter(`STATUS CLEAN` != "unclear/unknown/pending") %>% 
  filter(INCIDENT_YEAR == "2018" | INCIDENT_YEAR == "2019" | INCIDENT_YEAR == "2020" | INCIDENT_YEAR == "2021" | INCIDENT_YEAR == "2022" | INCIDENT_YEAR == "2023") %>%
  mutate(`AMOUNT SOUGHT CLEAN` = ifelse(is.na(`AMOUNT SOUGHT CLEAN`), 0, `AMOUNT SOUGHT CLEAN`)) %>% 
  mutate(`AMOUNT PAID CLEAN` = ifelse(is.na(`AMOUNT PAID CLEAN`), 0, `AMOUNT PAID CLEAN`)) %>% 
  group_by(INCIDENT_YEAR) %>% 
  summarise(TOTAL_AMOUNT_SOUGHT = sum(`AMOUNT SOUGHT CLEAN`),
            TOTAL_AMOUNT_PAID = sum(`AMOUNT PAID CLEAN`)) %>% 
  adorn_totals("row") %>% 
  mutate(pct_paid = round(((TOTAL_AMOUNT_PAID/TOTAL_AMOUNT_SOUGHT)*100), digits=1))



statewide_by_route <- caltrans_data_clean %>% 
  filter(INCIDENT_YEAR == "2018" | INCIDENT_YEAR == "2019" | INCIDENT_YEAR == "2020" | INCIDENT_YEAR == "2021" | INCIDENT_YEAR == "2022" | INCIDENT_YEAR == "2023") %>%
  group_by(`STATUS CLEAN`, `ROUTE CLEAN`) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = `STATUS CLEAN`, values_from = count) %>% 
  mutate(Approve = ifelse(is.na(Approve), 0, Approve),
         Deny = ifelse(is.na(Deny), 0, Deny),
         `Deny-NSH` = ifelse(is.na(`Deny-NSH`), 0, `Deny-NSH`),
         `Small Claims Court` = ifelse(is.na(`Small Claims Court`), 0, `Small Claims Court`),
         Tendered = ifelse(is.na(Tendered), 0, Tendered),
         `Transfer (unclear)` = ifelse(is.na(`Transfer (unclear)`), 0, `Transfer (unclear)`),
         other = ifelse(is.na(other), 0, other),
         `unclear/unknown/pending` = ifelse(is.na(`unclear/unknown/pending`), 0, `unclear/unknown/pending`)
         ) %>% 
  mutate(total = Approve + Deny + `Deny-NSH` + `Small Claims Court` + Tendered + `Transfer (unclear)` + other + `unclear/unknown/pending`) %>% 
  mutate(total_not_unknown_pending = Approve + Deny + `Deny-NSH`+ `Small Claims Court` + Tendered + `Transfer (unclear)` + other)
  