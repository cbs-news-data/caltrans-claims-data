library(tidyverse)
library(stringr)
library(lubridate)
library(janitor)


BAYAREA_claims_by_pothole_by_year <- caltrans_data_clean %>% 
  filter(COUNTY == "Alameda" | COUNTY == "Contra Costa" | COUNTY == "San Francisco" | COUNTY == "Santa Clara" | COUNTY == "Marin" | COUNTY == "San Mateo") %>% 
  filter(INCIDENT_YEAR == "2018" | INCIDENT_YEAR == "2019" | INCIDENT_YEAR == "2020" | INCIDENT_YEAR == "2021" | INCIDENT_YEAR == "2022" | INCIDENT_YEAR == "2023") %>% 
  group_by(INCIDENT_YEAR, claim_pothole) %>%
  summarise(count = n()) %>% 
  pivot_wider(names_from = claim_pothole, values_from = count) %>% 
  replace(is.na(.), 0) %>% 
  adorn_totals("row") %>%
  mutate(INCIDENT_YEAR = as.character(INCIDENT_YEAR)) %>% 
  mutate(total = other + pothole) %>% 
  mutate(percent_pothole = round(((pothole/total)*100),digits=1))

write.csv(BAYAREA_claims_by_pothole_by_year, "output/BAYAREA_claims_by_pothole_by_year.csv", row.names = FALSE) 


BAYAREA_claims_by_const_by_year <- caltrans_data_clean %>% 
  filter(COUNTY == "Alameda" | COUNTY == "Contra Costa" | COUNTY == "San Francisco" | COUNTY == "Santa Clara" | COUNTY == "Marin" | COUNTY == "San Mateo") %>% 
  filter(INCIDENT_YEAR == "2018" | INCIDENT_YEAR == "2019" | INCIDENT_YEAR == "2020" | INCIDENT_YEAR == "2021" | INCIDENT_YEAR == "2022" | INCIDENT_YEAR == "2023") %>% 
  group_by(INCIDENT_YEAR, claim_construction) %>%
  summarise(count = n()) %>% 
  pivot_wider(names_from = claim_construction, values_from = count) %>% 
  replace(is.na(.), 0) %>% 
  adorn_totals("row") %>%
  mutate(INCIDENT_YEAR = as.character(INCIDENT_YEAR)) %>% 
  mutate(total = other + construction) %>% 
  mutate(percent_construction = round(((construction/total)*100),digits=1))

write.csv(BAYAREA_claims_by_const_by_year, "output/BAYAREA_claims_by_const_by_year.csv", row.names = FALSE) 


# BAYAREA_claims_approve_deny_by_year_by_county <- caltrans_data_clean %>% 
#   filter(COUNTY == "Alameda" | COUNTY == "Contra Costa" | COUNTY == "Marin" | COUNTY == "San Francisco" | COUNTY == "San Mateo" | COUNTY == "Santa Clara" | COUNTY == "Sonoma" | COUNTY == "Napa") %>% 
#   filter(INCIDENT_YEAR == "2018" | INCIDENT_YEAR == "2019" | INCIDENT_YEAR == "2020" | INCIDENT_YEAR == "2021" | INCIDENT_YEAR == "2022" | INCIDENT_YEAR == "2023") %>% 
#   group_by(COUNTY, `STATUS CLEAN`, INCIDENT_YEAR) %>%
#   summarise(count = n()) %>% 
#   pivot_wider(names_from = `STATUS CLEAN`, values_from = count) %>% 
#   replace(is.na(.), 0) %>% 
#   adorn_totals("row") %>%
#   mutate(INCIDENT_YEAR = as.character(INCIDENT_YEAR)) %>% 
#   mutate(total = Approve + Deny  + `Small Claims Court` + `unclear/unknown/pending`) %>% 
#   mutate(total_not_unknown_pending = Approve + Deny + `Small Claims Court`) %>%
#   mutate(percent_approved_of_total = round(((Approve/total)*100),digits=1)) %>%
#   mutate(percent_approved_of_not_unk_pending = round(((Approve/total_not_unknown_pending)*100),digits=1))




BAYAREA_claims_approve_deny_by_year <- caltrans_data_clean %>% 
  filter(COUNTY == "Alameda" | COUNTY == "Contra Costa" | COUNTY == "San Francisco" | COUNTY == "Santa Clara" | COUNTY == "Marin" | COUNTY == "San Mateo") %>% 
  filter(INCIDENT_YEAR == "2018" | INCIDENT_YEAR == "2019" | INCIDENT_YEAR == "2020" | INCIDENT_YEAR == "2021" | INCIDENT_YEAR == "2022" | INCIDENT_YEAR == "2023") %>% 
  group_by(`STATUS CLEAN`, INCIDENT_YEAR) %>%
  summarise(count = n()) %>% 
  pivot_wider(names_from = `STATUS CLEAN`, values_from = count) %>% 
  replace(is.na(.), 0) %>% 
  adorn_totals("row") %>%
  mutate(INCIDENT_YEAR = as.character(INCIDENT_YEAR)) %>% 
  mutate(total = Approve + Deny  + `Small Claims Court` + `unclear/unknown/pending`) %>% 
  mutate(total_not_unknown_pending = Approve + Deny + `Small Claims Court`) %>%
  mutate(percent_approved_of_total = round(((Approve/total)*100),digits=1)) %>%
  mutate(percent_approved_of_not_unk_pending = round(((Approve/total_not_unknown_pending)*100),digits=1))

write.csv(BAYAREA_claims_approve_deny_by_year, "output/BAYAREA_claims_approve_deny_by_year.csv", row.names = FALSE)


bayarea_dollar_amount_by_year <- caltrans_data_clean %>% 
  filter(COUNTY == "Alameda" | COUNTY == "Contra Costa" | COUNTY == "San Francisco" | COUNTY == "Santa Clara" | COUNTY == "Marin" | COUNTY == "San Mateo") %>% 
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


bayarea_by_route <- caltrans_data_clean %>% 
  filter(COUNTY == "Alameda" | COUNTY == "Contra Costa" | COUNTY == "San Francisco" | COUNTY == "Santa Clara" | COUNTY == "Marin" | COUNTY == "San Mateo") %>% 
  filter(INCIDENT_YEAR == "2018" | INCIDENT_YEAR == "2019" | INCIDENT_YEAR == "2020" | INCIDENT_YEAR == "2021" | INCIDENT_YEAR == "2022" | INCIDENT_YEAR == "2023") %>%
  group_by(`STATUS CLEAN`, `ROUTE CLEAN`) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = `STATUS CLEAN`, values_from = count) %>% 
  mutate(Approve = ifelse(is.na(Approve), 0, Approve),
         Deny = ifelse(is.na(Deny), 0, Deny),
         `Small Claims Court` = ifelse(is.na(`Small Claims Court`), 0, `Small Claims Court`),
         `unclear/unknown/pending` = ifelse(is.na(`unclear/unknown/pending`), 0, `unclear/unknown/pending`)
  ) %>% 
  mutate(total = Approve + Deny + `Small Claims Court` + `unclear/unknown/pending`) %>% 
  mutate(total_not_unknown_pending = Approve + Deny + `Small Claims Court`)

bayarea_by_route_potholes <- caltrans_data_clean %>% 
  filter(COUNTY == "Alameda" | COUNTY == "Contra Costa" | COUNTY == "San Francisco" | COUNTY == "Santa Clara" | COUNTY == "Marin" | COUNTY == "San Mateo") %>% 
  filter(INCIDENT_YEAR == "2018" | INCIDENT_YEAR == "2019" | INCIDENT_YEAR == "2020" | INCIDENT_YEAR == "2021" | INCIDENT_YEAR == "2022" | INCIDENT_YEAR == "2023") %>%
  group_by(claim_pothole, `ROUTE CLEAN`) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = claim_pothole, values_from = count) %>% 
  mutate(total = other + pothole) %>% 
  mutate(pct_pothole = round(((pothole/total)*100), digits=0))


bayarea_top_route <- caltrans_data_clean %>%
  filter(COUNTY == "Alameda" | COUNTY == "Contra Costa" | COUNTY == "San Francisco" | COUNTY == "Santa Clara" | COUNTY == "Marin" | COUNTY == "San Mateo") %>% 
  filter(INCIDENT_YEAR == "2018" | INCIDENT_YEAR == "2019" | INCIDENT_YEAR == "2020" | INCIDENT_YEAR == "2021" | INCIDENT_YEAR == "2022" | INCIDENT_YEAR == "2023") %>%
  filter(ROUTE == "101") %>% 
  mutate(POST_MILE_CLEAN = `POST MILE`) %>% 
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "\\+\\/\\-", "")) %>% 
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "SM ", "")) %>% 
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "SF ", "")) %>% 
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "SCL ", "")) %>% 
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "T", "")) %>% 
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "101 ", "")) %>% 
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "SB ", "")) %>% 
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "SB", "")) %>% 
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "S", "")) %>% 
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "NB ", "")) %>% 
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "R ", "")) %>% 
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "R", "")) %>% 
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "M", "")) %>% 
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "L", "")) %>% 
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "In line with ", "")) %>% 
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "E", "")) %>% 
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "425", "")) %>%
  mutate(POST_MILE_CLEAN = str_replace(POST_MILE_CLEAN, "1809", "")) %>%
  mutate(POST_MILE_CLEAN = str_extract(POST_MILE_CLEAN, "[^-]+")) %>% 
  mutate(POST_MILE_CLEAN = as.numeric(POST_MILE_CLEAN)) %>% 
  mutate(bins = case_when(POST_MILE_CLEAN <= 10 ~ "0-10",
                          ((POST_MILE_CLEAN > 10) & (POST_MILE_CLEAN <= 20)) ~ "10-20",
                          ((POST_MILE_CLEAN > 20) & (POST_MILE_CLEAN <= 30)) ~ "20-30",
                          ((POST_MILE_CLEAN > 30) & (POST_MILE_CLEAN <= 40)) ~ "30-40",
                          ((POST_MILE_CLEAN > 40) & (POST_MILE_CLEAN <= 50)) ~ "40-50",
                          ((POST_MILE_CLEAN > 50) & (POST_MILE_CLEAN <= 60)) ~ "50-60",
                          ((POST_MILE_CLEAN > 60) & (POST_MILE_CLEAN <= 70)) ~ "60-70",
                          ((POST_MILE_CLEAN > 70) & (POST_MILE_CLEAN <= 80)) ~ "70-80",
                          ((POST_MILE_CLEAN > 80) & (POST_MILE_CLEAN <= 90)) ~ "80-90",
                          TRUE ~ "NA"
  )) %>% mutate(bins_county = paste(sep="", bins, "-", COUNTY)) %>% 
  group_by(bins_county) %>%
  summarize(count = n())


bayarea_only <- caltrans_data_clean %>% 
  filter(COUNTY == "Alameda" | COUNTY == "Contra Costa" | COUNTY == "San Francisco" | COUNTY == "Santa Clara" | COUNTY == "Marin" | COUNTY == "San Mateo")

#CHECK ROUTES
bayarea_caltrans_claims_na <- caltrans_data_clean %>% 
  filter(COUNTY == "Alameda" | COUNTY == "Contra Costa" | COUNTY == "San Francisco" | COUNTY == "Santa Clara" | COUNTY == "Marin" | COUNTY == "San Mateo") %>% 
  filter(INCIDENT_YEAR == "2018" | INCIDENT_YEAR == "2019" | INCIDENT_YEAR == "2020" | INCIDENT_YEAR == "2021" | INCIDENT_YEAR == "2022" | INCIDENT_YEAR == "2023") %>% 
  filter(is.na(`ROUTE CLEAN`)) %>% 
  filter(!is.na(`INCIDENT LOCATION DETAILS`))

#write.csv(bayarea_caltrans_claims_na, "bayarea_caltrans_claims_na.csv", row.names = FALSE)
