####### First attempt at project in customer insights #########

setwd("~/Molekylær biomedicin/NUS/DSA2361/Data")

library(tidyverse)

questionnaire <- read.csv('questionnaire.csv')

#### questions with h are all about health insurance: 

questionnaire_only_insurance <- questionnaire %>% 
  column_to_rownames(var = "SEQN") %>% 
  select(starts_with("HI")) %>% 
  rename("Are you covered by health Insurance" = HIQ011,
         "Are you covered by private insurance" = HIQ031A,
         "No coverage of any type" = HIQ031AA,
         "Are you covered by medicaid" = HIQ031B, 
         "Are you covered by Medi-Gap" = HIQ031C, 
         "Are you covered by Medicaid" = HIQ031D,
         "Are you covered by State Children's Health Insurance Program" = HIQ031E, 
         "Are you covered by military health plan" = HIQ031F, 
         "Are you covered by indian health service?" = HIQ031G, 
         "Are you covered state-sponsored health plan" = HIQ031H, 
         "Are you covered by other government insurance" = HIQ031I,
         "Are you covered by any single service plan" = HIQ031J,
         "Insurance card available" = HIQ105,
         "In the past 12 months was there any time without health insurance" = HIQ210,
         "Does any of these plans pay prescription" = HIQ270)

shorter_names <- questionnaire %>% 
  column_to_rownames(var = "SEQN") %>% 
  select(starts_with("HI")) %>% 
  rename("health_Insurance" = HIQ011,
         "private_insurance" = HIQ031A,
         "No_coverage" = HIQ031AA,
         "medicaid" = HIQ031B, 
         "Medi-Gap" = HIQ031C, 
         "Medicaid" = HIQ031D,
         "State_Children's_Health_Insurance_Program" = HIQ031E, 
         "military_health_plan" = HIQ031F, 
         "indian_health_service?" = HIQ031G, 
         "state-sponsored_health_plan" = HIQ031H, 
         "government_insurance" = HIQ031I,
         "single_service_plan" = HIQ031J,
         "Insurance_card_available" = HIQ105,
         "past_12_months_without_health_insurance" = HIQ210,
         "pay_prescription" = HIQ270)

questionnaire_only_insurance[1,]  
