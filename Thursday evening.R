####### First attempt at project in customer insights #########

setwd("~/DSA2361_presentation/DSA2361_presentation/Data")

library(tidyverse)
library(lattice)
library(cluster)
library(ggplot2)
set.seed(2019)

######### IMPORTING THE DATA:

questionnaire <- read.csv('questionnaire.csv')
diet <- read.csv("diet.csv")
demographics <- read.csv("demographic.csv")
medication = read.csv("medications.csv")


####### ADAPTING THE DATA FRAMES: 

#### Adapting the health insurance: 

medication$Druguse <- medication$RXDDAYS * medication$RXDCOUNT

medication <- filter(medication, RXDDAYS!=99999) #Removed those who did not know how long they had been using the medication

medication <- transform(medication, Druguse=ave(Druguse, SEQN, FUN=sum))

medication <- medication[!duplicated(medication$SEQN),]

medication <- medication %>% mutate("ID" = SEQN) %>% select("ID", "Druguse")

#### questions with h are all about health insurance: 

# First is a full length dataframe - just so we can always find out what the original question was

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

demographics_new_names <- demographics %>% 
  rename("Age" = RIDAGEYR,
         "Ethnicity" = RIDRETH3, 
         "Time_in_US" = DMDYRSUS,
         "Education" = DMDEDUC3, 
         "Gender" = RIAGENDR,
         "Ratio_to_poverty" = INDFMPIR,
         "Income_range" = INDFMIN2,
         "Family_size" = DMDFMSIZ,
         "Children" = DMDHHSZA,
         "ID" = SEQN) %>% 
  select("Age", "Ethnicity","Time_in_US","Education","Gender","Ratio_to_poverty","Income_range",
         "Family_size", "Children","ID")

demographics_new_names$Ethnicity[demographics_new_names$Ethnicity == 1] <- "Mexican_american"
demographics_new_names$Ethnicity[demographics_new_names$Ethnicity == 2] <- "Other_hispanic"
demographics_new_names$Ethnicity[demographics_new_names$Ethnicity == 3] <- "Non_hispanic_white"
demographics_new_names$Ethnicity[demographics_new_names$Ethnicity == 4] <- "Non_hispanic_black"
demographics_new_names$Ethnicity[demographics_new_names$Ethnicity == 6] <- "Asian"
demographics_new_names$Ethnicity[demographics_new_names$Ethnicity == 7] <- "Multiracial"

demographics_new_names$Gender[demographics_new_names$Gender == 1] <- "Male"
demographics_new_names$Gender[demographics_new_names$Gender == 2] <- "Female"


# After the full length array a shorter named list is made: 

shorter_names <- questionnaire %>% 
  rename("health_Insurance" = HIQ011,
         "private_insurance" = HIQ031A,
         "No_coverage" = HIQ031AA,
         "medicare" = HIQ031B, 
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
         "pay_prescription" = HIQ270,
         "ID" = SEQN) %>% 
  select("health_Insurance", "private_insurance", "No_coverage", "medicare", "Medi-Gap", "Medicaid",
         "State_Children's_Health_Insurance_Program", "military_health_plan", "indian_health_service?",
         "state-sponsored_health_plan", "government_insurance", "single_service_plan", "Insurance_card_available",
         "past_12_months_without_health_insurance", "pay_prescription", "ID")

# Changing the binary format to string

shorter_names[,2][shorter_names[,2] == 1] <- "yes"  
shorter_names[,2][shorter_names[,2] == 2] <- "no"

######## Generating bad health patients: 

Carbs = select(diet, SEQN, DR1TCARB) %>% filter(DR1TCARB >=0) %>% filter(DR1TCARB>quantile(DR1TCARB, 0.95) | DR1TCARB<quantile(DR1TCARB, 0.05)) %>% select(SEQN)  
Cholest = select(diet, SEQN, DR1TCHOL) %>% filter(DR1TCHOL >=0) %>% filter(DR1TCHOL>quantile(DR1TCHOL, 0.95) | DR1TCHOL<quantile(DR1TCHOL, 0.05)) %>% select(SEQN)  
Protein = select(diet, SEQN, DR1TPROT) %>% filter(DR1TPROT >=0) %>% filter(DR1TPROT>quantile(DR1TPROT, 0.95) | DR1TPROT<quantile(DR1TPROT, 0.05)) %>% select(SEQN)  
KCal = select(diet, SEQN, DR1TKCAL) %>% filter(DR1TKCAL >=0) %>% filter(DR1TKCAL>quantile(DR1TKCAL, 0.95) | DR1TKCAL<quantile(DR1TKCAL, 0.05)) %>% select(SEQN)  
UnsFat = select(diet, SEQN, DR1TMFAT) %>% filter(DR1TMFAT >=0) %>% filter(DR1TMFAT>quantile(DR1TMFAT, 0.95) | DR1TMFAT<quantile(DR1TMFAT, 0.05)) %>% select(SEQN)  
SatFat = select(diet, SEQN, DR1TSFAT) %>% filter(DR1TSFAT >=0) %>% filter(DR1TSFAT>quantile(DR1TSFAT, 0.95) | DR1TSFAT<quantile(DR1TSFAT, 0.05)) %>% select(SEQN)  

Sugar = select(diet, SEQN, DR1TSUGR) %>% filter(DR1TSUGR >=0) %>% filter(DR1TSUGR>quantile(DR1TSUGR, 0.95) | DR1TSUGR<quantile(DR1TSUGR, 0.05)) %>% select(SEQN)  
Fat = select(diet, SEQN, DR1TTFAT) %>% filter(DR1TTFAT >=0) %>% filter(DR1TTFAT>quantile(DR1TTFAT, 0.95) | DR1TTFAT<quantile(DR1TTFAT, 0.05)) %>% select(SEQN)  


Cholin = select(diet, SEQN, DR1TCHL) %>% filter(DR1TCHL >=0) %>% filter(DR1TCHL>quantile(DR1TCHL, 0.95) | DR1TCHL<quantile(DR1TCHL, 0.05)) %>% select(SEQN)  
VitE = select(diet, SEQN, DR1TATOC) %>% filter(DR1TATOC >=0) %>% filter(DR1TATOC>quantile(DR1TATOC, 0.95) | DR1TATOC<quantile(DR1TATOC, 0.05)) %>% select(SEQN)  
VitA = select(diet, SEQN, DR1TVARA) %>% filter(DR1TVARA >=0) %>% filter(DR1TVARA>quantile(DR1TVARA, 0.95) | DR1TVARA<quantile(DR1TVARA, 0.05)) %>% select(SEQN)  
VitB12 = select(diet, SEQN, DR1TVB12) %>% filter(DR1TVB12 >=0) %>% filter(DR1TVB12>quantile(DR1TVB12, 0.95) | DR1TVB12<quantile(DR1TVB12, 0.05)) %>% select(SEQN)  
VitC = select(diet, SEQN, DR1TVC) %>% filter(DR1TVC >=0) %>% filter(DR1TVC>quantile(DR1TVC, 0.95) | DR1TVC<quantile(DR1TVC, 0.05)) %>% select(SEQN)  
VitD = select(diet, SEQN, DR1TVD) %>% filter(DR1TVD >=0) %>% filter(DR1TVD>quantile(DR1TVD, 0.95) | DR1TVD<quantile(DR1TVD, 0.05)) %>% select(SEQN)  
VitK = select(diet, SEQN, DR1TVK) %>% filter(DR1TVK >=0) %>% filter(DR1TVK>quantile(DR1TVK, 0.95) | DR1TVK<quantile(DR1TVK, 0.05)) %>% select(SEQN)  


Folate = select(diet, SEQN, DR1TFA) %>% filter(DR1TFA >=0) %>% filter(DR1TFA>quantile(DR1TFA, 0.95) | DR1TFA<quantile(DR1TFA, 0.05)) %>% select(SEQN)  
Fibre = select(diet, SEQN, DR1TFIBE) %>% filter(DR1TFIBE >=0) %>% filter(DR1TFIBE>quantile(DR1TFIBE, 0.95) | DR1TFIBE<quantile(DR1TFIBE, 0.05)) %>% select(SEQN)   
Retinol = select(diet, SEQN, DR1TRET) %>% filter(DR1TRET >=0) %>% filter(DR1TRET>quantile(DR1TRET, 0.95) | DR1TRET<quantile(DR1TRET, 0.05)) %>% select(SEQN)  

health = rbind(Carbs, Cholest, Protein, KCal, UnsFat, 
               SatFat, Sugar, Fat, Cholin, VitE, VitA, VitB12,
               VitC, VitD, VitK, Folate, Fibre, Retinol)

newdf <- health %>% group_by(SEQN) %>% mutate(count=n()) %>% arrange(desc(count)) %>% distinct() #%>% ungroup()

#diet <- newdf %>% mutate("ID" = SEQN, "Unhealthyness" = count) %>% select(ID,Unhealthyness)

hhss <- dplyr::select(newdf, SEQN)
diets <- select(diet,SEQN)
nulls <- anti_join(diets, hhss, by='SEQN') %>% mutate(count=0)
total_diet = bind_rows(newdf, nulls)

diet <- total_diet %>% mutate("ID" = SEQN, "Unhealthyness" = count) %>% select(ID,Unhealthyness)

######## FINDING THE PATIENT ID SUBSET: 

# All the IDs for the people with no insurance

ID_no_insurance <-  shorter_names %>%
  filter(health_Insurance == "no") %>% 
  select("ID")

length(ID_no_insurance$ID)

# All the IDs for the people with insurance

ID_insurance <-  shorter_names %>%
  filter(health_Insurance == "yes") %>% 
  select("ID")

# All the IDs for people with private insurance

ID_private_insurance <-  shorter_names %>%
  filter(private_insurance == 14) %>% 
  select("ID")

length(ID_private_insurance$ID)

# All the IDs for the people who has insurance but not a private one. 

ID_insurance_not_private <- ID_insurance[!ID_insurance$ID %in% ID_private_insurance$ID , ]

index_insurance <- shorter_names$ID %in% ID_private_insurance$ID

!index_insurance

shorter_names[index_insurance , ]

# The demographics table with only the IDs that are also in the 

Demo_and_ins <- left_join(shorter_names,demographics_new_names)

demo_ins_and_med <- left_join(Demo_and_ins, medication)

demo_ins_med_diet <- left_join(demo_ins_and_med,diet)

Columns_we_are_gonna_use <- demo_ins_med_diet %>% select(ID,Age,Income_range,Family_size,Druguse,Unhealthyness)

no_NAs_our_data <- drop_na(Columns_we_are_gonna_use)

rownames(no_NAs_our_data) <- c()

ID_as_column <- column_to_rownames(no_NAs_our_data, "ID")

scaled_ID_as_column <-  scale(ID_as_column)

#### Doing k-means: 

MAX_K = 15
wss = numeric(MAX_K)

for(k in 1: MAX_K){
  wss[k] = kmeans(scaled_ID_as_column, k)$tot.withinss
}

plot(1:MAX_K, wss, pch = 20, type = "o",xlab="Number of Clusters (K)",ylab="Within Sum of Squares (WSS)", main = "Total WSS against K")

