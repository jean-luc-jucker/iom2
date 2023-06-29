# Contrary to preparation_res_training_v1, this, again, make the merge instead
# of using that of the client. It is therefore based on preparation_res_v5,
# which last change was:
# Main difference from v4: we eliminate all 75 duplicates

getwd()
library(readxl)
library(tidyverse)

# LOAD DATA ####

# Mimosa data (mimosa)
mimosa <- read_excel('data_raw/MIMOSA Reint Cases M&E-eco v2 identification.xlsx', # new data
                     na = c('N/A', 'NA', 'na'))
dim(mimosa) # 222,379 x 14

# Reintegration Economic Survey (kobo) ####
kobo <- read_excel('data_raw/RE_Economic_Survey_clean for data analysis.xlsx', # same data
                   na = c('N/A', 'NA', 'na')) 
dim(kobo) # 2,073 x 154


# 1. MIMOSA ####

# Check perfect duplicates
sum(duplicated(mimosa)) # 0
# Check duplicates in caseno
sum(duplicated(mimosa$caseno)) # 21,907 # expected (members of same family)
# Check duplicates in MemberNo
sum(duplicated(mimosa$MemberNo)) # 0 # expected (unique ID)

# Check date formats
str(mimosa)
# Date de reception de la reintegration needs to be converted to date (from char)
# Check current formats
mimosa[mimosa$`Date de reception de la reintegration` != 'NULL', "Date de reception de la reintegration"]
# Seem to be integers, which might have been coerced by R. Let's convert them.

# First, we need to convert them to numeric

# NA before
sum(is.na(mimosa$`Date de reception de la reintegration`)) # 0
# Non-null before
dim(mimosa[mimosa$`Date de reception de la reintegration` != 'NULL', "Date de reception de la reintegration"]) # 119,102
mimosa$`Date de reception de la reintegration` <- as.numeric(mimosa$`Date de reception de la reintegration`) # NA warning

# NA after
sum(is.na(mimosa$`Date de reception de la reintegration`)) # 103,277
# Non-null after
dim(mimosa[mimosa$`Date de reception de la reintegration` != 'NULL', "Date de reception de la reintegration"]) # 222,379
# It seems 119,102 - 103,277 = 15,825 dates have been nanified

# But let's try to convert anyway
mimosa$`Date de reception de la reintegration` <- as.Date(mimosa$`Date de reception de la reintegration`, origin = "1899-12-30")

# NA finally
sum(is.na(mimosa$`Date de reception de la reintegration`)) # 103,277 as before

# Let's print a few
mimosa[!is.na(mimosa$`Date de reception de la reintegration`), "Date de reception de la reintegration"] # first, note length is 119,102, as expected

# Look good, but we'll need to compare raw and processed later


# Durations

# A calculer

# Temps entre retour et réception réintégration
# Mimosa:
# ArrivalDate_Mimosa #  ---> DateOfReturn
# Date de reception de la reintegration # ---> ReintegrationDate
# DONE (but check mistakes), new name ---> ReturnToReintegration

# Délai entre la réception de l'assistance te le jour de l'enquête
# Mimosa: Date de réception de la réintégration # --> ReintegrationDate
# Kobo: Date de l'enquête # --> InterviewDate
# DONE, new name ---> AssistanceDuration


# Déjà calculé
# Kobo:
#  DONE Durée de l’absence du pays d’origine   Mettre 0 si moins d'un an # ---> MigrationDuration
# Mimosa
#  DONE Duree formation (contrôler la variable -- OK!) # ---> TrainingDuration


mimosa_slim <- mimosa %>% select(
  # SELECT
  caseno,
  MemberNo,
  `Niveau microbusiness`,
  `Type de formation`,
  `Duree formation`, #
  ArrivalDate_Mimosa, #
  `Date de reception de la reintegration`, #
  Gender_Mimosa,
  AgeAtReferral_Mimosa
  
) %>% 
  # RENAME
  rename(
    "ID_1" = 
      caseno,
    "ID_2" = 
      MemberNo,
    "MicroBusinessLevel" =
      "Niveau microbusiness",
    "TrainingType" =
      "Type de formation",
    "TrainingDuration" = #
      "Duree formation",
    "DateOfReturn" = #
      "ArrivalDate_Mimosa",
    "ReintegrationDate" = 
      "Date de reception de la reintegration",
  ) %>% 
  
  # Add variables
  mutate(ReturnToReintegration = 
           as.numeric(difftime(ReintegrationDate, DateOfReturn, units = "days"))
  )



# 2. KOBO ####

# Check perfect duplicates
sum(duplicated(kobo)) # 0

# Check duplicates in Identifiant MiMOSA du cas bis
sum(duplicated(kobo$`Identifiant MiMOSA du cas bis`)) # 137 (+1), corresponding to blanks

kobo_slim <- kobo %>% select(
  # SELECT
  `Identifiant MiMOSA du cas bis`,
  
  
  # Metadata
  "Date de l'enquête", #
  "Mode d'enquête",
  # Dependent variables
  "Comment se porte votre entreprise ou business actuellement ?",
  "L’entreprise vous permet -elle de gagner assez d’argent pour subvenir à vos besoins et à celle de votre famille ?",
  "Avez-vous déjà planifié de migrer de nouveau ?",
  "Si vous n’aviez pas bénéficié de l’aide de l’OIM pour votre réintégration économique quelle serait votre situation actuelle ?",
  "Pensez-vous que le retour a été une bonne décision ?",
  "Êtes-vous satisfait de l’aide à la réintégration de manière globale ?",
  # Grouping variables
  "Pays",
  "Pays d’où le migrant est de retour :",
  "Sexe",
  "Age (l'enquête est destinée aux personnes agées de 14 ans et plus)",
  "Durée de l’absence du pays d’origine   Mettre 0 si moins d'un an",  #
  "Situation de handicap",
  
  
  "Par quel moyen avez-vous reçu cette assistance économique ?",
  "Type de business bis",
  "Qui sont les membres de cette entreprise ?",
  "L'OIM   ou un de ses partenaires vous a-t-elle formé sur la façon de gérer une entreprise ?",
  "L’entreprise emploie-t-elle du personnel ?",
  "Si oui, combien des personnes sont employées par votre entreprise ?",
  "Est-ce votre entreprise a été affectée par la maladie de Coronavirus ?",
  "Est-ce que le type d'assistance économique que vous avez reçu correspondait à votre premier choix ?"
  
  
) %>% 
  # RENAME
  rename("ID_1" = 
           `Identifiant MiMOSA du cas bis`,
         "InterviewDate" =
           "Date de l'enquête", #
         "InterviewType" =
           "Mode d'enquête",
         
         # Dependent variables
         "BusinessSuccess" =
           "Comment se porte votre entreprise ou business actuellement ?",
         "BusinessProfitability" =
           "L’entreprise vous permet -elle de gagner assez d’argent pour subvenir à vos besoins et à celle de votre famille ?",
         "WouldMigrateAgain" =
           "Avez-vous déjà planifié de migrer de nouveau ?",
         "SituationWithoutSupport" =
           "Si vous n’aviez pas bénéficié de l’aide de l’OIM pour votre réintégration économique quelle serait votre situation actuelle ?",
         "ReturningWasGoodDecision" =
           "Pensez-vous que le retour a été une bonne décision ?",
         "SatisfiedWithReintegrationSupport" =
           "Êtes-vous satisfait de l’aide à la réintégration de manière globale ?",
         
         # Grouping variables
         "Country" = 
           "Pays",
         "CountryOfReturn" =
           "Pays d’où le migrant est de retour :",
         "Gender" =
           "Sexe",
         "AgeGroup" =
           "Age (l'enquête est destinée aux personnes agées de 14 ans et plus)",
         "MigrationDuration" =
           "Durée de l’absence du pays d’origine   Mettre 0 si moins d'un an", #
         "Disabled" =
           "Situation de handicap",
         
         "ReceivedSupportAs" =
           "Par quel moyen avez-vous reçu cette assistance économique ?",
         "BusinessType" = 
           "Type de business bis",
         "BusinessMembers" =
           "Qui sont les membres de cette entreprise ?",
         "ReceivedIOMBusinessAdvice" =
           "L'OIM   ou un de ses partenaires vous a-t-elle formé sur la façon de gérer une entreprise ?",
         "BusinessHasEmployees" =
           "L’entreprise emploie-t-elle du personnel ?",
         "EmployeeNumber" =
           "Si oui, combien des personnes sont employées par votre entreprise ?",
         "CoronaImpactOnBusiness" =
           "Est-ce votre entreprise a été affectée par la maladie de Coronavirus ?",
         "FirstChoice" = 
           "Est-ce que le type d'assistance économique que vous avez reçu correspondait à votre premier choix ?"
         
  )



# Before the merge, interview_data from Kobo needs to be converted to proper date
# Type before
typeof(kobo_slim$InterviewDate) # char
# NA before
sum(is.na(kobo_slim$InterviewDate)) # 1
# Print a few dates
kobo_slim$InterviewDate # seems to be in y-m-d format
# Let's try to convert
library(lubridate)
kobo_slim$InterviewDate <-  ymd(kobo_slim$InterviewDate)
# Type after
typeof(kobo_slim$InterviewDate) # double
# NA after
sum(is.na(kobo_slim$InterviewDate)) # still 1, all good
# Also check using str
str(kobo_slim) # Now a date


# Merge ####

mimosa_slim # 222,379 x 10
kobo_slim # 2,073 x 23

# expected
# 2,073 x 30


# Implement stack here: #########################

mimosa_test <- mimosa_slim %>% 
  pivot_longer(c(ID_1, ID_2), values_to = "ID_1") %>% 
  select(-name) %>% 
  distinct_all()



result <- kobo_slim %>% left_join(mimosa_test)
result
dim(result) # 2,211, which is 2,211 - 2,073 = 138 more rows than in initial data!

colSums(is.na(result)) # ID_1 = 138, corresponding to blanks


sum(duplicated(result$ID_1)) # 275 (i.e., 275 - 138 blanks = our 137 rows (+1) from above)
to_resolve  <- result[duplicated(result$ID_1) | duplicated(result$ID_1, fromLast = TRUE), ]
to_resolve # 351 x 31

# Save file with errors and inconsistencies
write_csv(to_resolve, 'data_clean/res_training_to_resolve.csv')

# Count number of cases these errors represent
countingit  <- to_resolve %>% filter(!is.na(ID_1)) %>%  group_by(ID_1) %>% summarise(count=n())
countingit # The 138 supplementary rows are 75 cases

# Save cases
write_csv(countingit, 'data_clean/res_training_to_resolve_cases.csv')

sum(countingit$count) # they represent 213 rows

list(countingit$ID_1)


#[31] ""  ""  ""  ""  ""  "" 
#[37] ""  ""  ""  ""  ""  "" 
#[43] ""  ""  ""  ""  ""  ""
#[49] ""  ""  "" ""  ""  "" 
#[55] ""  ""  ""  ""  ""  ""
#[61] "" "" "" "" ""  "" 
#[67] ""  ""  ""  ""  ""  "" 
#[73] ""  ""  "" 
dim(result) # 2211

# Remove unresolvable cases, after discussion with client

res <- result %>% filter(ID_1 != "CH5017X029301" & ID_1 != "CH5017X029302" & ID_1 != "CH5018X008561" &
                    ID_1 != "DZ1021003884" & ID_1 != "DZ1021004350" & ID_1 != "EG1020001001" &
                    ID_1 != "LY1017002826" & ID_1 != "LY1018003615" & ID_1 != "LY1019002074" &
                    ID_1 != "LY1019005427" & ID_1 != "LY1019009209" & ID_1 != "LY1020005937" &
                    ID_1 != "LY1020005982" & ID_1 != "LY1021006171" & ID_1 != "LY1021006863" &
                    ID_1 != "LY1021007548" & ID_1 != "LY1021007767" & ID_1 != "LY1021008663" &
                    ID_1 != "LY1021008993" & ID_1 != "LY1021010520" & ID_1 != "LY1021010615" &
                    ID_1 != "LY1021011159" & ID_1 != "LY1021011513" & ID_1 != "LY1022001492" &
                    ID_1 != "LY1022002734" & ID_1 != "LY1022004031" & ID_1 != "LY1022004033" &
                    ID_1 != "MA1017002391" & ID_1 != "MA1018001175" & ID_1 != "MA1019001113" &
                    ID_1 != "MA1019001234" & ID_1 != "MA1019002188" & ID_1 != "MA1020003273" &
                    ID_1 != "MA1020003524" & ID_1 != "MA1020003929" & ID_1 != "MA1021001274" &
                    ID_1 != "MA1021001348" & ID_1 != "MA1021001608" & ID_1 != "MA1021001859" &
                    ID_1 != "MA1021001930" & ID_1 != "MA1021002702" & ID_1 != "MA1021002825" &
                    ID_1 != "MA1021003343" & ID_1 != "MA1021003397" & ID_1 != "MA1021003875" &
                    ID_1 != "MA1021003939" & ID_1 != "MA1021004177" & ID_1 != "ML1019X004589" &
                    ID_1 != "MR1020002212" & ID_1 != "MR1020002251" & ID_1 != "MR1020X000871" &
                    ID_1 != "MR1021001601" & ID_1 != "MR1022001155" & ID_1 != "MR1022001186" &
                    ID_1 != "NE1018001196" & ID_1 != "NE1018001888" & ID_1 != "NE1018004403" &
                    ID_1 != "NE1019002475" & ID_1 != "NE1019002484" & ID_1 != "NE1019X071222" &
                    ID_1 != "NE1020X006991" & ID_1 != "NE1020X020753" & ID_1 != "NE1021X051030" &
                    ID_1 != "NE1022X002609" & ID_1 != "SN1020001566" & ID_1 != "TD1021001049" &
                    ID_1 != "TD1021001245" & ID_1 != "TD1021001356" & ID_1 != "TD1021001444" &
                    ID_1 != "TD1021001618" & ID_1 != "TD1022001065" & ID_1 != "TD1022001219" &
                    ID_1 != "TN1018001384" & ID_1 != "TN1020001880" & ID_1 != "TN1021002294" 
                  | is.na(ID_1))


dim(res) # 1998
# This is 2,211 - 213 = 1,998 indeed
# Furthermore, we have 2,073 - 1,998 =  75 less participants than in initial data indeed!

# That said... we still have an issue!

# Some respondents who are men in Kobo are women in Mimosa:
res[res$Gender == 'Masculin' & res$Gender_Mimosa == 'Female' & !is.na(res$Gender) & !is.na(res$Gender_Mimosa), ] # 14

# And respondents who are women in Kobo are men in Mimosa:
res[res$Gender == 'Féminin' & res$Gender_Mimosa == 'Male' & !is.na(res$Gender) & !is.na(res$Gender_Mimosa), ] # 16


# Save these cases
gender_issue <- res[((res$Gender == 'Masculin' & res$Gender_Mimosa == 'Female' & !is.na(res$Gender) & !is.na(res$Gender_Mimosa)) | (res$Gender == 'Féminin' & res$Gender_Mimosa == 'Male' & !is.na(res$Gender) & !is.na(res$Gender_Mimosa))), ]

write_excel_csv(gender_issue, 'data_clean/res_training_to_resolve_gender.csv')




# After discussion with client, who cannot explain this, we decide to remove these
# 30 cases

res <- res[!((res$Gender == 'Masculin' & res$Gender_Mimosa == 'Female' & !is.na(res$Gender) & !is.na(res$Gender_Mimosa)) | (res$Gender == 'Féminin' & res$Gender_Mimosa == 'Male' & !is.na(res$Gender) & !is.na(res$Gender_Mimosa))), ]



dim(res) # 1968 x 31
# Indeed, 1998 - 30 = 1968 indeed


str(res)


# Compute last needed variable, which is
# Délai entre la réception de l'assistance te le jour de l'enquête
# Mimosa: Date de réception de la réintégration # --> ReintegrationDate
# Kobo: Date de l'enquête # --> InterviewDate
# DONE, new name ---> AssistanceDuration

res  <- res %>% mutate(AssistanceDuration = 
                         as.numeric(difftime(InterviewDate, ReintegrationDate, units = "days")))


dim(res) # 1968 x 32, as expected

# Subset to only variables we'll need
res  <- res %>% select(-c(Gender_Mimosa, AgeAtReferral_Mimosa))
dim(res) # 1,968 x 30


# Export
write_excel_csv(res, 'data_clean/res_training.csv')
# RDS version
saveRDS(res, file = 'data_clean/res_training.rds')


# Recode independent variables

# Print counts
res %>% group_by(BusinessSuccess) %>% summarise(count = n()) %>%
  mutate(percent = count/sum(count)*100) %>% arrange(-percent)
res %>% group_by(BusinessProfitability) %>% summarise(count = n()) %>%
  mutate(percent = count/sum(count)*100) %>% arrange(-percent)
res %>% group_by(WouldMigrateAgain) %>% summarise(count = n()) %>%
  mutate(percent = count/sum(count)*100) %>% arrange(-percent)

res <- res %>% mutate(
  BusinessSuccess =
    case_when(BusinessSuccess == 'Actuellement ouvert et les activités marchent bien' ~ 'High',
              BusinessSuccess != 'Actuellement ouvert et les activités marchent bien' ~ 'Low'),
  BusinessProfitability =
    case_when(BusinessProfitability == 'Oui' ~ 'High',
              BusinessProfitability != 'Oui' ~ 'Low'),
  WouldMigrateAgain = 
    case_when(WouldMigrateAgain == 'Non' ~ 'No',
              WouldMigrateAgain != 'Non' ~ 'Yes')
)

# Reprint counts
res %>% group_by(BusinessSuccess) %>% summarise(count = n()) %>%
  mutate(percent = count/sum(count)*100) %>% arrange(-percent)
res %>% group_by(BusinessProfitability) %>% summarise(count = n()) %>%
  mutate(percent = count/sum(count)*100) %>% arrange(-percent)
res %>% group_by(WouldMigrateAgain) %>% summarise(count = n()) %>%
  mutate(percent = count/sum(count)*100) %>% arrange(-percent)


# Assess NA
colSums(is.na(res))


# Drop NA from our main variables of interest

# Dim before
dim(res) # 1968 x 30

res <- res %>% drop_na(BusinessSuccess, WouldMigrateAgain, Gender, CountryOfReturn)

# Reassess NA
colSums(is.na(res))

# New dim
dim(res) # 1892 x 30, as expected




# Recode independent variables
# Taken from preparation_res.R, with some exceptions,
# notably for Mimosa variables

names(res)

# Before
res %>% group_by(TrainingDuration) %>% summarise(count = n()) %>% 
  mutate(percent = count/sum(count)*100) %>% arrange(-percent) %>% print(n=30)

res <- res %>% mutate(
  
  # RECODE
  InterviewType = 
    case_when(InterviewType == 'Par téléphone' ~ 'Par téléphone',
              InterviewType != 'Par téléphone' ~ 'Terrain/bureau OIM'
    ),
  Country = 
    case_when(
      Country != "Sénégal" & Country != "Guinée" & Country != "Côte D'Ivoire" & Country != "Burkina Faso" & Country != "Ghana" ~ 'Autre',
      TRUE ~ as.character(Country)
    ),
  CountryOfReturn =
    case_when(
      CountryOfReturn != "Lybie" & CountryOfReturn != "Algerie" & CountryOfReturn != "Niger" & CountryOfReturn != "Maroc" ~ 'Autre',
      TRUE ~ as.character(CountryOfReturn)
    ),
  Gender =
    case_when(
      Gender == "Masculin"  ~ "Masculin",
      Gender == "Féminin" ~ "Féminin" # we purposefully do not specify Refused, to convert
      # it to a NA
    ),
  AgeGroup =
    case_when(
      AgeGroup == "18-35 ans" ~ "14-35",
      AgeGroup == "36-65 ans" ~ "36+",
      AgeGroup == "14-17 ans" ~ "14-35",
      AgeGroup == "+65 ans" ~ "36+"
    ),
  BusinessType = 
    case_when(
      BusinessType == "Commerce" ~ "Commerce",
      BusinessType == "Elevage" ~ "Elevage",
      BusinessType == "Transport (Moto - Auto et autres)" ~ "Transport",
      BusinessType == "Agriculture" ~ "Agriculture/aviculture",
      BusinessType == "Aviculture" ~ "Agriculture/aviculture",
      BusinessType == "Artisan-Ouvrier" ~ "Autre",
      BusinessType == "Couture" ~ "Autre",
      BusinessType == "Autre" ~ "Autre",
      BusinessType == "Restauration" ~ "Autre",
      BusinessType == "Bâtiment/construction" ~ "Autre",
      BusinessType == "Coiffure" ~ "Autre",
      BusinessType == "Mécanique" ~ "Autre",
      BusinessType == "Pêche" ~ "Autre"
    ),
  BusinessMembers = 
    case_when(BusinessMembers != "Moi uniquement" ~ "Moi et d'autres",
              TRUE ~ as.character(BusinessMembers)
    ),
  ReceivedIOMBusinessAdvice =
    case_when(
      ReceivedIOMBusinessAdvice == "Non"  ~ "Non",
      ReceivedIOMBusinessAdvice == "Oui" ~ "Oui" # TRUE purposefully not
      # specified to convert Ne sait pas and Refused to NA
    ),
  BusinessHasEmployees = 
    case_when(
      BusinessHasEmployees == "Non"  ~ "Non",
      BusinessHasEmployees == "Oui" ~ "Autre",
      BusinessHasEmployees == "Souhaite ne pas répondre" ~ "Autre" # group Oui
      # and Refused to avoid losing 48 respondents
    ),
  EmployeeNumber = 
    case_when(EmployeeNumber == "1" ~ "1",
              EmployeeNumber == "2" ~ "1+",
              EmployeeNumber == "4 et plus" ~ "1+",
              EmployeeNumber == "3" ~ "1+",
              EmployeeNumber == "Ne souhaite pas répondre" ~ "1", # we know that
              # the 6 respondents who Refused to answer replied to the previous
              # question (BusinessHasEmployees) that they have employees. We decide
              # to replace the refused with the most common value for respondents
              # who have employees, that is, 1
    ),
  CoronaImpactOnBusiness =
    case_when(CoronaImpactOnBusiness != "Non" ~ "Oui",
              TRUE ~ as.character(CoronaImpactOnBusiness)
    ),
  FirstChoice = 
    case_when(
      FirstChoice == "Non"  ~ "Non",
      FirstChoice == "Oui" ~ "Oui" # TRUE purposefully not
      # specified to convert Refused to NA
    ),
  
  # From Mimosa--NEW
  
  TrainingType =
    case_when(
      TrainingType == "Business development/management training" ~ "Business/management",
      TrainingType == "NULL" ~ "None", # Decision we make
      TrainingType == "Vocational training" ~ "Other",
      TrainingType == "Non spécifié" ~ "Other",
      TrainingType == "Other" ~ "Other", # NA are filled below
      
    ),
  
  TrainingDuration = 
    case_when(
      TrainingDuration == "NULL" ~ "0",
      TRUE ~ as.character(TrainingDuration)
    )
  
) %>% 
  
  # REPLACE NA
  mutate(BusinessHasEmployees = replace_na(BusinessHasEmployees, "Autre"), # to avoid
         # losing 48 respondents
         EmployeeNumber = replace_na(EmployeeNumber, "0"), # All NA are for respondents
         # who do not have employees, so we can replace NA with 0
         TrainingType = replace_na(TrainingType, "None") # We decide to replace NA to None
  )

# After
res %>% group_by(TrainingDuration) %>% summarise(count = n()) %>% 
  mutate(percent = count/sum(count)*100) %>% arrange(-percent)
#

dim(res)


# Before we clean numeric variables, let's do a NA assessment and remove
# some variables as discussed above

# Current dim
dim(res) # 1892 x 30, as above

# Reassess NA
colSums(is.na(res))

# For the following variables, we will drop rows with NA:
# Gender (1)
# BusinessType (18)
# ReceivedIOMBusinessAdvice (11)
# FirstChoice (12)
# Total: 42

# That's because these cannot be meaningfully recoded or solved as we have done
# in the previous step. Let's do it.

res <- res %>% drop_na(Gender, BusinessType, ReceivedIOMBusinessAdvice, FirstChoice)

# New dim
dim(res) # 1,852 x 30, meaning we lost 40 observations (in other words, 2 were missing
# for several variables)

# Let's also discard 2 variables entirely, since they are very low count and cannot
# be recoded. These variables are:
# Disabled, who has only 89
# MicroBusinessLevel (from Mimosa), due to following counts
#MicroBusinessLevel count percent
#Individual          1541   81.4 
#NA                   199   10.5 
#Collective            71    3.75
#Community             54    2.85
#NULL                  27    1.43

res <- res %>% select(-c(Disabled, MicroBusinessLevel))

# New dim
dim(res) # 1,852 x 28, as expected

# New NA assessment
colSums(is.na(res)) # 0 in all expected (i.e. categorical) variables

# We are finally ready to clean numeric variables. Let's do it.


# Our numeric variables are:
str(res)

# (1) MigrationDuration,      a char to be converted to num,  with  97 NA
# (2) TrainingDuration,       a char to be converted to num,  with 194 NA
# (3) ReturnToReintegration,  already a num,                  with 206 NA
# (4) AssistanceDuration,     already a num,                  with 206 NA

# Also remember what these variables are:

# (1) MigrationDuration     Kobo,   "Durée de l’absence du pays d’origine   Mettre 0 si moins d'un an"
#     --> available as such in raw data
#     --> in years

# (2) TrainingDuration      Mimosa, "Duree formation"
#     --> available as such in raw data
#     --> in days

# (3) ReturnToReintegration Mimosa, "ArrivalDate_Mimosa"                    renamed DateOfReturn 
#                       AND Mimosa, "Date de reception de la reintegration" renamed ReintegrationDate
#     --> computed as ReintegrationDate - DateOfReturn
#     --> in days

# (4) AssistanceDuration    Mimosa, "Date de réception de la réintégration" renamed ReintegrationDate
#                       AND Kobo,   "Date de l'enquête"                     renamed InterviewDate
#     --> computed as InterviewDate - ReintegrationDate
#     --> in days


# We'll try to use a similar code as in preparation_rss.R. That's because it is also
# based on a merge (i.e, it has Mimosa data), contrary to preparation.res.R, which only
# has kobo data.

# This table will come handy to be sure we deal with the same variables

# Name in RES ################################## Name in RSS ### Checked it is same variable?
# MigrationDuration-----------------------migration_duration-----YES
# TrainingDuration--------------------------TrainingDuration-----YES, but in RSS, was computed using
                                                                 # TrainingEndDate & TrainingStartDate
# ReturnToReintegration-------------------No equivalent in RSS---Not applicable
# AssistanceDuration----------------------No equivalent in RSS---But... is similar to
                                                                 # MBAssistanceDuration, which is 
                                                                 # interview_date [Kobo]
                                                                 # - MicrobusinessEndDate [Mimosa]


# MigrationDuration

# To be converted to num first
# Conversion might coerce to NA; check NA count before
sum(is.na(res$MigrationDuration)) # 97
# Print levels before
res %>% group_by(MigrationDuration) %>% summarise(count = n()) %>% print(n=49)
# Convert to number
res$MigrationDuration  <- as.numeric(res$MigrationDuration)
# Print levels after
res %>% group_by(MigrationDuration) %>% summarise(count = n()) %>% print(n=49) # all good
# Check NA count after
sum(is.na(res$MigrationDuration)) # still 97, all good
# First, we'll replace these 3 values, which are mistakes and not outliers,
# with NA
res[res$MigrationDuration > 100 & !is.na(res$MigrationDuration), "MigrationDuration"]
res[res$MigrationDuration > 100 & !is.na(res$MigrationDuration), "MigrationDuration"]  <- NA
# New NA count
sum(is.na(res$MigrationDuration)) # 100, as expected
# Summarise
summary(res$MigrationDuration) # median = 2, min 0, max 36, NA 100, all as expected
# Let's store the median
q_median <- median(res$MigrationDuration, na.rm = TRUE)
q_median # 2 years
# Boxplot before
boxplot(res$MigrationDuration,
        ylab = "Years",
        main = "MigrationDuration"
)
# Spot outliers using percentile method, with conservative threshold of 0.01/0.99
lower_bound <- quantile(res$MigrationDuration, 0.01, na.rm = TRUE)
upper_bound <- quantile(res$MigrationDuration, 0.99, na.rm = TRUE)
outlier_ind <- which(res$MigrationDuration < lower_bound | res$MigrationDuration > upper_bound)
dim(res[outlier_ind, "MigrationDuration"]) # 16 outliers,
res[outlier_ind, "MigrationDuration"] # with smallest being 16 years
# Replace 16 extreme outliers with median
res[outlier_ind, "MigrationDuration"] <- q_median
# Boxplot after
boxplot(res$MigrationDuration,
        ylab = "Years",
        main = "MigrationDuration"
)
# Re-summarise
summary(res$MigrationDuration) # median still 2, max 14 years as expected
# We should still have the same number of NA
sum(is.na(res$MigrationDuration)) # 100 --> as expected
# And we will also replace them with the median
res[is.na(res$MigrationDuration), "MigrationDuration"] <- q_median
# Re-summarise
summary(res$MigrationDuration) # median still 2, max still 14
# No NA should remain
sum(is.na(res$MigrationDuration)) # 0, all good



# TrainingDuration

# To be converted to num first
# Conversion might coerce to NA; check NA count before
sum(is.na(res$TrainingDuration)) # 194
# Print levels before
res %>% group_by(TrainingDuration) %>% summarise(count = n()) %>% print(n=49)
# Convert to number
res$TrainingDuration  <- as.numeric(res$TrainingDuration)
# Print levels after
res %>% group_by(TrainingDuration) %>% summarise(count = n()) %>% print(n=49) # all good
# Check NA count after
sum(is.na(res$TrainingDuration)) # still 194, all good
# Summarise
summary(res$TrainingDuration) # median = 0, min 0, max 756, NA 194, all as expected
# Let's store the median
q_median <- median(res$TrainingDuration, na.rm = TRUE)
q_median # 0
# Boxplot before
boxplot(res$TrainingDuration,
        ylab = "Days",
        main = "TrainingDuration"
)
# Spot outliers using percentile method, with conservative threshold of 0.01/0.99
lower_bound <- quantile(res$TrainingDuration, 0.01, na.rm = TRUE)
upper_bound <- quantile(res$TrainingDuration, 0.99, na.rm = TRUE)
outlier_ind <- which(res$TrainingDuration < lower_bound | res$TrainingDuration > upper_bound)
dim(res[outlier_ind, "TrainingDuration"]) # 11 outliers,
res[outlier_ind, "TrainingDuration"] # with smallest being 35 days
# Replace 11 extreme outliers with median
res[outlier_ind, "TrainingDuration"] <- q_median
# Boxplot after
boxplot(res$TrainingDuration,
        ylab = "Days",
        main = "TrainingDuration"
)
# Re-summarise
summary(res$TrainingDuration) # median still 0, max 34 years as expected
# We should still have the same number of NA
sum(is.na(res$TrainingDuration)) # 194 --> as expected
# And we will also replace them with the median
res[is.na(res$TrainingDuration), "TrainingDuration"] <- q_median
# Re-summarise
summary(res$TrainingDuration) # median still 0, max still 34
# No NA should remain
sum(is.na(res$TrainingDuration)) # 0, all good


# ReturnToReintegration

# Print NA
sum(is.na(res$ReturnToReintegration)) # 206
# Summarise
summary(res$ReturnToReintegration) # median = 212, min -132!, max 2025
# We have some negative values, which is weird. Let's take a look
res[res$ReturnToReintegration <= 0 & !is.na(res$ReturnToReintegration), "ReturnToReintegration"] # 7
# We will replace these values with the median. For this, we need to 
# compute the median without taking the negative values into consideration:
summary(res[!res$ReturnToReintegration <= 0 & !is.na(res$ReturnToReintegration), "ReturnToReintegration"]) # 1,639, which is 1852 - 7 negative values - 206 NA indeed
# Store median
q_median <- 214
# Replace negative values with the updated median (note, we keep 1 zero value)
res[res$ReturnToReintegration < 0 & !is.na(res$ReturnToReintegration), "ReturnToReintegration"] <- q_median
# Resummarise
summary(res$ReturnToReintegration) # median = now 214, min now 0, max still 2025, all as expected
# Boxplot before
boxplot(res$ReturnToReintegration,
        ylab = "Days",
        main = "ReturnToReintegration"
)
# Spot outliers using percentile method, with conservative threshold of 0.01/0.99
lower_bound <- quantile(res$ReturnToReintegration, 0.01, na.rm = TRUE)
upper_bound <- quantile(res$ReturnToReintegration, 0.99, na.rm = TRUE)
outlier_ind <- which(res$ReturnToReintegration < lower_bound | res$ReturnToReintegration > upper_bound)
dim(res[outlier_ind, "ReturnToReintegration"]) # 32 outliers,
res[outlier_ind, "ReturnToReintegration"] %>% print(n=32) # with smallest being 0 days or 1552 days
# We do not want to replace lower_bound outliers, though, so we take only upper bound
lower_bound <- quantile(res$ReturnToReintegration, 0.01, na.rm = TRUE)
upper_bound <- quantile(res$ReturnToReintegration, 0.99, na.rm = TRUE)
outlier_ind <- which(res$ReturnToReintegration > upper_bound) # only upper bound
dim(res[outlier_ind, "ReturnToReintegration"]) # 17 outliers,
res[outlier_ind, "ReturnToReintegration"] %>% print(n=32) # with smallest being 1552 days
# Replace 17 extreme outliers with median
res[outlier_ind, "ReturnToReintegration"] <- q_median
# Boxplot after
boxplot(res$ReturnToReintegration,
        ylab = "Days",
        main = "ReturnToReintegration"
)
# Re-summarise
summary(res$ReturnToReintegration) # median still 214, max 1543 years as expected
# We should still have the same number of NA
sum(is.na(res$ReturnToReintegration)) # 206 --> as expected
# And we will also replace them with the median
res[is.na(res$ReturnToReintegration), "ReturnToReintegration"] <- q_median
# Re-summarise
summary(res$ReturnToReintegration) # median still 214, max still 1543
# No NA should remain
sum(is.na(res$ReturnToReintegration)) # 0, all good




# AssistanceDuration


# Print NA
sum(is.na(res$AssistanceDuration)) # 206
# Summarise
summary(res$AssistanceDuration) # median = 92, min -574!, max 1468
# We have some negative values, which is weird. Let's take a look
res[res$AssistanceDuration <= 0 & !is.na(res$AssistanceDuration), "AssistanceDuration"] # 237
# We will replace these values with the median. For this, we need to 
# compute the median without taking the negative values into consideration:
summary(res[!res$AssistanceDuration <= 0 & !is.na(res$AssistanceDuration), "AssistanceDuration"]) # 1,409, which is 1852 - 237 negative values - 206 NA indeed
# Store median
q_median <- 112
# Replace negative values with the updated median (note, we keep the zero values)
res[res$AssistanceDuration < 0 & !is.na(res$AssistanceDuration), "AssistanceDuration"] <- q_median
# Resummarise
summary(res$AssistanceDuration) # median = now 112, min now 0, max still 1468, all as expected
# Boxplot before
boxplot(res$AssistanceDuration,
        ylab = "Days",
        main = "AssistanceDuration"
)
# Spot outliers using percentile method, with conservative threshold of 0.01/0.99
lower_bound <- quantile(res$AssistanceDuration, 0.01, na.rm = TRUE)
upper_bound <- quantile(res$AssistanceDuration, 0.99, na.rm = TRUE)
outlier_ind <- which(res$AssistanceDuration < lower_bound | res$AssistanceDuration > upper_bound)
dim(res[outlier_ind, "AssistanceDuration"]) # 34 outliers,
res[outlier_ind, "AssistanceDuration"] %>% print(n=34) # with smallest being 0 days or 1130 days
# We do not want to replace lower_bound outliers, though, so we take only upper bound
lower_bound <- quantile(res$AssistanceDuration, 0.01, na.rm = TRUE)
upper_bound <- quantile(res$AssistanceDuration, 0.99, na.rm = TRUE)
outlier_ind <- which(res$AssistanceDuration > upper_bound) # only upper bound
dim(res[outlier_ind, "AssistanceDuration"]) # 17 outliers,
res[outlier_ind, "AssistanceDuration"] %>% print(n=32) # with smallest being 1130 days
# Replace 17 extreme outliers with median
res[outlier_ind, "AssistanceDuration"] <- q_median
# Boxplot after
boxplot(res$AssistanceDuration,
        ylab = "Days",
        main = "AssistanceDuration"
)
# Re-summarise
summary(res$AssistanceDuration) # median still 112, max 1122 days as expected
# We should still have the same number of NA
sum(is.na(res$AssistanceDuration)) # 206 --> as expected
# And we will also replace them with the median
res[is.na(res$AssistanceDuration), "AssistanceDuration"] <- q_median
# Re-summarise
summary(res$AssistanceDuration) # median still 112, max still 1122
# No NA should remain
sum(is.na(res$AssistanceDuration)) # 0, all good



# Export slim version

# Final dim
dim(res) # 1,852 x 28

# Export
write_excel_csv(res, 'data_clean/res_training_slim.csv')
# RDS version
saveRDS(res, file = 'data_clean/res_training_slim.rds')










# Export
#write_excel_csv(df, 'data_clean/res_slim.csv') # using extension .xls will avoid
# wrapping, but will produce unsafe warning, so we use .csv
# RDS version
#saveRDS(df, file = 'data_clean/res_slim.rds')



# NOTES

# #"AssistanceType" or "Quel est le principal type d’assistance économique que
# vous avez reçue ?", was completely removed since counts are as follows:
#AssistanceType     count  percent
#1 Micro Business    1946 99.9   
#2 Autre                1  0.0513
#3 Placement emploi     1  0.0513
# That said, it might be good to say in the report that nearly all assistance
# received was for micro business.






