# Main difference from all previous versions: We no longer do a merge, and use only
# Kobo data, following discussion with client. All training variables are dropped.

getwd()
library(readxl)
library(tidyverse)

# Reintegration Economic Survey ####
kobo <- read_excel('data_raw/RE_Economic_Survey_clean for data analysis_final.xlsx',
                  na = c('N/A', 'NA', 'na')) 
warnings()
dim(kobo) #   2,073 x 154
# Previously 2,001 x 154

# Subset ####
res <- kobo %>% 
  select(
    # Metadata
    "Identifiant MiMOSA du cas bis",
    "Date de l'enquête",
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
    "Durée de l’absence du pays d’origine   Mettre 0 si moins d'un an",
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
  rename(
    # Metadata
    "MimosaID" =
      "Identifiant MiMOSA du cas bis", 
    "InterviewDate" =
      "Date de l'enquête",
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
      "Durée de l’absence du pays d’origine   Mettre 0 si moins d'un an",
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
    
         ) %>% 
  # Filter out NL countries (Togo = 8, Sierra-Leone = 45)
  filter(Country != 'Togo' & Country != 'Sierra-Leone')

dim(res) # 2,026 x 23
# Previously 1948 x 32
#view(res)

# NA
colSums(is.na(res))

# Dupes

# Perfect
sum(duplicated(res)) # 0
# Previously 0

# Pseudo
sum(duplicated(res$MimosaID)) # 132
# Previously 95
res[duplicated(res$MimosaID), 'MimosaID'] %>% print(n = 132)
# Note, they are all NA, as expected

# Data types
str(res)
# MigrationDuration     chr --> needs to be converted *
# TimeToReceiveSupport  chr --> needs to be converted # no longer exists
# SupportDuration       num # no longer exists
# TrainingDuration      num # no longer exists

# Check levels before
res %>% group_by(MigrationDuration) %>% summarise(count = n()) %>% print(n = 33) # --> all are numbers,
# with also 114 NA, which we'll convert to median later

# Converting to numeric could produce NA
# Print before
colSums(is.na(res))
# MigrationDuration     114


# Character to Numeric
res$MigrationDuration <- as.numeric(res$MigrationDuration)

# Print after
colSums(is.na(res))
# MigrationDuration     still 114, all good


# Check levels after
res %>% group_by(MigrationDuration) %>% summarise(count = n()) %>% print(n = 29) # --> we see, e.g.,
# that count of 3 is 255, which are the 253 3 plus 2 03 from above


# Export
#write_excel_csv(res, 'data_clean/res.csv') # using extension .xls will avoid
# wrapping, but will produce unsafe warning, so we use .csv
# RDS version
#saveRDS(res, file = 'data_clean/res.rds')


# Recode dependent variables

# Print levels
levels(as.factor(res$BusinessSuccess))
levels(as.factor(res$BusinessProfitability))
levels(as.factor(res$WouldMigrateAgain))

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


# Recode Independent variables

# Principles:
# Answers who represent less than 10% of all answers are grouped together
# Exceptions:
# - Disabled, with 95% Non and 5% Yes, is kept as is
# - MicroBusinessLevel, we kept Collective and Individual together (7.29%),
# Since removing them would mean not being able to use this variable at
# all
# - EmployeeNumber, we kept 1+ employee (160) to be able to make at least
# one comparison
# The below section was revised after client recoding some of these variables,
# notably, BusinessType

colSums(is.na(res))

# Before
res %>% group_by(BusinessType) %>% summarise(count = n()) %>% 
  mutate(percent = count/sum(count)*100) %>% arrange(-percent) %>% print(n=21)

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
      # and Refused to avoid losing 124 respondents
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
    )
                ) %>% 
  
  # REPLACE NA
  mutate(BusinessHasEmployees = replace_na(BusinessHasEmployees, "Autre"), # to avoid
         # losing 124 respondents
         EmployeeNumber = replace_na(EmployeeNumber, "0") # All NA are for respondents
         # who do not have employees, so we can replace NA with 0
         )

# After
res %>% group_by(BusinessType) %>% summarise(count = n()) %>% 
  mutate(percent = count/sum(count)*100) %>% arrange(-percent)


# Outliers; fill NA in numeric variables

# MigrationDuration
# Definition: "Durée de l’absence du pays d’origine   Mettre 0 si moins d'un an"
# In years
# NA
sum(is.na(res$MigrationDuration)) # 114
# Previously 110

# Summarise
summary(res$MigrationDuration)
# Check no values are under zero
res[res$MigrationDuration <= 0, "MigrationDuration"] %>% arrange(MigrationDuration) %>% 
  print(n=565) # 565, with 451 zeros and 114 NA
# Previously 541, with 431 zero and 110 NA

# First, we'll replace these 3 values, which are mistakes and not outliers,
# with NA
res[order(res$MigrationDuration, decreasing=TRUE), 'MigrationDuration'][0:3,]
res[res$MigrationDuration >= 936 & !is.na(res$MigrationDuration), 'MigrationDuration']  <- NA
sum(is.na(res$MigrationDuration)) # 117 --> as expected
# Let's re-summarise
summary(res$MigrationDuration)
# And let's store the median
q_median <- median(res$MigrationDuration, na.rm = TRUE)
q_median # 2 years


# Make a boxplot (IQR)

# Boxplot has a function to detect outliers
outliers <- boxplot.stats(res$MigrationDuration)$out
out_ind <- which(res$MigrationDuration %in% c(outliers))
out_ind

# Print outliers
res[out_ind, "MigrationDuration"] %>% arrange(MigrationDuration) # 132, with
# smallest being 7 years
# Previously 130, with smallest being 7 years

# Plot outliers
boxplot(res$MigrationDuration,
        ylab = "Years",
        main = "MigrationDuration"
)

# Spot outliers using percentile method, with conservative threshold of 0.01/0.99
lower_bound <- quantile(res$MigrationDuration, 0.01, na.rm = TRUE)
upper_bound <- quantile(res$MigrationDuration, 0.99, na.rm = TRUE)
outlier_ind <- which(res$MigrationDuration < lower_bound | res$MigrationDuration > upper_bound)
res[outlier_ind, "MigrationDuration"] %>% arrange(MigrationDuration) # 18, with smallest being 15 years
# Previously 18, with smallest being 15 years


# Replace 18 extreme outliers with median
res[outlier_ind, "MigrationDuration"] <- q_median

# Re-summarise
summary(res$MigrationDuration) # median still 2, max 14 years as expected
# Replot
outliers <- boxplot.stats(res$MigrationDuration)$out
out_ind <- which(res$MigrationDuration %in% c(outliers))
boxplot(res$MigrationDuration,
        ylab = "Years",
        main = "MigrationDuration"
)
# It is looking better
# We should still have the same number of NA
sum(is.na(res$MigrationDuration)) # 117 --> as expected
# And we will also replace them with the median
res[is.na(res$MigrationDuration), "MigrationDuration"] <- q_median
# Final summary and plot
summary(res$MigrationDuration) # median still 2, max still 14 years
outliers <- boxplot.stats(res$MigrationDuration)$out
out_ind <- which(res$MigrationDuration %in% c(outliers))
boxplot(res$MigrationDuration,
        ylab = "Years",
        main = "MigrationDuration"
)
# As above
# Check no NA are remaining
sum(is.na(res$MigrationDuration)) # 0, all good


# Assess NA
dim(res) # 2,026 x 23
colSums(is.na(res))


# Drop NA
res <- res %>% drop_na(BusinessSuccess, WouldMigrateAgain, Gender, CountryOfReturn,
                     BusinessType, ReceivedIOMBusinessAdvice, FirstChoice)

# Reassess NA
colSums(is.na(res))
# None are remaining except for the ID, but we won't use it in regression,
# so we are good.

# Print size
dim(res) # 1,917 x 23
# We therefore lost 2,026 - 1,917 = 109 cases


# Export
#write_excel_csv(res, 'data_clean/res_slim.csv') # using extension .xls will avoid
# wrapping, but will produce unsafe warning, so we use .csv
# RDS version
#saveRDS(res, file = 'data_clean/res_slim.rds')


# Check levels n before regression

dim(res)

levels_count <- res %>% group_by(BusinessSuccess, InterviewType, Country, CountryOfReturn,
                Gender, AgeGroup,
                 Disabled, ReceivedSupportAs, BusinessType, BusinessMembers,
                 ReceivedIOMBusinessAdvice, BusinessHasEmployees, EmployeeNumber,
                 CoronaImpactOnBusiness, FirstChoice
                 ) %>% summarise(Count = n()) %>% arrange(Count)

#write_excel_csv(levels_count, 'data_clean/levels_count_model_1.csv')



levels_count %>% pivot_wider(names_from = BusinessSuccess, values_from = Count) %>% view()


pivot


# Crosstable

library(crosstable)

crosstable(res, cols = c(InterviewType, Country, CountryOfReturn, Gender, AgeGroup,
                         Disabled, ReceivedSupportAs, BusinessType, BusinessMembers,
                         ReceivedIOMBusinessAdvice, BusinessHasEmployees, EmployeeNumber,
                         CoronaImpactOnBusiness, FirstChoice),
           by = c(BusinessSuccess), 
                  percent_digits = 0, percent_pattern = "{n}",
                  showNA = 'ifany', label = FALSE) %>% print(n=40)


# Will take forever:
#crosstable(res, cols = c(BusinessSuccess),
#           by = c(InterviewType, Country, CountryOfReturn, Gender, AgeGroup,
#                  Disabled, ReceivedSupportAs, BusinessType, BusinessMembers,
#                  ReceivedIOMBusinessAdvice, BusinessHasEmployees, EmployeeNumber,
#                  CoronaImpactOnBusiness, FirstChoice), 
#           percent_digits = 0, percent_pattern = "{n}",
#           showNA = 'ifany', label = FALSE)



crosst



# NOTES

# #"AssistanceType" or "Quel est le principal type d’assistance économique que
# vous avez reçue ?", was completely removed since counts are as follows:
#AssistanceType     count  percent
#1 Micro Business    1946 99.9   
#2 Autre                1  0.0513
#3 Placement emploi     1  0.0513
# That said, it might be good to say in the report that nearly all assistance
# received was for micro business.
