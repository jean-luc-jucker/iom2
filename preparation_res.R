# Main difference from all previous versions: We no longer do a merge, and use only
# Kobo data, following discussion with client. All training variables are dropped.

getwd()
library(readxl)
library(tidyverse)

# Reintegration Economic Survey ####
res <- read_excel('data_raw/RE_Economic_Survey_clean for data analysis_final.xlsx',
                  na = c('N/A', 'NA', 'na')) 
warnings()
dim(res) #   2,001 x 154
# Previously 2,012 x 154

# Subset ####
df <- res %>% 
  select(
    # Metadata
    "Identifiant MiMOSA du cas bis",
    "Projet Bis",
    "Date de l'enquête",
    "Date de reception de la reintegration", # We choose this one since it has less NA
    # than Date of beginning of training
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
    "Type de formation",
    "Date de debut formation",
    "Date de fin formation",
    "Combien de temps entre votre retour et la réception de l’aide à la réintégration (ou sa première fourniture) ? En semaines",
    "Par quel moyen avez-vous reçu cette assistance économique ?",
    #"Quel est le principal type d’assistance économique que vous avez reçue ?",
    "Type de business bis",
    "Qui sont les membres de cette entreprise ?",
    "Niveau microbusiness",
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
    "Project" =
      "Projet Bis",
    "InterviewDate" =
      "Date de l'enquête",
    "ReintegrationDate" = 
      "Date de reception de la reintegration",
    "InterviewType" =
      "Mode d'enquête",
    # Dependent variables
    "BusinessSucess" =
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
    "TrainingType" =
      "Type de formation",
    "TrainingStart" = 
      "Date de debut formation",
    "TrainingEnd" = 
      "Date de fin formation",
    "TimeToReceiveSupport" =
      "Combien de temps entre votre retour et la réception de l’aide à la réintégration (ou sa première fourniture) ? En semaines",
    "ReceivedSupportAs" =
      "Par quel moyen avez-vous reçu cette assistance économique ?",
    #"AssistanceType" =
    #  "Quel est le principal type d’assistance économique que vous avez reçue ?",
    "BusinessType" = 
      "Type de business bis",
    "BusinessMembers" =
      "Qui sont les membres de cette entreprise ?",
    "MicroBusinessLevel" =
      "Niveau microbusiness",
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
  # Add variables
  mutate(SupportDuration = 
          as.numeric(difftime(InterviewDate, ReintegrationDate, units = "days")),
         TrainingDuration = 
           as.numeric(difftime(TrainingEnd, TrainingStart, units = "days"))
         ) %>% 
  # Filter out NL countries (Togo = 8, Sierra-Leone = 45)
  filter(Country != 'Togo' & Country != 'Sierra-Leone')

dim(df) #    1948 x 32
# Previously 1959 x 32
#view(df)

# NA
colSums(is.na(df))

# Dupes

# Perfect
sum(duplicated(df)) # 0
# Previously 2
dim(df) # 1948 x 33
# Previously 1957 x 33

# Pseudo
sum(duplicated(df$MimosaID)) # 95
# Previously 77
df[duplicated(df$MimosaID), 'MimosaID'] %>% print(n = 95)
# Note, they are all NA, as expected

# Data types
str(df)
# MigrationDuration     chr --> needs to be converted
# TimeToReceiveSupport  chr --> needs to be converted
# SupportDuration       num
# TrainingDuration      num

# Check levels before
df %>% group_by(MigrationDuration) %>% summarise(count = n()) %>% print(n = 31) # --> all are numbers
df %>% group_by(TimeToReceiveSupport) %>% summarise(count = n()) %>% print(n = 116) # --> not all are numbers;
# 35 answsers are not numbers, out of which most (29) are 'je ne me souviens pas'. These can be safely
# coerced to NA, since we will then replace NA with the median. One observation, which is 16o (letter o
# instead of number 0), is manually changed here:
df[!is.na(df$TimeToReceiveSupport) & df$TimeToReceiveSupport == '16o', "TimeToReceiveSupport"]  <- "160"


# Converting to numeric could produce NA
# Print before
colSums(is.na(df))
# MigrationDuration     110
# TimeToReceiveSupport  169


# Character to Numeric
df$MigrationDuration <- as.numeric(df$MigrationDuration)
df$TimeToReceiveSupport <- as.numeric(df$TimeToReceiveSupport)

# Print after
colSums(is.na(df))
# MigrationDuration     110
# TimeToReceiveSupport  203 (34 NA introduced by coercion, which corresponds to no number from above;
# this can therefore be safely ignored)


# Check levels after
df %>% group_by(MigrationDuration) %>% summarise(count = n()) %>% print(n = 31) # --> we see, e.g.,
# that count of 3 is 252, which are the 251 3 plus 1 03 from above
df %>% group_by(TimeToReceiveSupport) %>% summarise(count = n()) %>% print(n = 116) # we see, e.g.,
# that count of 160 is now 4, which are the three cases from above plus the 160 which manually corrected


# Recode dependent variables

# Print levels
levels(as.factor(df$BusinessSucess))
levels(as.factor(df$BusinessProfitability))
levels(as.factor(df$WouldMigrateAgain))

# Print counts
df %>% group_by(BusinessSucess) %>% summarise(count = n()) %>%
  mutate(percent = count/sum(count)*100) %>% arrange(-percent)
df %>% group_by(BusinessProfitability) %>% summarise(count = n()) %>%
  mutate(percent = count/sum(count)*100) %>% arrange(-percent)
df %>% group_by(WouldMigrateAgain) %>% summarise(count = n()) %>%
  mutate(percent = count/sum(count)*100) %>% arrange(-percent)

df <- df %>% mutate(
  BusinessSucess =
    case_when(BusinessSucess == 'Actuellement ouvert et les activités marchent bien' ~ 'High',
              BusinessSucess != 'Actuellement ouvert et les activités marchent bien' ~ 'Low'),
  BusinessProfitability =
    case_when(BusinessProfitability == 'Oui' ~ 'High',
              BusinessProfitability != 'Oui' ~ 'Low'),
  WouldMigrateAgain = 
    case_when(WouldMigrateAgain == 'Non' ~ 'No',
              WouldMigrateAgain != 'Non' ~ 'Yes')
)

# Reprint counts
df %>% group_by(BusinessSucess) %>% summarise(count = n()) %>%
  mutate(percent = count/sum(count)*100) %>% arrange(-percent)
df %>% group_by(BusinessProfitability) %>% summarise(count = n()) %>%
  mutate(percent = count/sum(count)*100) %>% arrange(-percent)
df %>% group_by(WouldMigrateAgain) %>% summarise(count = n()) %>%
  mutate(percent = count/sum(count)*100) %>% arrange(-percent)


# Assess NA
colSums(is.na(df))


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

colSums(is.na(df))

# Before
df %>% group_by(FirstChoice) %>% summarise(count = n()) %>% 
  mutate(percent = count/sum(count)*100) %>% arrange(-percent)

df <- df %>% mutate(
  
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
  
  TrainingType =
    case_when(
      TrainingType == "Business development/management training" ~ "Business/management",
      TrainingType != "Business development/management training" ~ "Other/Unknown" # NA
      # purposefully not specified to keep them as is
    ),
  
  BusinessType = 
    case_when(
      BusinessType == "Commerce" ~ "Commerce",
      BusinessType == "Elevage" ~ "Elevage",
      BusinessType == "Transport (Moto - Auto)" ~ "Transport",
      
      BusinessType == "Agriculture" ~ "Agriculture/aviculture",
      BusinessType == "Aviculture" ~ "Agriculture/aviculture",
      
      BusinessType == "Artisan-Ouvrier" ~ "Autre",
      BusinessType == "Couture" ~ "Autre",
      BusinessType == "Autre" ~ "Autre",
      BusinessType == "Restauration" ~ "Autre",
      BusinessType == "Bâtiment/construction" ~ "Autre",
      BusinessType == "Coiffure - Salon de beauté" ~ "Autre",
      BusinessType == "Mécanique" ~ "Autre"
    ),
  
  BusinessMembers = 
    case_when(BusinessMembers != "Moi uniquement" ~ "Moi et d'autres",
              TRUE ~ as.character(BusinessMembers)
    ),
  
  MicroBusinessLevel =
    case_when(MicroBusinessLevel != "Individual" ~ "Not individual",
              TRUE ~ as.character(MicroBusinessLevel)
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
      # and Refused
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
  
  mutate(MicroBusinessLevel = replace_na(MicroBusinessLevel, "Unknown"), # to
         # avoid losing 336 obs
         BusinessHasEmployees = replace_na(BusinessHasEmployees, "Autre"),
         EmployeeNumber = replace_na(EmployeeNumber, "0") # All NA are for respondents
         # who do not have employees, so we can replace NA with 0
         )


# After
df %>% group_by(FirstChoice) %>% summarise(count = n()) %>% 
  mutate(percent = count/sum(count)*100) %>% arrange(-percent)










# Outliers; fill NA in numeric variables

# MigrationDuration
# Definition: "Durée de l’absence du pays d’origine   Mettre 0 si moins d'un an"
# In years
# NA
sum(is.na(df$MigrationDuration)) # 110

# Summarise
summary(df$MigrationDuration)
# Check no values are under zero
df[df$MigrationDuration <= 0, "MigrationDuration"] %>% arrange(MigrationDuration) %>% 
  print(n=541)# 541, with 431 zero and 110 NA

# First, we'll replace these 3 values, which are mistakes and not outliers,
# with NA
df[order(df$MigrationDuration, decreasing=TRUE), 'MigrationDuration'][0:3,]
df[df$MigrationDuration >= 936 & !is.na(df$MigrationDuration), 'MigrationDuration']  <- NA
sum(is.na(df$MigrationDuration)) # 113 --> as expected
# Let's re-summarise
summary(df$MigrationDuration)
# And let's store the median
q_median <- median(df$MigrationDuration, na.rm = TRUE)
q_median # 2 years


# Make a boxplot (IQR)

# Boxplot has a function to detect outliers
outliers <- boxplot.stats(df$MigrationDuration)$out
out_ind <- which(df$MigrationDuration %in% c(outliers))
out_ind

# Print outliers
df[out_ind, "MigrationDuration"] %>% arrange(MigrationDuration) # 130, with
# smallest being 7 years

# Plot outliers
boxplot(df$MigrationDuration,
        ylab = "Years",
        main = "MigrationDuration"
)

# Spot outliers using percentile method, with conservative threshold of 0.01/0.99
lower_bound <- quantile(df$MigrationDuration, 0.01, na.rm = TRUE)
upper_bound <- quantile(df$MigrationDuration, 0.99, na.rm = TRUE)
outlier_ind <- which(df$MigrationDuration < lower_bound | df$MigrationDuration > upper_bound)
df[outlier_ind, "MigrationDuration"] %>% arrange(MigrationDuration) # 18, with
# smallest being 15 years


# Replace 18 extreme outliers with median
df[outlier_ind, "MigrationDuration"] <- q_median

# Re-summarise
summary(df$MigrationDuration) # median still 2, max 14 years as expected
# Replot
outliers <- boxplot.stats(df$MigrationDuration)$out
out_ind <- which(df$MigrationDuration %in% c(outliers))
boxplot(df$MigrationDuration,
        ylab = "Years",
        main = "MigrationDuration"
)
# It is looking better
# We should still have the same number of NA
sum(is.na(df$MigrationDuration)) # 113 --> as expected
# And we will also replace them with the median
df[is.na(df$MigrationDuration), "MigrationDuration"] <- q_median
# Final summary and plot
summary(df$MigrationDuration) # median still 2, max still 14 years
outliers <- boxplot.stats(df$MigrationDuration)$out
out_ind <- which(df$MigrationDuration %in% c(outliers))
boxplot(df$MigrationDuration,
        ylab = "Years",
        main = "MigrationDuration"
)
# As above
# Check no NA are remaining
sum(is.na(df$MigrationDuration)) # 0, all good





# TimeToReceiveSupport
# Definition: "Combien de temps entre votre retour et la réception de l’aide à
# la réintégration (ou sa première fourniture) ? En semaines"
# In weeks
# NA
sum(is.na(df$TimeToReceiveSupport)) # 203

# Summarise
summary(df$TimeToReceiveSupport)

# Check no value is under 0
df[df$TimeToReceiveSupport <= 0, "TimeToReceiveSupport"] %>% arrange(TimeToReceiveSupport) %>% 
  print(n=213)# 213, with 10 zero and 203 NA


# Let's store the median
q_median <- median(df$TimeToReceiveSupport, na.rm = TRUE)
q_median # 16 weeks

# Make a boxplot (IQR)

# Boxplot has a function to detect outliers
outliers <- boxplot.stats(df$TimeToReceiveSupport)$out
out_ind <- which(df$TimeToReceiveSupport %in% c(outliers))
out_ind

# Print outliers
df[out_ind, "TimeToReceiveSupport"] %>% arrange(TimeToReceiveSupport) # 214, with
# smallest being 80 weeks

# Plot outliers
boxplot(df$TimeToReceiveSupport,
        ylab = "Weeks",
        main = "TimeToReceiveSupport"
)

# Spot outliers using percentile method, with conservative threshold of 0.01/0.99
lower_bound <- quantile(df$TimeToReceiveSupport, 0.01, na.rm = TRUE)
upper_bound <- quantile(df$TimeToReceiveSupport, 0.99, na.rm = TRUE)
outlier_ind <- which(df$TimeToReceiveSupport < lower_bound | df$TimeToReceiveSupport > upper_bound)
df[outlier_ind, "TimeToReceiveSupport"] %>% arrange(TimeToReceiveSupport) %>% print(n=34) # 34, with
# smallest being 0 weeks (lower) or 180 (upper)


# Replace 34 extreme outliers with median
df[outlier_ind, "TimeToReceiveSupport"] <- q_median

# Re-summarise
summary(df$TimeToReceiveSupport) # median still 16, max 176 weeks as expected
# Replot
outliers <- boxplot.stats(df$TimeToReceiveSupport)$out
out_ind <- which(df$TimeToReceiveSupport %in% c(outliers))
boxplot(df$TimeToReceiveSupport,
        ylab = "Weeks",
        main = "TimeToReceiveSupport"
)
# It is looking better
# We should still have the same number of NA
sum(is.na(df$TimeToReceiveSupport)) # 203 --> as expected
# And we will also replace them with the median
df[is.na(df$TimeToReceiveSupport), "TimeToReceiveSupport"] <- q_median
# Final summary and plot
summary(df$TimeToReceiveSupport) # median still 16, max still 176 weeks years
outliers <- boxplot.stats(df$TimeToReceiveSupport)$out
out_ind <- which(df$TimeToReceiveSupport %in% c(outliers))
boxplot(df$TimeToReceiveSupport,
        ylab = "Weeks",
        main = "TimeToReceiveSupport"
)
# As above
# Check no NA are remaining
sum(is.na(df$TimeToReceiveSupport)) # 0, all good






# SupportDuration OUTSTANDING
# Definition: SupportDuration = as.numeric(difftime(InterviewDate,
# ReintegrationDate, units = "days"
# In days
# NA
sum(is.na(df$SupportDuration)) # 259

# Summarise
summary(df$SupportDuration)
# Show smallest numbers, since some are negative
df[df$SupportDuration <= 0, "SupportDuration"] %>% arrange(SupportDuration) # 332! Ask Julie

# Continue here

# Let's store the median
q_median <- median(df$SupportDuration, na.rm = TRUE)
q_median # 113 days


# Make a boxplot (IQR)

# Boxplot has a function to detect outliers
outliers <- boxplot.stats(df$SupportDuration)$out
out_ind <- which(df$SupportDuration %in% c(outliers))
out_ind

# Print outliers
df[out_ind, "SupportDuration"] %>% arrange(SupportDuration) # 209, with
# smallest being -270 days (lower) or 540 days (upper)

# Plot outliers
boxplot(df$SupportDuration,
        ylab = "Days",
        main = "SupportDuration"
)

# Spot outliers using percentile method, with conservative threshold of 0.01/0.99
lower_bound <- quantile(df$SupportDuration, 0.01, na.rm = TRUE)
upper_bound <- quantile(df$SupportDuration, 0.99, na.rm = TRUE)
outlier_ind <- which(df$SupportDuration < lower_bound | df$SupportDuration > upper_bound)
df[outlier_ind, "SupportDuration"] %>% arrange(SupportDuration) %>% print(n=34) # 34, with
# smallest being 0 weeks (lower) or 180 (upper)


# Replace 34 extreme outliers with median
df[outlier_ind, "SupportDuration"] <- q_median

# Re-summarise
summary(df$SupportDuration) # median still 16, max 176 weeks as expected
# Replot
outliers <- boxplot.stats(df$SupportDuration)$out
out_ind <- which(df$SupportDuration %in% c(outliers))
boxplot(df$SupportDuration,
        ylab = "Days",
        main = "SupportDuration"
)
# It is looking better
# We should still have the same number of NA
sum(is.na(df$SupportDuration)) # 206 --> as expected
# And we will also replace them with the median
df[is.na(df$SupportDuration), "SupportDuration"] <- q_median
# Final summary and plot
summary(df$SupportDuration) # median still 16, max still 176 weeks years
outliers <- boxplot.stats(df$SupportDuration)$out
out_ind <- which(df$SupportDuration %in% c(outliers))
boxplot(df$SupportDuration,
        ylab = "Days",
        main = "SupportDuration"
)
# As above
# Check no NA are remaining
sum(is.na(df$SupportDuration)) # 0, all good











# TrainingDuration
# Definition: TrainingDuration = as.numeric(difftime(TrainingEnd, TrainingStart,
# units = "days")
# In days
# NA
sum(is.na(df$TrainingDuration)) # 1378
# Previously 1347 (difference 31)

# Since not everybody received training, for this variable, we need to subset
# on non-NA rows only. That is, this subset
df[!is.na(df$TrainingDuration), ] # 570
# Previously 610 (difference 40)

# We can also check this variable
# df %>% group_by(TrainingType) %>% summarise(count = n()) # run only if needed
# It has 3 more NA, so it is best to choose the other one

# Summarise
summary(df[!is.na(df$TrainingDuration), "TrainingDuration"])
# Show smallest numbers, since some are negative
df[!is.na(df$TrainingDuration) & df$TrainingDuration <= 0, "TrainingDuration"] %>% arrange(TrainingDuration) %>% print(n = 10) # 168 zero


# Let's store the median
q_median <- median(df$TrainingDuration, na.rm = TRUE) # No need to subset here, since NA
# are removed with dedicated arguments
q_median # 3 days


# Make a boxplot (IQR)

# Boxplot has a function to detect outliers
outliers <- boxplot.stats(df$TrainingDuration)$out
out_ind <- which(df$TrainingDuration %in% c(outliers))
out_ind

# Print outliers
df[out_ind, "TrainingDuration"] %>% arrange(TrainingDuration) # 29, with
# smallest being 13 days 

# Plot outliers
boxplot(df$TrainingDuration,
        ylab = "Days",
        main = "TrainingDuration"
)

# Spot outliers using percentile method, with conservative threshold of 0.01/0.99
lower_bound <- quantile(df$TrainingDuration, 0.01, na.rm = TRUE)
upper_bound <- quantile(df$TrainingDuration, 0.99, na.rm = TRUE)
outlier_ind <- which(df$TrainingDuration < lower_bound | df$TrainingDuration > upper_bound)
df[outlier_ind, "TrainingDuration"] %>% arrange(TrainingDuration) %>% print(n=6) # 6, with
# smallest being 105


# Replace 6 extreme outliers with median
df[outlier_ind, "TrainingDuration"] <- q_median

# Re-summarise
summary(df$TrainingDuration) # median still 3, max 86 weeks as expected
# Replot
outliers <- boxplot.stats(df$TrainingDuration)$out
out_ind <- which(df$TrainingDuration %in% c(outliers))
boxplot(df$TrainingDuration,
        ylab = "Days",
        main = "TrainingDuration"
)
# It is looking better
# We should still have the same number of NA
sum(is.na(df$TrainingDuration)) # 1378 --> as expected

# But contrary to other variables, we do NOT replace them



# Ask Julie if the above makes sense




 

# Assess NA
colSums(is.na(df))







# NOTES

# #"AssistanceType" or "Quel est le principal type d’assistance économique que
# vous avez reçue ?", was completely removed since counts are as follows:
#AssistanceType     count  percent
#1 Micro Business    1946 99.9   
#2 Autre                1  0.0513
#3 Placement emploi     1  0.0513
# That said, it might be good to say in the report that nearly all assistance
# received was for micro business.









# Export
#write_excel_csv(df, 'data_clean/res_slim.csv') # using extension .xls will avoid
# wrapping, but will produce unsafe warning, so we use .csv
# RDS version
#saveRDS(df, file = 'data_clean/res_slim.rds')




