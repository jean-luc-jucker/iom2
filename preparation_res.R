getwd()
library(readxl)
library(tidyverse)

# Reintegration Economic Survey ####
res <- read_excel('data_raw/Reintegration Economic_clean 6.6.23.xlsx',
                  na = c('N/A', 'NA', 'na')) 
warnings()
dim(res) # 2,012 x 154

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
    "Quel est le principal type d’assistance économique que vous avez reçue ?",
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
    "AssistanceType" =
      "Quel est le principal type d’assistance économique que vous avez reçue ?",
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

dim(df) # 1959 x 33
#view(df)

# NA
colSums(is.na(df))

# Dupes

# Perfect
sum(duplicated(df)) # 2
df[duplicated(df) | duplicated(df, fromLast = TRUE), ]
# Remove
df  <- distinct(df)
dim(df) # 1957 x 33

# Pseudo
sum(duplicated(df$MimosaID)) # 77
df[duplicated(df$MimosaID), 'MimosaID']

#write.csv(df[duplicated(df$MimosaID), 'MimosaID'], 'data_clean/res_pseudo_dupes.csv')

# Data types
str(df)

# Character to Numeric
df$MigrationDuration <- as.numeric(df$MigrationDuration)
df$TimeToReceiveSupport <- as.numeric(df$TimeToReceiveSupport)
df$SupportDuration  <- as.numeric(df$SupportDuration)


# Remove non-SL countries!!! (in pipe) Togo, Sierra Leone, Liberia


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




# Outliers; fill NA in numeric variables

# MigrationDuration
# Definition: "Durée de l’absence du pays d’origine   Mettre 0 si moins d'un an"
# In years
# NA
sum(is.na(df$MigrationDuration)) # 111

# Summarise
summary(df$MigrationDuration)
# Check no observations are below or 0
df[df$MigrationDuration <= 0, "MigrationDuration"] %>% arrange(MigrationDuration) # 543! ask Julie


# First, we'll replace these 3 values, which are mistakes and not outliers,
# with NA
df[order(df$MigrationDuration, decreasing=TRUE), 'MigrationDuration'][0:3,]
df[df$MigrationDuration >= 936 & !is.na(df$MigrationDuration), 'MigrationDuration']  <- NA
sum(is.na(df$MigrationDuration)) # 114 --> as expected
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
sum(is.na(df$MigrationDuration)) # 114 --> as expected
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
sum(is.na(df$TimeToReceiveSupport)) # 206

# Summarise
summary(df$TimeToReceiveSupport)
# Check no observations are below or 0
df[df$TimeToReceiveSupport <= 0, "TimeToReceiveSupport"] %>% arrange(TimeToReceiveSupport) # 216! ask Julie


# Let's store the median
q_median <- median(df$TimeToReceiveSupport, na.rm = TRUE)
q_median # 16 weeks


# Make a boxplot (IQR)

# Boxplot has a function to detect outliers
outliers <- boxplot.stats(df$TimeToReceiveSupport)$out
out_ind <- which(df$TimeToReceiveSupport %in% c(outliers))
out_ind

# Print outliers
df[out_ind, "TimeToReceiveSupport"] %>% arrange(TimeToReceiveSupport) # 215, with
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
sum(is.na(df$TimeToReceiveSupport)) # 206 --> as expected
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









# SupportDuration
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












# Export
#write_excel_csv(df, 'data_clean/res_slim.csv') # using extension .xls will avoid
# wrapping, but will produce unsafe warning, so we use .csv
# RDS version
#saveRDS(df, file = 'data_clean/res_slim.rds')




