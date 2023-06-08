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
    "Combien de temps entre votre retour et la réception de l’aide à la réintégration (ou sa première fourniture) ? En semaines",
    "Par quel moyen avez-vous reçu cette assistance économique ?",
    "Quel est le principal type d’assistance économique que vous avez reçue ?",
    "Type de business bis",
    "Qui sont les membres de cette entreprise ?",
    "Niveau microbusiness",
    "Quelle est la valeur totale de votre aide ?",
    "L'OIM   ou un de ses partenaires vous a-t-elle formé sur la façon de gérer une entreprise ?",
    "L’entreprise emploie-t-elle du personnel ?",
    "Si oui, combien des personnes sont employées par votre entreprise ?",
    "Est-ce votre entreprise a été affectée par la maladie de Coronavirus ?"
    
    ) %>% 
  rename(
    # Metadata
    "MimosaID" =
      "Identifiant MiMOSA du cas bis", 
    "Project" =
      "Projet Bis",
    "InterviewDate" =
      "Date de l'enquête",
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
    "SupportTotalValue" = 
      "Quelle est la valeur totale de votre aide ?",
    "ReceivedIOMBusinessAdvice" =
      "L'OIM   ou un de ses partenaires vous a-t-elle formé sur la façon de gérer une entreprise ?",
    "BusinessHasEmployees" =
      "L’entreprise emploie-t-elle du personnel ?",
    "EmployeeNumber" =
      "Si oui, combien des personnes sont employées par votre entreprise ?",
    "CoronaImpactOnBusiness" =
      "Est-ce votre entreprise a été affectée par la maladie de Coronavirus ?"
    
         )

df

# NA
colSums(is.na(df))






























