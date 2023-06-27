# Main change from v3: we start data preparation for regression

getwd()
library(readxl)
library(tidyverse)


# LOAD DATA ####

# Mimosa data (mimosa)
mimosa <- read_excel('data_raw/Reintegration Cases M&E RSS June 2023 (liste complète).xlsx',
                     na = c('N/A', 'NA', 'na', 'NULL'))
dim(mimosa) # 2,580 x 57

# Reintegration Economic Survey (kobo) ####
kobo <- read_excel('data_raw/RSS data cleaned to use for analysis V3 doublons corriges.xlsx',
                   na = c('N/A', 'NA', 'na')) 
dim(kobo) # 1,386 x 96


# 1. MIMOSA ####

# Check perfect duplicates
sum(duplicated(mimosa)) # 0
# Print names and types
names(mimosa)
str(mimosa)

# Subset
mimosa_slim <- mimosa %>% select(
  # Meta
  "CaseNo/IndividualNo",
  "ArrivalDate",
  # Assistance
  "CounsellingStatus",
  "EconomicSupport",
  "FinancialServices",
  "JobPlacement",
  "Microbusiness",
  "Training",
  "TrainingStartDate",
  "TrainingEndDate",
  "SocialSupport",
  "ChildCare",
  "Education",
  "Housing",
  "LegalServices",
  "MaterialAssistance",
  "MedicalSupport",
  "SocialProtectionSchemes",
  "PsychosocialSupport",
  # Vulnerability
  "VOTs",
  "UMINOR",
  "HealthCondition",
  # Microbusiness
  "Microbusinesslevel",
  "MicrobusinessDeliveredBy",
  "MicrobusinessFormOfAssistance",
  "MicrobusinessEndDate"
  ) %>% 
  # Computed variables
  mutate(TrainingDuration = 
           as.numeric(difftime(TrainingEndDate, TrainingStartDate, units = "days")),
         MBSupportDuration = 
           as.numeric(difftime(MicrobusinessEndDate, ArrivalDate, units = "days"))
           ) %>% 
  # Rename ID for future merge
  rename("ID" = 
         "CaseNo/IndividualNo")
# Final properties
dim(mimosa_slim) # 2,580 x 28
#mimosa_slim
#view(mimosa_slim)


# 2. KOBO ####

# Check perfect duplicates
sum(duplicated(kobo)) # 1
# Remove 1 duplicate
kobo <- kobo %>% distinct()
# Print new size
dim(kobo) # 1,385 x 96, as expected
# Recheck duplicates
sum(duplicated(kobo)) # now 0
# Print names
names(kobo)
# Print types
str(kobo)


# Subset ######################################################################
kobo_slim <- kobo %>% 
  
  # PULL ######################################################################
select(
  # Metadata
  "Code Mimosa Corrigé",
  "Date d'enquete kobo",
  # Grouping variables
  "Sexe",
  "Age",
  "Durée de l’absence du pays d’origine   (Mettre 0 si moins d'un an, sinon mettre le nombre d'année)",
  "Pays de retour",
  "Mission OIM",
  
  # Dependent variables
  # Economic
  "1. Dans quelle mesure êtes-vous satisfait de votre situation économique actuelle?", # Read Note 1!
  "2. A quelle fréquence avez-vous dû réduire la quantité ou la qualité des aliments que vous mangez en raison de leur coût au cours du mois passe (des 30 derniers jours)?",
  "3. Avez vous la possibilité emprunter de l’argent si vous en avez besoin?\r\n(Perception de la disponibilité du crédit, quelle que soit la source - banque, famille, amis, système de prêts traditionnel, microcrédit, etc. - et peu importe si le répondant prend effectivement des prêts ou non)",
  "4. Empruntez-vous de l’argent? À quelle fréquence?\r\n(Comportement autodéclaré par le répondant, peu importe la source du crédit et le montant – même les très petits montants comptent)",
  "5. En moyenne, quel est le montant le plus élevé : vos dépenses chaque mois ou votre dette",
  "6. Comment évalueriez-vous votre accès aux possibilités (emploi et formation)?",
  "7. Travaillez-vous actuellement?\r\n(Emploi formel ou informel; travail indépendant; propre entreprise ou exploitation agricole. Si l’intimé suit actuellement une formation non rémunérée ou fréquente l’école, sélectionnez « Sans objet ».)",
  "8. Possédez-vous l’un des actifs productifs suivants?",
  "9. Êtes-vous actuellement à la recherche d’un emploi?",
  # Social
  "11. Comment évalueriez-vous votre accès au logement dans votre collectivité?", # Read Note 4!
  "12. Comment évalueriez-vous la condition du logement dans lequelle vous vivez aujourd’hui?",
  "13. Comment évalueriez-vous l’accès à l’éducation dans votre coommunauté?",
  "14. Tous les enfants d’âge scolaire de votre ménage fréquentent-ils actuellement l’école?\r\n(Cela comprend les enfants dont le répondant est un parent ou un tuteur, ainsi que les autres enfants du ménage des répondants.)",
  "15. Comment évalueriez-vous l’accès à la justice et à l’application de la loi dans votre collectivité?\r\n(tribunaux, police, armée, etc.)",
  "16. Avez-vous au moins une pièce d’identité?\r\n(passeport, document d’identification national ou local, certificat de naissance, etc.)",
  "17. Comment évalueriez-vous l’accès aux documents (pièce d’identité personnelle, certificats de naissance, etc.) dans votre collectivité?",
  "18. Comment évalueriez-vous l’accès à l’eau potable dans votre collectivité?",
  "19. Comment évalueriez-vous l’accès aux soins de santé dans votre collectivité?",
  "20. Quelle est la qualité des services de santé auquels vous avez accès?",
  # Psycho social
  "22. À quelle fréquence êtes-vous invité ou participez-vous à des activités sociales (célébrations, mariages, autres événements) au sein de votre communauté?", # Read Note 6!
  "23. Que pensez-vous de votre réseau de soutien? Pouvez-vous compter sur le soutien du réseau?\r\n(Réseau de soutien qui peut fournir une aide émotionnelle ou pratique en cas de besoin, peu importe le type factuel, la taille ou la force du soutien)",
  "24. Avez-vous l’impression de faire partie de la collectivité où vous vivez actuellement?",
  "25. Dans quelle mesure vous sentez-vous en sécurité physiquement pour vous-même et votre famille pendant vos activités quotidiennes à l’extérieur? \r\n(Perception de la sécurité physique contre la violence et la persécution et/ou d’autres formes d’insécurité. Peut être lié à l’appartenance à un groupe social ou au statut de rapatrié seul.)",
  "26. À quelle fréquence avez-vous connu des tensions ou des conflits importants entre vous et votre famille depuis votre retour?",
  "27. Vous êtes-vous senti victime de discrimination depuis votre retour?\r\nDéfinition: la discrimination implique l’impossibilité de jouir de droits et de libertés sans distinction aucune, telle que la race, la couleur, le sexe, la langue, la religion, l’opinion politique ou toute autre opinion, l’origine nationale ou sociale, la propriété, la naissance ou toute autre situation.",
  "28. Souffrez-vous souvent de l’un des éléments suivants? \r\n- Se sentir en colère \r\n- Se sentir triste \r\n- Avoir peur \r\n- Se sentir stressé \r\n- Se sentir seul \r\n- Se sentir faible estime de soi \r\n- Difficulté à se concentrer",
  "29. Souhaitez-vous recevoir un soutien psychologique spécialisé",
  "30. Sentez-vous que vous êtes en mesure de rester et de vivre dans ce pays?",
  "31. Qu’est-ce qui vous fait ressentir cela?"
  
) %>% 
  
  # RENAME ####################################################################
rename(
  # Metadata
  "ID" = 
    "Code Mimosa Corrigé",
  "interview_date" = 
    "Date d'enquete kobo",
  # Grouping variables
  "sex" =
    "Sexe",
  "age" = 
    "Age",
  "migration_duration" =
    "Durée de l’absence du pays d’origine   (Mettre 0 si moins d'un an, sinon mettre le nombre d'année)",
  "return_country" =
    "Pays de retour",
  "origin_country" = "Mission OIM",
  
  # Dependent variables
  # Economic
  "1_economic" =
    "1. Dans quelle mesure êtes-vous satisfait de votre situation économique actuelle?", # Read Note 1!
  "2_food" =
    "2. A quelle fréquence avez-vous dû réduire la quantité ou la qualité des aliments que vous mangez en raison de leur coût au cours du mois passe (des 30 derniers jours)?",
  "3_borrow" =
    "3. Avez vous la possibilité emprunter de l’argent si vous en avez besoin?\r\n(Perception de la disponibilité du crédit, quelle que soit la source - banque, famille, amis, système de prêts traditionnel, microcrédit, etc. - et peu importe si le répondant prend effectivement des prêts ou non)",
  "4_borrow_freq" = 
    "4. Empruntez-vous de l’argent? À quelle fréquence?\r\n(Comportement autodéclaré par le répondant, peu importe la source du crédit et le montant – même les très petits montants comptent)",
  "5_debt_ratio" = 
    "5. En moyenne, quel est le montant le plus élevé : vos dépenses chaque mois ou votre dette",
  "6_employment" =
    "6. Comment évalueriez-vous votre accès aux possibilités (emploi et formation)?",
  "7_working" = 
    "7. Travaillez-vous actuellement?\r\n(Emploi formel ou informel; travail indépendant; propre entreprise ou exploitation agricole. Si l’intimé suit actuellement une formation non rémunérée ou fréquente l’école, sélectionnez « Sans objet ».)",
  "8_assets" = 
    "8. Possédez-vous l’un des actifs productifs suivants?",
  "9_searching_job" = 
    "9. Êtes-vous actuellement à la recherche d’un emploi?",
  # Social
  "10_housing" = # Read Note 4!
    "11. Comment évalueriez-vous votre accès au logement dans votre collectivité?",
  "11_housing_qual" = 
    "12. Comment évalueriez-vous la condition du logement dans lequelle vous vivez aujourd’hui?",
  "12_education" = 
    "13. Comment évalueriez-vous l’accès à l’éducation dans votre coommunauté?",
  "13_school" = 
    "14. Tous les enfants d’âge scolaire de votre ménage fréquentent-ils actuellement l’école?\r\n(Cela comprend les enfants dont le répondant est un parent ou un tuteur, ainsi que les autres enfants du ménage des répondants.)",
  "14_justice" = 
    "15. Comment évalueriez-vous l’accès à la justice et à l’application de la loi dans votre collectivité?\r\n(tribunaux, police, armée, etc.)",
  "15_id" = 
    "16. Avez-vous au moins une pièce d’identité?\r\n(passeport, document d’identification national ou local, certificat de naissance, etc.)",
  "16_documentation" = 
    "17. Comment évalueriez-vous l’accès aux documents (pièce d’identité personnelle, certificats de naissance, etc.) dans votre collectivité?",
  "17_water" = 
    "18. Comment évalueriez-vous l’accès à l’eau potable dans votre collectivité?",
  "18_health" = 
    "19. Comment évalueriez-vous l’accès aux soins de santé dans votre collectivité?",
  "19_health_qual" = 
    "20. Quelle est la qualité des services de santé auquels vous avez accès?",
  # Psycho social
  "21_socialize" = 
    "22. À quelle fréquence êtes-vous invité ou participez-vous à des activités sociales (célébrations, mariages, autres événements) au sein de votre communauté?",
  "22_network" = 
    "23. Que pensez-vous de votre réseau de soutien? Pouvez-vous compter sur le soutien du réseau?\r\n(Réseau de soutien qui peut fournir une aide émotionnelle ou pratique en cas de besoin, peu importe le type factuel, la taille ou la force du soutien)",
  "23_community" = 
    "24. Avez-vous l’impression de faire partie de la collectivité où vous vivez actuellement?",
  "24_safety" = 
    "25. Dans quelle mesure vous sentez-vous en sécurité physiquement pour vous-même et votre famille pendant vos activités quotidiennes à l’extérieur? \r\n(Perception de la sécurité physique contre la violence et la persécution et/ou d’autres formes d’insécurité. Peut être lié à l’appartenance à un groupe social ou au statut de rapatrié seul.)",
  "25_family_conflict" = 
    "26. À quelle fréquence avez-vous connu des tensions ou des conflits importants entre vous et votre famille depuis votre retour?",
  "26_discrimination" = 
    "27. Vous êtes-vous senti victime de discrimination depuis votre retour?\r\nDéfinition: la discrimination implique l’impossibilité de jouir de droits et de libertés sans distinction aucune, telle que la race, la couleur, le sexe, la langue, la religion, l’opinion politique ou toute autre opinion, l’origine nationale ou sociale, la propriété, la naissance ou toute autre situation.",
  "27_distress" = 
    "28. Souffrez-vous souvent de l’un des éléments suivants? \r\n- Se sentir en colère \r\n- Se sentir triste \r\n- Avoir peur \r\n- Se sentir stressé \r\n- Se sentir seul \r\n- Se sentir faible estime de soi \r\n- Difficulté à se concentrer",
  "28_psy_support" = 
    "29. Souhaitez-vous recevoir un soutien psychologique spécialisé",
  
  
  "29_able_to_stay" =
  "30. Sentez-vous que vous êtes en mesure de rester et de vivre dans ce pays?",
  
  "30_wish_vs_need_leave" =
  "31. Qu’est-ce qui vous fait ressentir cela?"
  
) %>% 
  
  # CODE TO NUMERIC ###########################################################
# Economic
mutate(`1_economic_n` =
         recode(`1_economic`,
                "Très satisfait" = 1,
                "Satisafait" = 0.75,
                "Neutre" = 0.5,
                "Insatisfait" = 0.25,
                "Très insatisafait" = 0,
                "Je préfére ne pas répondre" = 0.5
         ),
       `2_food_n` =
         recode(`2_food`,
                "Jamais" = 1,
                "Rarement( Une fois le mois ou pas tous les mois)" = 0.75,
                "Des fois ( 2 ou 3 fois le mois)" = 0.5,
                "Souvent( au moins une fois la semaine)" = 0.25,
                "Très souvent ( plusieurs fois dans la semaine/chaque jours)" = 0,
                "Je souhaite ne pas répondre" = 0.5
         ),
       `3_borrow_n` = 
         recode(`3_borrow`,
                "Oui" = 1,
                "Non" = 0,
                "Je ne sait pas" = 0.5,
                "je ne souhaite pas répondre" = 0.5
         ),
       `4_borrow_freq_n` =
         recode(`4_borrow_freq`,
                "Jamais" = 1,
                "Rarement( Une fois le mois ou pas tous les mois)" = 0.75,
                "Des fois ( 2 ou 3 fois le mois)" = 0.5,
                "Souvent( au moins une fois la semaine)" = 0.25,
                "Très souvent ( plusieurs fois dans la semaine/chaque jours)" = 0,
                "Je souhaite ne pas répondre" = 0.5
         ),
       `5_debt_ratio_n` =
         recode(`5_debt_ratio`, # Read Note 2!
                "Les dépenses sont plus importantes" = 1,
                "La dette est plus grande" = 0,
                "Je souhaite ne pas répondre" = 0.5
         ),
       `6_employment_n` =
         recode(`6_employment`,
                "Très bien" = 1,
                "Bien" = 0.75,
                "Acceptable" = 0.5,
                "Faible" = 0.25,
                "Très faible" = 0,
                "Je ne sais pas" = 0.5,
                "je ne souhaite pas répondre" = 0.5
         ),
       `7_working_n` =
         recode(`7_working`,
                "Oui" = 1,
                "Non" = 0,
                "Je ne sait pas" = 0.5,
                "je ne souhaite pas répondre" = 0.5,
                "Pas applicable" = 0.5
         ),
       `8_assets_n` =
         recode(`8_assets`, # Read Note 3! Warning can be safely ignored
                "Aucun actif détenu" = 0,
                "Je ne sais pas" = 0.5,
                "Je souhaite ne pas répondre" = 0.5
         ),
       
       `9_searching_job_n` =
         recode(`9_searching_job`,
                "Non" = 1,
                "Oui" = 0,
                "Je ne sait pas" = 0.5
         ),
       # Social
       `10_housing_n` =
         recode(`10_housing`,
                "Très bien" = 1,
                "Bien" = 0.75,
                "Acceptable" = 0.5,
                "Faible" = 0.25,
                "Très faible" = 0,
                "Je ne sais pas" = 0.5
         ),
       `11_housing_qual_n` =
         recode(`11_housing_qual`,
                "Très bien" = 1,
                "Bien" = 0.75,
                "Acceptable" = 0.5,
                "Faible" = 0.25,
                "Très faible" = 0,
                "Je ne sais pas" = 0.5
         ),
       `12_education_n` = 
         recode(`12_education`,
                "Très bien" = 1,
                "Bien" = 0.75,
                "Acceptable" = 0.5,
                "Faible" = 0.25,
                "Très faible" = 0,
                "Je ne sais pas" = 0.5
         ),
       `13_school_n` = # Read Notes 5 and 10!
         recode(`13_school`,
                "Oui" = 1,
                "Non - quelques uns mais pas tous" = 0.5,
                "Aucun" = 0,
                "Non" = 0
         ),
       `14_justice_n` =
         recode(`14_justice`,
                "Très bien" = 1,
                "Bien" = 0.75,
                "Acceptable" = 0.5,
                "Faible" = 0.25,
                "Très faible" = 0,
                "Je ne sais pas" = 0.5,
                "je ne souhaite pas répondre" = 0.5
         ),
       `15_id_n` =
         recode(`15_id`,
                "Oui" = 1,
                "Non" = 0
         ),
       `16_documentation_n` =
         recode(`16_documentation`,
                "Très bien" = 1,
                "Bien" = 0.75,
                "Acceptable" = 0.5,
                "Faible" = 0.25,
                "Très faible" = 0,
                "Je ne sais pas" = 0.5,
                "je ne souhaite pas répondre" = 0.5
         ),
       `17_water_n` =
         recode(`17_water`,
                "Très bien" = 1,
                "Bien" = 0.75,
                "Acceptable" = 0.5,
                "Faible" = 0.25,
                "Très faible" = 0,
                "Je ne sais pas" = 0.5
         ),
       `18_health_n` =
         recode(`18_health`,
                "Très bien" = 1,
                "Bien" = 0.75,
                "Acceptable" = 0.5,
                "Faible" = 0.25,
                "Très faible" = 0,
                "Je ne sais pas" = 0.5,
                "je ne souhaite pas répondre" = 0.5
         ),
       `19_health_qual_n` =
         recode(`19_health_qual`,
                "Très bien" = 1,
                "Bien" = 0.75,
                "Acceptable" = 0.5,
                "Faible" = 0.25,
                "Très faible" = 0,
                "Je ne sais pas" = 0.5
         ),
       # Measure 20 is a construct
       `20_services_construct_n` = (`10_housing_n` + `12_education_n` + `14_justice_n` + `16_documentation_n` + `17_water_n` + `18_health_n`) / 6,
       # Psycho social
       `21_socialize_n` =
         recode(`21_socialize`,
                "Très souvent ( plusieurs fois dans la semaine/chaque jours)" = 1,
                "Souvent( au moins une fois la semaine)" = 0.75,
                "Des fois ( 2 ou 3 fois le mois)" = 0.5,
                "Rarement( Une fois le mois ou pas tous les mois)" = 0.25,
                "Jamais" = 0
         ),
       `22_network_n` =
         recode(`22_network`,
                "Tres bon - un très fort réseau" = 1,
                "Bien" = 0.75,
                "Correct" = 0.5,
                "Mauvais" = 0.25,
                "Très mauvais - un réseau très faible" = 0,
                "Je ne sais pas" = 0.5,
                "je ne souhaite pas répondre" = 0.5
         ),
       `23_community_n` =
         recode(`23_community`,
                "Je suis d'accord - je sens fortement que je fais partie de la communauté" = 1,
                "je suis un peu d'accord" = 0.75,
                "je ne suis pas d'accord ou pas d'accord" = 0.5, # Read Note 7!
                "je suis un peu en désaccord" = 0.25,
                "Je ne suis pas du tout d’accord - je n’ai pas du tout l’impression de faire partie de la communauté" = 0,
                "Je ne sais pas" = 0.5,
                "Je ne veux pas répondre" = 0.5
         ),
       `24_safety_n` =
         recode(`24_safety`,
                "Je me sens très en sécurité tout le temps" = 1,
                "Je me sens en sécurité la plupart du temps" = 0.75,
                "Neutre" = 0.5,
                "Je ne me sens pas en sécurité la plupart du temps" = 0.25,
                "Je me sens très en danger tout le temps" = 0,
                "Je ne veux pas répondre" = 0.5
         ),
       `25_family_conflict_n` =
         recode(`25_family_conflict`,
                "Jamais" = 1,
                "Rarement( Une fois le mois ou pas tous les mois)" = 0.75,
                "Des fois ( 2 ou 3 fois le mois)" = 0.5,
                "Souvent( au moins une fois la semaine)" = 0.25,
                "Très souvent ( plusieurs fois dans la semaine/chaque jours)" = 0,
                "Je souhaite ne pas répondre" = 0.5
         ),
       `26_discrimination_n` =
         recode(`26_discrimination`, # Read Note 8!
                "Jamais discriminé" = 1,
                "Rarement discriminé( C'est arrivé une fois)" = 0.75,
                "Parfois discriminé(C'est arrrivé plusieurs fois)" = 0.5,
                "Très souvent discriminé(Chaque semaine / Chaque jour)" = 0.25,
                "Je ne veux pas répondre" = 0.5
         ),
       `27_distress_n` =
         recode(`27_distress`,
                "Jamais" = 1,
                "Rarement( Une fois le mois ou pas tous les mois)" = 0.75,
                "Des fois ( 2 ou 3 fois le mois)" = 0.5,
                "Souvent( au moins une fois la semaine)" = 0.25,
                "Très souvent ( plusieurs fois dans la semaine/chaque jours)" = 0,
                "Je souhaite ne pas répondre" = 0.5
         ),
       `28_psy_support_n` =
         recode(`28_psy_support`,
                "Non" = 1,
                "Oui" = 0,
                "Je ne sait pas" = 0.5
         ),
       `29_able_to_stay_n` =
         recode(`29_able_to_stay`,
                "Oui" = 1, # Read Note 9!
                "Non" = 0,
                "Je ne sait pas" = 0, # Read Note 9!
                "Je souhaite ne pas répondre" = 0.5,
         ),
       `30_wish_vs_need_leave_n` =
         recode(`30_wish_vs_need_leave`,
                "SOUHAITAIS PARTIR - Par exemple, mes amis / membres de ma famille me manquent ailleurs; les facteurs culturels; souhaite poursuivre des études à l’étranger; etc." = 1,
                "BESOIN DE PARTIR - Par exemple, le manque d’emplois; le manque de sécurité; faibles revenus; le manque de services essentiels; la pression familiale; etc." = 0,
         )
       
) %>% 
  
  # REPLACE NA  ###############################################################
# According to documentation, all NAs should be coded 0.5
# Also note, these are either pre-existing NAs, or NAs produce by recode()*
# Economic
mutate(`5_debt_ratio_n` = replace_na(`5_debt_ratio_n`, 0.5), # Read Note 2! 
       `8_assets_n` = replace_na(`8_assets_n`, 1), # Read Note 3!
       # Social
       `13_school_n` = replace_na(`13_school_n`, 0.5), # Read Note 10!
       # Psycho social
       `30_wish_vs_need_leave_n` = replace_na(`30_wish_vs_need_leave_n`, 0) # Read Note 9!
       
) %>% 

  # ADD 29+30 VARIABLE  #######################################################
# Crucially, this needs to be done AFTER replacing the NA

mutate(# Construct of the above 2 measures
  `29_30_remigration_construct` = `29_able_to_stay_n` + `30_wish_vs_need_leave_n` # also
  # some NA, see above
       
) %>% 
    
  # ADD WEIGHT ################################################################
# Economic
mutate(`1_economic_dim_weight` = 0.15,
       `1_economic_comp_weight` = 0.05,
       `2_food_dim_weight` = 0.12,
       `2_food_comp_weight` = 0.08,
       `3_borrow_dim_weight` = 0.08,
       `3_borrow_comp_weight` = 0.02,
       `4_borrow_freq_dim_weight` = 0.1,
       `4_borrow_freq_comp_weight` = 0.02,
       `5_debt_ratio_dim_weight` = 0.08,
       `5_debt_ratio_comp_weight` = 0.04,
       `6_employment_dim_weight` = 0.13,
       `6_employment_comp_weight` = 0.03,
       `7_working_dim_weight` = 0.1,
       `7_working_comp_weight` = 0.03,
       `8_assets_dim_weight` = 0.11,
       `8_assets_comp_weight` = 0.03,
       `9_searching_job_dim_weight` = 0.13,
       `9_searching_job_comp_weight` = 0.03,
       # Social
       `10_housing_dim_weight` = 0.1,
       `10_housing_comp_weight` = 0.03,
       `11_housing_qual_dim_weight` = 0.12,
       `11_housing_qual_comp_weight` = 0.03,
       `12_education_dim_weight` = 0.11,
       `12_education_comp_weight` = 0.03,
       `13_school_dim_weight` = 0.07,
       `13_school_comp_weight` = 0.02,
       `14_justice_dim_weight` = 0.12,
       `14_justice_comp_weight` = 0.04,
       `15_id_dim_weight` = 0.05,
       `15_id_comp_weight` = 0.05,
       `16_documentation_dim_weight` = 0, # weight = 0!
       `16_documentation_comp_weight` = 0, # weight = 0!
       `17_water_dim_weight` = 0, # weight = 0!
       `17_water_comp_weight` = 0, # weight = 0!
       `18_health_dim_weight` = 0.2,
       `18_health_comp_weight` = 0.06,
       `19_health_qual_dim_weight` = 0.15,
       `19_health_qual_comp_weight` = 0.03,
       `20_services_construct_dim_weight` = 0.08, # construct!
       `20_services_construct_comp_weight` = 0.04, # construct!
       # Psycho social
       `21_socialize_dim_weight` = 0.12,
       `21_socialize_comp_weight` = 0.04,
       `22_network_dim_weight` = 0.05,
       `22_network_comp_weight` = 0.03,
       `23_community_dim_weight` = 0.15,
       `23_community_comp_weight` = 0.04,
       `24_safety_dim_weight` = 0.1,
       `24_safety_comp_weight` = 0.05,
       `25_family_conflict_dim_weight` = 0.12,
       `25_family_conflict_comp_weight` = 0.01,
       `26_discrimination_dim_weight` = 0.11,
       `26_discrimination_comp_weight` = 0,
       `27_distress_dim_weight` = 0.1,
       `27_distress_comp_weight` = 0.04,
       `28_psy_support_dim_weight` = 0.1,
       `28_psy_support_comp_weight` = 0.03,
       `29_30_remigration_construct_dim_weight` = 0.15, # construct!
       `29_30_remigration_construct_comp_weight` = 0.1, # construct!
       
) %>% 
  
  # COMPUTE SCORES #############################################################
# Economic (9 indicators)
mutate(`1_economic_dim_score` = `1_economic_n` * `1_economic_dim_weight`,
       `1_economic_comp_score` = `1_economic_n` * `1_economic_comp_weight`,
       `2_food_dim_score` = `2_food_n` * `2_food_dim_weight`,
       `2_food_comp_score` = `2_food_n` * `2_food_comp_weight`,
       `3_borrow_dim_score` = `3_borrow_n` * `3_borrow_dim_weight`,
       `3_borrow_comp_score` = `3_borrow_n` * `3_borrow_comp_weight`,
       `4_borrow_freq_dim_score` = `4_borrow_freq_n` * `4_borrow_freq_dim_weight`,
       `4_borrow_freq_comp_score` = `4_borrow_freq_n` * `4_borrow_freq_comp_weight`,
       `5_debt_ratio_dim_score` = `5_debt_ratio_n` * `5_debt_ratio_dim_weight`,
       `5_debt_ratio_comp_score` = `5_debt_ratio_n` * `5_debt_ratio_comp_weight`,
       `6_employment_dim_score` = `6_employment_n` * `6_employment_dim_weight`,
       `6_employment_comp_score` = `6_employment_n` * `6_employment_comp_weight`,
       `7_working_dim_score` = `7_working_n` * `7_working_dim_weight`,
       `7_working_comp_score` = `7_working_n` * `7_working_comp_weight`,
       `8_assets_dim_score` = `8_assets_n` * `8_assets_dim_weight`,
       `8_assets_comp_score` = `8_assets_n` * `8_assets_comp_weight`,
       `9_searching_job_dim_score` = `9_searching_job_n` * `9_searching_job_dim_weight`,
       `9_searching_job_comp_score` = `9_searching_job_n` * `9_searching_job_comp_weight`,
       # Social (11 indicators)
       `10_housing_dim_score` = `10_housing_n` * `10_housing_dim_weight`,
       `10_housing_comp_score` = `10_housing_n` * `10_housing_comp_weight`,
       `11_housing_qual_dim_score` = `11_housing_qual_n` * `11_housing_qual_dim_weight`,
       `11_housing_qual_comp_score` = `11_housing_qual_n` * `11_housing_qual_comp_weight`,
       `12_education_dim_score` = `12_education_n` * `12_education_dim_weight`,
       `12_education_comp_score` = `12_education_n` * `12_education_comp_weight`,
       `13_school_dim_score` = `13_school_n` * `13_school_dim_weight`,
       `13_school_comp_score` = `13_school_n` * `13_school_comp_weight`,
       `14_justice_dim_score` = `14_justice_n` * `14_justice_dim_weight`,
       `14_justice_comp_score` = `14_justice_n` * `14_justice_comp_weight`,
       `15_id_dim_score` = `15_id_n` * `15_id_dim_weight`,
       `15_id_comp_score` = `15_id_n` * `15_id_comp_weight`,
       `16_documentation_dim_score` = `16_documentation_n` * `16_documentation_dim_weight`, # weight=0!
       `16_documentation_comp_score` = `16_documentation_n` * `16_documentation_comp_weight`, # weight=0!
       `17_water_dim_score` = `17_water_n` * `17_water_dim_weight`, # weight = 0!
       `17_water_comp_score` = `17_water_n` * `17_water_comp_weight`, # weight = 0!
       `18_health_dim_score` = `18_health_n` * `18_health_dim_weight`,
       `18_health_comp_score` = `18_health_n` * `18_health_comp_weight`,
       `19_health_qual_dim_score` = `19_health_qual_n` * `19_health_qual_dim_weight`,
       `19_health_qual_comp_score` = `19_health_qual_n` * `19_health_qual_comp_weight`,
       `20_services_construct_dim_score` = `20_services_construct_n` * `20_services_construct_dim_weight`, # construct!
       `20_services_construct_comp_score` = `20_services_construct_n` * `20_services_construct_comp_weight`, # construct!
       # Psycho social (9 indicators)
       `21_socialize_dim_score` = `21_socialize_n` * `21_socialize_dim_weight`,
       `21_socialize_comp_score` = `21_socialize_n` * `21_socialize_comp_weight`,
       `22_network_dim_score` = `22_network_n` * `22_network_dim_weight`,
       `22_network_comp_score` = `22_network_n` * `22_network_comp_weight`,
       `23_community_dim_score` = `23_community_n` * `23_community_dim_weight`,
       `23_community_comp_score` = `23_community_n` * `23_community_comp_weight`,
       `24_safety_dim_score` = `24_safety_n` * `24_safety_dim_weight`,
       `24_safety_comp_score` = `24_safety_n` * `24_safety_comp_weight`,
       `25_family_conflict_dim_score` = `25_family_conflict_n` * `25_family_conflict_dim_weight`,
       `25_family_conflict_comp_score` = `25_family_conflict_n` * `25_family_conflict_comp_weight`,
       `26_discrimination_dim_score` = `26_discrimination_n` * `26_discrimination_dim_weight`,
       `26_discrimination_comp_score` = `26_discrimination_n` * `26_discrimination_comp_weight`,
       `27_distress_dim_score` = `27_distress_n` * `27_distress_dim_weight`,
       `27_distress_comp_score` = `27_distress_n` * `27_distress_comp_weight`,
       `28_psy_support_dim_score` = `28_psy_support_n` * `28_psy_support_dim_weight`,
       `28_psy_support_comp_score` = `28_psy_support_n` * `28_psy_support_comp_weight`,
       `29_30_remigration_construct_dim_score` = `29_30_remigration_construct` * `29_30_remigration_construct_dim_weight`, # construct!
       `29_30_remigration_construct_comp_score` = `29_30_remigration_construct` * `29_30_remigration_construct_comp_weight`, # construct!
       
       
) %>% 
  
  # COMPUTE DIMENSIONAL SCORES ################################################
# Economic (9 indicators)
mutate(EconomicScore = 
         `1_economic_dim_score` + `2_food_dim_score` + `3_borrow_dim_score` + `4_borrow_freq_dim_score` + `5_debt_ratio_dim_score` + `6_employment_dim_score` + `7_working_dim_score` + `8_assets_dim_score` + `9_searching_job_dim_score`,
       # Social (11 indicators)
       SocialScore = `10_housing_dim_score` + `11_housing_qual_dim_score` + `12_education_dim_score` + `13_school_dim_score` + `14_justice_dim_score` + `15_id_dim_score` + `16_documentation_dim_score` + `17_water_dim_score` + `18_health_dim_score` + `19_health_qual_dim_score` + `20_services_construct_dim_score`,
       # Psycho social (9 indicators)
       PsychoSocialScore = `21_socialize_dim_score` + `22_network_dim_score` + `23_community_dim_score` + `24_safety_dim_score` + `25_family_conflict_dim_score` + `26_discrimination_dim_score` + `27_distress_dim_score` + `28_psy_support_dim_score` + `29_30_remigration_construct_dim_score`
       
) %>% 
  # COMPUTE COMPOSITE SCORE ################################################
# 29 indicators
mutate(CompositeScore = `1_economic_comp_score` + `2_food_comp_score` + `3_borrow_comp_score` + `4_borrow_freq_comp_score` + `5_debt_ratio_comp_score` + `6_employment_comp_score` + `7_working_comp_score` + `8_assets_comp_score` + `9_searching_job_comp_score` + `10_housing_comp_score` + `11_housing_qual_comp_score` + `12_education_comp_score` + `13_school_comp_score` + `14_justice_comp_score` + `15_id_comp_score` + `16_documentation_comp_score` + `17_water_comp_score` + `18_health_comp_score` + `19_health_qual_comp_score` + `20_services_construct_comp_score` + `21_socialize_comp_score` + `22_network_comp_score` + `23_community_comp_score` + `24_safety_comp_score` + `25_family_conflict_comp_score` + `26_discrimination_comp_score` + `27_distress_comp_score` + `28_psy_support_comp_score` + `29_30_remigration_construct_comp_score`
       
         )
  

# Warnings
# (1) Problem while computing `8_assets_n = recode(...)`. Can be safely ignored,
# since NA are replaced following it, on purpose.

# 3. MERGE ####

# Objects dimensions
dim(mimosa_slim) # 2,580 x  28
dim(kobo_slim)   # 1,385 x 187

# Target dimension
# 1,385 x 214

# Check duplicates in Mimosa (not an issue, just for the record)
sum(duplicated(mimosa$`CaseNo/IndividualNo`)) # 13
sum(duplicated(mimosa_slim$ID))  # 13
mimosa_slim[duplicated(mimosa_slim$ID), 'ID']

# We'll do a right join, keeping all rows from kobo_slim
rss <- merge(mimosa_slim, kobo_slim, by='ID', all.y = TRUE)

# Dimensions
dim(rss) # 1,385 x 214 --> as expected

# Check perfect duplicates
sum(duplicated(rss)) # 0
# Check pseudo-duplicates
sum(duplicated(rss$ID)) # 128
# Note all these duplicates are NA, as expected
rss[duplicated(rss$ID), "ID"]

# Compute last needed variable
rss  <- rss %>% mutate(MBAssistanceDuration = 
                 as.numeric(difftime(interview_date, MicrobusinessEndDate, units = "days")))

# Final dimensions
dim(rss) # 1,385 x 215 --> as expected



# NA
colSums(is.na(rss))

# Data types
str(rss)

# Character to Numeric
# If needed, outstanding


# EXPORT FULL DATA ####
#write_excel_csv(rss, 'data_clean/rss.csv')
# RDS version
#saveRDS(rss, file = 'data_clean/rss.rds')


###############################################################################


# SUBSET A SLIM VERSION ####

rss_slim <- rss %>% 
  
  # SELECT
  select(ID,
         
         # Dependent variables
         EconomicScore, 
         SocialScore,
         PsychoSocialScore,
         CompositeScore,
         
         # Independent variables
         # (a) Numeric
         migration_duration,
         TrainingDuration,
         MBSupportDuration,
         MBAssistanceDuration,
         
         # (b) Categorical
         sex,
         age, # --> to convert to num in due time
         return_country,
         origin_country,
         VOTs,
         UMINOR, 
         HealthCondition,
         
         # (c) Assistance
         CounsellingStatus,
         EconomicSupport,
         FinancialServices,
         JobPlacement,
         Microbusiness,
         Training, 
         SocialSupport,
         ChildCare,
         Education,
         Housing,
         LegalServices,
         MaterialAssistance,
         MedicalSupport,
         SocialProtectionSchemes,
         PsychosocialSupport,
         
         # (d) Other
         Microbusinesslevel, 
         MicrobusinessDeliveredBy, 
         MicrobusinessFormOfAssistance
  )



# WE ARE HERE

# NA
#colSums(is.na(rss_slim))

# Recode Independent variables

# Principles:
# Answers who represent less than 15% of all answers are grouped together
# Exceptions:
# sex (female=14%), since this is a crucial variable
# origin_country, kept all above 10%

colSums(is.na(rss_slim))


# Before
rss_slim %>% group_by(FinancialServices) %>% summarise(count = n()) %>% 
  mutate(percent = count/sum(count)*100) %>% arrange(-percent) %>% print(n=21)


# Variable-------------------------Levels ---Smallest -----NA -----Decision
# sex: -------------------------------- 2 ------  195 ----- 0 ---> keep as is
# return_country: -------------------- 20 --------- 1 ----- 0 ---> recode
# origin_country: -------------------- 12 -------- 43 ----- 0 ---> recode
# VOTs: ------------------------------- 2 -------- 27 --- 189 ---> drop variable
# UMINOR: ----------------------------- 2 --------- 6 --- 189 ---> drop variable
# HealthCondition:--------------------- 2 -------- 59 --- 189 ---> drop variable
# CounsellingStatus:------------------- 5 ------- *70 --- 325 ---> drop variable
# EconomicSupport:--------------------- 2 --------- 4 --- 189 ---> drop variable
# FinancialServices:------------------- 2 ------- 345 --- 189 ---> keep variable, fill NA (189)
# JobPlacement:------------------------ 2 --------- 2 --- 189 ---> drop variable
# Microbusiness:----------------------- 2 -------- 12 --- 189 ---> drop variable
# Training:---------------------------- 2 ------- 581 --- 189 ---> keep variable, fill NA (189)
# SocialSupport:----------------------- 2 ------- 198 --- 189 ---> keep variable, fill NA (189)
# ChildCare:--------------------------- 2 --------- 2 --- 189 ---> drop variable
# Education:--------------------------- 2 --------- 4 --- 189 ---> drop variable
# Housing:----------------------------- 2 -------- 19 --- 189 ---> drop variable
# LegalServices:----------------------- 2 -------- 39 --- 189 ---> drop variable
# MaterialAssistance :----------------- 2 ------- 154 --- 189 ---> keep variable, fill NA (189)
# MedicalSupport:---------------------- 2 ------- 193 --- 189 ---> keep variable, fill NA (189)
# SocialProtectionSchemes:------------- 2 --------- 1 --- 189 ---> drop variable
# PsychosocialSupport:----------------- 2 ------- 434 --- 189 ---> keep variable, fill NA (189)
# Microbusinesslevel:------------------ 3 ------- *33 --- 201 ---> drop variable
# MicrobusinessDeliveredBy:------------ 2 -------- 23 --- 201 ---> drop variable
# MicrobusinessFormOfAssistance:------- 3 ------- 186 --- 262 ---> keep variable, fill NA (262)

# Note, levels do NOT include NA level
# *several categories, but cannot be combined



dim(rss_slim) # 1385 x 34

# If NA are dropped, 1385 - 189 = 1196




rss_slim <- rss_slim %>% mutate(
  
  # RECODE
  return_country = 
    case_when(
      return_country != "Libye" & return_country != "Niger" & return_country != "Algérie"  ~ 'Autre',
      TRUE ~ as.character(return_country)
    ),
  
  origin_country = 
    case_when(
      origin_country != "Niger" & origin_country != "Guinee Conakry" & origin_country != "Mali" & origin_country != "Tchad"  ~ 'Autre',
      TRUE ~ as.character(origin_country)
    )

) %>% 
  
  # FILL NA
  mutate(MicrobusinessFormOfAssistance = replace_na(MicrobusinessFormOfAssistance, 'Unknown'),
         FinancialServices = replace_na(FinancialServices, 'Unknown'), 
         Training = replace_na(Training, 'Unknown'),
         SocialSupport = replace_na(SocialSupport, 'Unknown'),
         MaterialAssistance = replace_na(MaterialAssistance, 'Unknown'),
         MedicalSupport = replace_na(MedicalSupport, 'Unknown'),
         PsychosocialSupport = replace_na(PsychosocialSupport, 'Unknown'),
         
         ) %>% 
  
  # DROP VARIABLES
  select(-c(VOTs, UMINOR, HealthCondition, CounsellingStatus, EconomicSupport,
            JobPlacement, Microbusiness, ChildCare, Education, Housing,
            LegalServices, SocialProtectionSchemes, Microbusinesslevel,
            MicrobusinessDeliveredBy)
         
  )

# After
rss_slim %>% group_by(FinancialServices) %>% summarise(count = n()) %>% 
  mutate(percent = count/sum(count)*100) %>% arrange(-percent)


dim(rss_slim) # 1,385 x 20 (14 columns less, as expected)


# NA
colSums(is.na(rss_slim))


str(rss_slim)

# Outliers; fill NA in numeric variables

# Variables to process

# Age (years)
# Not computed

# To be converted to num first
# Conversion might coerce to NA; check NA count before
sum(is.na(rss_slim$age)) # 0
# Print levels before
rss_slim %>% group_by(age) %>% summarise(count = n()) %>% print(n=58)
# Convert to number
rss_slim$age  <- as.numeric(rss_slim$age)
# Print levels after
rss_slim %>% group_by(age) %>% summarise(count = n()) %>% print(n=58) # all good
# Check NA count after
sum(is.na(rss_slim$age)) # 0, all good

# Summarise
summary(rss_slim$age) # median = 28, min = 0, max = 300

# First we'll convert some values that are obvious mistakes to NA
rss_slim[rss_slim$age < 14 | rss_slim$age > 100, 'age']  <- NA
length(rss_slim[rss_slim$age < 14 | rss_slim$age > 100, 'age']) # 19
# We should now have 19 NA
sum(is.na(rss_slim$age)) # 19 as expected

# Let's re-summarise
summary(rss_slim$age)
# And let's store the median
q_median <- median(rss_slim$age, na.rm = TRUE)
q_median # 28 years old

# Although age has outliers, it seems reasonably distributed
# We'll therefore do not replace outliers, but simply replace all NA
# with the median
rss_slim[is.na(rss_slim$age), 'age']  <- q_median

# Let's re-summarise
summary(rss_slim$age) # median still 28
# We should now have 0 NA
sum(is.na(rss_slim$age)) # 0 as expected


# migration_duration (years)
# Not computed

# Warning, I wonder if some answers are not weeks instead of years!!!

# To be converted to num first
# Conversion might coerce to NA; check NA count before
sum(is.na(rss_slim$migration_duration)) # 0
# Print levels before
rss_slim %>% group_by(migration_duration) %>% summarise(count = n()) %>% print(n=49)

# Convert to number
rss_slim$migration_duration  <- as.numeric(rss_slim$migration_duration)
# Print levels after
rss_slim %>% group_by(migration_duration) %>% summarise(count = n()) %>% print(n=49) # all good
# Check NA count after
sum(is.na(rss_slim$migration_duration)) # 0, all good

# First, we'll replace these 7 values, which are mistakes and not outliers,
# with NA
sort(rss_slim[rss_slim$migration_duration > 100, "migration_duration"])
rss_slim[rss_slim$migration_duration > 100, "migration_duration"]  <- NA

# Summarise
summary(rss_slim$migration_duration) # median = 2, min 0, max 86, NA 7, all as expected

# Let's store the median
q_median <- median(rss_slim$migration_duration, na.rm = TRUE)
q_median # 2 years


# Spot outliers using percentile method, with conservative threshold of 0.01/0.99
lower_bound <- quantile(rss_slim$migration_duration, 0.01, na.rm = TRUE)
upper_bound <- quantile(rss_slim$migration_duration, 0.99, na.rm = TRUE)
outlier_ind <- which(rss_slim$migration_duration < lower_bound | rss_slim$migration_duration > upper_bound)
length(rss_slim[outlier_ind, "migration_duration"]) # 12 outliers,
sort(rss_slim[outlier_ind, "migration_duration"]) # with smallest being 60 years

# Replace 18 extreme outliers with median
rss_slim[outlier_ind, "migration_duration"] <- q_median

# Re-summarise
summary(rss_slim$migration_duration) # median still 2, max 48 years as expected

# We should still have the same number of NA
sum(is.na(rss_slim$migration_duration)) # 7 --> as expected

# And we will also replace them with the median
rss_slim[is.na(rss_slim$migration_duration), "migration_duration"] <- q_median


# Re-summarise
summary(rss_slim$migration_duration) # median still 2, max still 48
# No NA should remain
sum(is.na(rss_slim$migration_duration)) # 0, all good


#######################################################


# TrainingDuration (days)
# Computed from Mimosa:
# TrainingEndDate - TrainingStartDate


# Summarise
summary(rss_slim$TrainingDuration) # median = 4, min 0, max 167, NA 770

# NA
sum(is.na(rss_slim$TrainingDuration)) # 770


# We have 770 NA. Logically, all these NA should be people who did not receive
# training or that we recoded as Unknown above. Let's check
rss_slim %>% group_by(Training) %>% summarise(count = n())
# No         581
# Unknown    189
# Yes        615

# That is the case, however, we cannot be sure that the Unknown received no training!
# But we decide to recode all these 770 cases as 0 days of training, which will need
# to be reported (i.e., 189 Unknown were recoded to 0 days).

# Another thing we need to check is that no people who receive training are NA or have
# 0 days of training. Let's check:
dim(rss_slim[rss_slim$Training == 'Yes' & rss_slim$TrainingDuration == 0 & !is.na(rss_slim$TrainingDuration), c('Training', 'TrainingDuration')]) # 115

# Unfortunatly, we have 115 respondents who stated they received training, but have 0
# days of training.

# So what we'll do is this. We will add one day to all respondents, and code the NA as
# 0, like this, we'll still be able to differentiate between No training and training.
# This too will need to be reported.
rss_slim$TrainingDuration <- rss_slim$TrainingDuration + 1

# Re-summarise
summary(rss_slim$TrainingDuration) # median = 4, min now 1, max now 168, NA still 770,
# all as expected

# We will now check for outliers. We will replace NA with 0 after this, because doing it
# now would change the median to 0, which would be incorrect for those outliers who did
# received training.
# Also, we will use the median before adding the ones, since this seems more adequate.


# Let's store the median
q_median <- 4 # exception, from above



# Spot outliers using percentile method, with conservative threshold of 0.01/0.99
lower_bound <- quantile(rss_slim$TrainingDuration, 0.01, na.rm = TRUE)
upper_bound <- quantile(rss_slim$TrainingDuration, 0.99, na.rm = TRUE)
outlier_ind <- which(rss_slim$TrainingDuration < lower_bound | rss_slim$TrainingDuration > upper_bound)
length(rss_slim[outlier_ind, "TrainingDuration"]) # 7 outliers,
sort(rss_slim[outlier_ind, "TrainingDuration"]) # with smallest being 83 days

# Replace 7 extreme outliers with first median
rss_slim[outlier_ind, "TrainingDuration"] <- q_median

# Re-summarise
summary(rss_slim$TrainingDuration) # median = 5, min still 1, max 74 as expected, NA still 770,
# all as expected
# Note median is now 5 (not 4), because of the added ones. That said, we replaced the values of
# outliers with the original median, 4

# Let us now finally code all the NA as 0 days:

rss_slim <- rss_slim %>% mutate(TrainingDuration = replace_na(TrainingDuration, 0))

# Re-check NA
sum(is.na(rss_slim$TrainingDuration)) # 0 as expected
# Re-summarise
summary(rss_slim$TrainingDuration) # median is now 0 (as expected), min is 0 (as expected),
# and max is still 74

# Let's plot our final distribution

boxplot(rss_slim$TrainingDuration,
        ylab = "Days",
        main = "TrainingDuration"
)
# Skewed to 0, but better than losing all these observations, I guess, and difficult to
# do better given inconsistency in the data mentioned above.


#######################################################


# MBSupportDuration (days)
# Computed from Mimosa:
# MicrobusinessEndDate - ArrivalDate

# Summarise
summary(rss_slim$MBSupportDuration) # median = 121.5, min -191 (!), max 1658, NA 203

# NA
sum(is.na(rss_slim$MBSupportDuration)) # 203

# First, investigate negative numbers
rss_slim[rss_slim$MBSupportDuration <= 0 & !is.na(rss_slim$MBSupportDuration), "MBSupportDuration"]

# There is only one, which we will convert to NA
rss_slim[rss_slim$MBSupportDuration <= 0 & !is.na(rss_slim$MBSupportDuration), "MBSupportDuration"]  <- NA

# Re-summarise
summary(rss_slim$MBSupportDuration) # median now 122, min now 2, max still 1658, NA now 204, all
# as expected

# As for TrainingDuration, we will replace the NA with 0. That said, we do not have the same
# issue concerning respondents who received training with 0 days.
# Again, we'll do the change after spotting outliers.

# Let's store the median
q_median <- 121.5 # exception, from above



# Spot outliers using percentile method, with conservative threshold of 0.01/0.99
lower_bound <- quantile(rss_slim$MBSupportDuration, 0.01, na.rm = TRUE)
upper_bound <- quantile(rss_slim$MBSupportDuration, 0.99, na.rm = TRUE)
outlier_ind <- which(rss_slim$MBSupportDuration < lower_bound | rss_slim$MBSupportDuration > upper_bound)
length(rss_slim[outlier_ind, "MBSupportDuration"]) # 12 outliers,
sort(rss_slim[outlier_ind, "MBSupportDuration"]) # with smallest being 384 days

# Replace 7 extreme outliers with first median
rss_slim[outlier_ind, "MBSupportDuration"] <- q_median

# Re-summarise
summary(rss_slim$MBSupportDuration) # median still 121.5, min still 2, max now 383, NA still 204,
# all as expected


# Let us now finally code all the NA as 0 days:

rss_slim <- rss_slim %>% mutate(MBSupportDuration = replace_na(MBSupportDuration, 0))

# Re-check NA
sum(is.na(rss_slim$MBSupportDuration)) # 0 as expected
# Re-summarise
summary(rss_slim$MBSupportDuration) # median is now 96, min is now 0,
# and max is still 383

# Let's plot our final distribution

boxplot(rss_slim$MBSupportDuration,
        ylab = "Days",
        main = "MBSupportDuration"
)
# Looking quite good.

# I am here






































# MBAssistanceDuration (days)
# Computed from Mimosa and Kobo
# interview_date [Kobo] - MicrobusinessEndDate [Mimosa]






# Export slim data
#write_excel_csv(rss_slim, 'data_clean/rss_slim.csv')
# RDS version
#saveRDS(rss_slim, file = 'data_clean/rss_slim.rds')


###############################################################################


# Notes ####

# (1) This item is actually "1.Satisfation situation economique actuelle", ac-
# -cording to email of Julie of 12 June 2023. Now solved.

# (2) All NA were coded as 0.5 to follow the documentation, however, there is an
# important limitation with this item.
# First, note that this item, contrary to the English version and documentation,
# does NOT have an answer option 'I don't have debts'.
# We might be tempted to solve this issue using the previous question, which is
# "4b. Avez-vous actuellement une dette à rembourser?". For example, we might
# consider that all participants who replied that they do not have debt in
# question 4b should not be NA in our target question (5), but that they do not
# have debts (i.e., the missing answer option), which should be coded 1 (not 0.5
# as we have done).
# That said, an inconsistency prevents us from doing that. That inconsistency is
# that 666 respondents who replied No to question 4b still have answers to
# question 5, which is contradictory. Put differently, not all participants
# who have NA for question 5 do not have debts, if we trust question 4b.
# I believe that the 'real' number of NA for question 5 is 18 (not 220), which
# corresponds to people who replied Yes to question 4b, and do not have data
# for question 5, but we cannot be sure given the inconsistency mentioned 
# above.
# In summary, I suggest to investigate further how questions 4b and 5 were coded,
# since it might contain an anomaly, which might biase the final scores.
# For the record, this is how they are currently coded
# Labels
#rss %>% group_by(`5_debt_ratio`) %>% summarise(count = n())
#1 Je souhaite ne pas répondre           21 --> 0.5
#2 La dette est plus grande             162 --> 0
#3 Les dépenses sont plus importantes   982 --> 1
#4 NA                                   220 --> 0.5
# Numbers
#rss %>% group_by(`5_debt_ratio_n`) %>% summarise(count = n())
#1  0     162 --> correct
#2  0.5   241 --> correct (21+220)
#3  1     982 --> correct

# (3) 8 respondents stated not to have any assets and having assets at the same
# time. These were coded as having assets (code = 1).

# (4) This item should be numbered 10, not 11.

# (5) Since French version has 1 more category than English version, we decided
# to group 'Aucun' (2 answers) and 'Non' (107 answers) as 'None' (code = 0).

# (6) This item should be numbered 21, not 22.

# (7) This is a confusing answer option.

# (8) One answer option, 'All the time' ou 'Tout le temps', code = 0, is missing
# compared to the documentation. Or, 'Souvent', which equally biases the result.

# (9) The instructions on how to code this item might be wrong (reversed) in the
# documentation. This is a major limitation that needs to be investigated

# (10) All missing data for that question ("14. Tous les enfants d’âge scolaire
# de votre ménage fréquentent-ils actuellement l’école?") were coded as 0 in raw
# data, but should have been coded as 0.5 according to documentation, meaning that
# 300+ scores were incorrect in the raw data.




# Skeletons ####

# To add variables to compute RSS scores:
#`XXX_n` =
#  recode(`XXX`,
#         "" = 1,
#         "" = 0.75,
#         "" = 0.5,
#         "" = 0.25,
#         "" = 0,
#         "" = 0.5
#  ),
##################
#`XXX_dim_weight` = ,
#`XXX_comp_weight` = ,
##################
#`XXX_dim_score` = `XXX_n` * `XXX_dim_weight`,
#`XXX_comp_score` = `XXX_n` * `XXX_comp_weight`,
############
#names(kobo)
#kobo %>% group_by(`FULLQUESTIONNAME`) %>% summarise(Count = n())
#kobo_slim %>% group_by(`XXX_n`) %>% summarise(Count = n())
#view(kobo_slim)












