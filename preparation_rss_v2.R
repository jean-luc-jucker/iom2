# Main change from v1: we use new cleaned data (received 14 June 2023 from
# client), and do the merge ourselves.

getwd()
library(readxl)
library(tidyverse)

# Reintegration Economic Survey (kobo) ####
kobo <- read_excel('data_raw/RSS data cleaned to use for analysis V2.xlsx',
                  na = c('N/A', 'NA', 'na')) 
dim(kobo) # 1,386 x 96
# Previous database: 1,361 x 319 (25 more)

# Mimosa data (mimosa)
mimosa <- read_excel('data_raw/Mimosa data - Reintegration Cases M&E RSS June 2023.xlsx',
                     na = c('N/A', 'NA', 'na'))
dim(mimosa) # 2,536 x 57


# Dupes
# First note, there were 1 perfect duplicate and 82 duplicated ID (`Code Mimosa Corrigé`)
# in previous file
sum(duplicated(kobo)) # 1
sum(duplicated(kobo$`Code Mimosa Corrigé`)) # 108
sum(duplicated(kobo$`Identifiant MiMOSA du cas`)) # 75

dupes <- kobo[duplicated(kobo$`Code Mimosa Corrigé`), "Code Mimosa Corrigé"]
write.csv(dupes, 'data_clean/final_dupes.csv')

sum(duplicated(mimosa)) # 0


# Subset
kobo_slim <- kobo %>% select(`Code Mimosa Corrigé`, `Identifiant MiMOSA du cas`)
mimosa_slim <- mimosa %>% select(`CaseNo/IndividualNo`, CaseNo, MemberNo)

# Compare variable intersect
# Using Code Mimosa Corrigé as reference
# Common IDs between Code Mimosa Corrigé and CaseNo/IndividualNo
length(intersect(kobo_slim$`Code Mimosa Corrigé`, mimosa_slim$`CaseNo/IndividualNo`)) # 1,189
# Common IDs between Code Mimosa Corrigé and CaseNo
length(intersect(kobo_slim$`Code Mimosa Corrigé`, mimosa_slim$CaseNo)) # 881
# Common IDs between Code Mimosa Corrigé and MemberNo
length(intersect(kobo_slim$`Code Mimosa Corrigé`, mimosa_slim$MemberNo)) # 308  NOTE, 881 + 308 = 1,189
# Using Identifiant MiMOSA Corrigé as reference (smaller intersect)
# Common IDs between Identifiant MiMOSA du cas and CaseNo/IndividualNo
#length(intersect(kobo_slim$`Identifiant MiMOSA du cas`, mimosa_slim$`CaseNo/IndividualNo`)) # 1,110
# Common IDs between Identifiant MiMOSA du cas and CaseNo
#length(intersect(kobo_slim$`Identifiant MiMOSA du cas`, mimosa_slim$CaseNo)) # 820
# Common IDs between Identifiant MiMOSA du cas and MemberNo
#length(intersect(kobo_slim$`Identifiant MiMOSA du cas`, mimosa_slim$MemberNo)) # 290  NOTE, 820 + 290 = 1,110

# Previous real sample size
# 1361 - 81 = 1,280

# Current sample size
# 1,189
# Difference: 1,280 - 1,189 = 91 missing compared to previous version


# 1,189 - 1,110 = 79 (would mean a final difference of 91 - 79 = 12)


# 



# subset
# merge
# compute scores

# merge
# subset, rename, compute scores




# Subset ######################################################################
df <- rss %>% 
  
  # PULL ######################################################################
  select(
    # Metadata
    
    # only for checking purposes, to be removed in due time
    "Code Mimosa Corrigé",
    #"Economic reintegration Score",
    #"Social reintegration score",

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
    "29. Souhaitez-vous recevoir un soutien psychologique spécialisé"
    
    

    # Grouping variables

    ) %>% 
  
  # RENAME ####################################################################
  rename(
    # Metadata

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
      "29. Souhaitez-vous recevoir un soutien psychologique spécialisé"
    
    
    # Grouping variables

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
         `13_school_n` = # Read Note 5!
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
         
         
         
         
    ) %>% 
  
  # REPLACE NA  ###############################################################
  # According to documentation, all NAs should be coded 0.5
  # Also note, these are either pre-existing NAs, or NAs produce by recode()*
  # Economic
  mutate(`5_debt_ratio_n` = replace_na(`5_debt_ratio_n`, 0.5), # Read Note 2! # previously 1!!!!!!!!!!
         `8_assets_n` = replace_na(`8_assets_n`, 1), # * Read Note 3!
         # Social
         `13_school_n` = replace_na(`13_school_n`, 0.5),
         # Psycho social
         
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
         
         
         
         
  
    ) %>% 
  
  # COMPUTE SCORES #############################################################
  # Economic
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
         # Social
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
         # Psycho social
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
         
    
    ) %>% 
  
  # COMPUTE DIMENSIONAL SCORES ################################################
  # Economic
  mutate(EconomicScore = 
           `1_economic_dim_score` + `2_food_dim_score` + `3_borrow_dim_score` + `4_borrow_freq_dim_score` + `5_debt_ratio_dim_score` + `6_employment_dim_score` + `7_working_dim_score` + `8_assets_dim_score` + `9_searching_job_dim_score`,
         # Social
         SocialScore = `10_housing_dim_score` + `11_housing_qual_dim_score` + `12_education_dim_score` + `13_school_dim_score` + `14_justice_dim_score` + `15_id_dim_score` + `16_documentation_dim_score` + `17_water_dim_score` + `18_health_dim_score` + `19_health_qual_dim_score` + `20_services_construct_dim_score`,
         # Psycho social
           
           )# %>% 
  
  # only for checking purposes, to be removed in due time
  #mutate(Difference = SocialScore - `Social reintegration score`)
  
# Warnings
# (1) Problem while computing `8_assets_n = recode(...)`. Can be safely ignored,
# since NA are replaced following it, on purpose.

# export
#write.csv(df, 'data_clean/df_clean_temp.csv')

###############################################################################
###############################################################################

names(rss)

rss %>% group_by(`8. Possédez-vous l’un des actifs productifs suivants?`) %>% summarise(Count = n())

df %>% group_by(`8_assets_n`) %>% summarise(Count = n())
view(df)



###############################################################################
###############################################################################

# Economic
# Faulty cases
faulty <- df %>% select(`Code Mimosa Corrigé`, Difference) %>% filter(Difference != 0)
faulty
write.csv(faulty, 'data_clean/faulty_cases.csv')

# Duplicates
dupes <- df[duplicated(df$`Code Mimosa Corrigé`),]
dupes
write.csv(dupes, 'data_clean/dupes.csv')

# Social
mean(df$SocialScore) # 0.5763275 (not rounded)        0.5762821 (rounded)  
mean(df$`Social reintegration score`, na.rm = TRUE) # 0.5662696 (rounded) -> slightly lower
# Faulty cases
faulty <- df %>% select(`Code Mimosa Corrigé`, Difference) %>% filter(Difference != 0)
faulty
write.csv(faulty, 'data_clean/faulty_cases_social.csv')


df$`Social reintegration score`

df[df$`Code Mimosa Corrigé` == 'CH1022X000307', c("Social reintegration score", "SocialScore",
              "10_housing_dim_score", "11_housing_qual_dim_score", "12_education_dim_score", "20_services_construct_dim_score"
                                                  )]

# TEMP ########################################################################
###############################################################################

`XXX_n` =
  recode(`XXX`,
         "" = 1,
         "" = 0.75,
         "" = 0.5,
         "" = 0.25,
         "" = 0,
         "" = 0.5
  ),

##################

`XXX_dim_weight` = ,
`XXX_comp_weight` = ,

##################

`XXX_dim_score` = `XXX_n` * `XXX_dim_weight`,
`XXX_comp_score` = `XXX_n` * `XXX_comp_weight`,

############

names(rss)

rss %>% group_by(`FULLQUESTIONNAME`) %>% summarise(Count = n())

df %>% group_by(`XXX_n`) %>% summarise(Count = n())
view(df)

###############################################################################
###############################################################################

dim(df)




view(rss)




# Notes ####

# (1) This item is actually "1.Satisfation situation economique actuelle", ac-
# -cording to email of Julie of 12 June 2023.

# (2) NA should normally be coded as 0.5. That said, the French version of RSS
# is missing an answer option compared to the English version, which is "I don't
# have debts". Since all of the NAs replied that they do not have debt in the
# previous question ("4b. Avez-vous actuellement une dette à rembourser?"), it
# is likely that the NA mean that the respondents do not have debts. If that is
# the case, they should be coded as 1 following the documentation, which is what
# was done here.
# However, we note that 657 respondents who also replied not having debt in qu-
# estion 4b have an answer for question 5, which is odd. I therefore suggest to
# investigate further how question 5 was coded, since it might contain an anomaly,
# which might biase the final scores.

# (3) 8 respondents stated not to have any assets and having assets at the same
# time. These were coded as having assets (code = 1).

# (4) This item should be numbered 10, not 11.

# (5) Since French version has 1 more category than English version, we decided
# to group 'Aucun' (2 answers) and 'Non' (107 answers) as 'None' (code = 0).

# (6) This item should be numbered 21, not 22.

# (7) This is a confusing answer option.

# (8) One answer option, 'All the time' ou 'Tout le temps', code = 0, is missing
# compared to the documentation. Or, 'Souvent', which equally biases the result.










# NA
colSums(is.na(df))

# Data types
str(df)

# Character to Numeric
df$MigrationDuration <- as.numeric(df$MigrationDuration)
df$TimeToReceiveSupport <- as.numeric(df$TimeToReceiveSupport)
df$SupportTotalValue <- as.numeric(df$SupportTotalValue)


# Export
write_excel_csv(df, 'data_clean/res_slim.csv') # using extension .xls will avoid
# wrapping, but will produce unsafe warning, so we use .csv
# RDS version
saveRDS(df, file = 'data_clean/res_slim.rds')

# Base exploration


# Load res object as df, and convert strings to factors
df <- readRDS('data_clean/res_slim.rds')%>% 
  mutate(across(where(is.character), as.factor))
dim(df)

# Discard unplottable variables
df <- df %>% select(-c(MimosaID, InterviewDate))
dim(df)

# 1. Overall ##################################################################

# Set iteration counter to 0. This is needed only for numbering the output .png
# plots. This needs to be an external variable.
counter <- 0

# Function
make_plots_overall <- function(df){ # fun start
  
  for (col in colnames(df)){ # for-loop start
    
    # (1) Some variables we'll need
    # Count of non-na rows; needed for plot title
    N <- sum(!is.na(df[col]))
    # Count of missing observations; needed for plot title
    Missing <- sum(is.na(df[col]))
    # Number of levels for each factor; needed for saving plots in correct aspect
    # ratio
    n_levels <- dim(unique(df[col]))[1]
    # Update counter; needed for plot numbering
    counter <- counter + 1
    
    # Skip items with 0 observations
    # This is crucial to avoid error when wrapping long level names (these cannot
    # be wrapped in there are none)
    if (N == 0) next
    
    # (2) UNORDERED FACTORS
    if (is.factor(df[[col]]) & !is.ordered(df[[col]])) { # if-else start
      # We remove NA here; also note we use sym(), needed mainly to reorder the
      # bars in a loop; also note, fct_rev() is necessary to revert fct_infreq()
      p <- ggplot(data = df[col] %>% filter(!is.na(df[col])),
                  aes(x=fct_rev(fct_infreq(!!sym(col)))))
      # Compute percents for bars
      p <- p + geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5,
                        fill = 'black')
      if (N<100){
        # Low N warning
        p <- p + labs(title = paste(col, '| Overall'),
                      subtitle = paste0('Single-select | ', 'N = ', format(N, big.mark = ',')
                                        , ', interpret with caution!'
                      ))
      } else {
        # No warning
        p <- p + labs(title = paste(col, '| Overall'),
                      subtitle = paste0('Single-select | ', 'N = ', format(N, big.mark = ',')
                      ))
      }
      # Aesthetics
      # Wrap long level names
      p <- p + scale_x_discrete(labels = function(x) str_wrap(x, width = 30))
      # Add percents as annotations; note, rounding percents using round() be-
      # haves oddly, and it is much better to use accuracy = 0.1L
      p <- p + geom_text(aes(label = scales::percent((..count..)/sum(..count..),
                                                     accuracy = 0.1L),
                             y = ((..count..)/sum(..count..))), stat="count",
                         hjust = -0.1, size=4)
      # Aesthetics
      p <- p + theme_minimal()
      p <- p + theme(axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y = element_text(colour='black', size = 12),
                     plot.margin = margin(r=25, l=1, b=0, t=1),
                     plot.title = element_text(size = 16),
                     plot.subtitle = element_text(size = 12),
                     panel.grid = element_blank())
      # Flip coordinates; note, this is needed only to accommodate the percent an-
      # notation, in combination with plot.margin(); without annotations, we might
      # have simply reversed x and y everywhere to get exact same result
      p <- p + coord_flip(clip = 'off')
      # # Print
      print(p)
      # Save as png; note n_levels*1.3+1 nicely accommodates most variables for a
      # MS Word output; note we decided to number files just to preserve df order
      # instead of alphabetical order
      if (n_levels<15) {
        ggsave(paste(counter, '_', col, '.png'), path = 'plots/single_select/overall',
               width = 16.5,
               height = n_levels * 1.3 + 1, units = 'cm')
        # If factor has more than 13 levels, it is better to use this proportion
      } else {
        ggsave(paste(counter, '_', col, '.png'), path = 'plots/single_select/overall',
               width = 16.5,
               height = n_levels * 0.9, units = 'cm')  
      }
      
      # (3) ORDERED FACTORS
    } else if (is.factor(df[[col]]) & is.ordered(df[[col]])){
      # We remove NA here; also note we use sym(), needed mainly to reorder the
      # bars in a loop; also note, fct_rev() is necessary to revert fct_infreq()
      p <- ggplot(data = df[col] %>% filter(!is.na(df[col])),
                  aes(x=!!sym(col)))
      # Compute percents for bars
      p <- p + geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5,
                        fill = 'black')
      if (N<100){
        # Low N warning
        p <- p + labs(title = paste(col, '| Overall'),
                      subtitle = paste0('Single-select | ', 'N = ', format(N, big.mark = ','),
                                        ', interpret with caution!'
                      ))
      } else {
        # No warning
        p <- p + labs(title = paste(col, '| Overall'),
                      subtitle = paste0('Single-select | ', 'N = ', format(N, big.mark = ',')
                      ))
      }
      # Aesthetics
      # Wrap long level names
      p <- p + scale_x_discrete(labels = function(x) str_wrap(x, width = 30))
      # Add percents as annotations; note, rounding percents using round() be-
      # haves oddly, and it is much better to use accuracy = 0.1L
      p <- p + geom_text(aes(label = scales::percent((..count..)/sum(..count..),
                                                     accuracy = 0.1L),
                             y = ((..count..)/sum(..count..))), stat="count",
                         hjust = -0.1, size=4)
      # Aesthetics
      p <- p + theme_minimal()
      p <- p + theme(axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y = element_text(colour='black', size = 12),
                     plot.margin = margin(r=25, l=1, b=0, t=1),
                     plot.title = element_text(size = 16),
                     plot.subtitle = element_text(size = 12),
                     panel.grid = element_blank())
      # Flip coordinates; note, this is needed only to accommodate the percent an-
      # notation, in combination with plot.margin(); without annotations, we might
      # have simply reversed x and y everywhere to get exact same result
      p <- p + coord_flip(clip = 'off')
      # # Print
      print(p)
      # Save as png; note n_levels*1.3+1 nicely accommodates most variables for a
      # MS Word output; note we decided to number files just to preserve df order
      # instead of alphabetical order
      if (n_levels<15) {
        ggsave(paste(counter, '_', col, '.png'), path = 'plots/single_select/overall',
               width = 16.5,
               height = n_levels * 1.3 + 1, units = 'cm')
        # If factor has more than 13 levels, it is better to use this proportion
      } else {
        ggsave(paste(counter, '_', col, '.png'), path = 'plots/single_select/overall',
               width = 16.5,
               height = n_levels * 0.9, units = 'cm')  
      }
      
      # (4) NUMERIC
    } else if (is.numeric(df[[col]])) {
      # Plot
      p <- ggplot(data = df, aes(y = !!sym(col), x=""))
      p <- p + geom_jitter(width = 0.2, alpha=0.8, size=0.6)
      p <- p +  geom_boxplot(alpha=0.9, fill='transparent')
      #p <- p + scale_y_continuous(breaks = round(seq(min(df[[col]]), max(df[[col]]), by = 10),1))
      # PUT above line once data are clean! Currently AmountSpent is infinite
      p <- p + coord_flip()
      
      if (N<100){
        # Low N warning
        p <- p + labs(title = paste(col, '| Overall'),
                      subtitle = paste0('Single-select | ', 'N = ', format(N, big.mark = ','),
                                        ', interpret with caution!' 
                      ))
      } else {
        # No warning
        p <- p + labs(title = paste(col, '| Overall'),
                      subtitle = paste0('Single-select | ', 'N = ', format(N, big.mark = ',')
                      ))
      }
      
      # Aesthetics
      p <- p + theme_minimal()
      p <- p + theme(axis.title.y = element_blank(),
                     axis.text.y = element_text(colour='black', size = 12),
                     axis.text.x = element_text(colour='black', size = 12),
                     axis.title.x = element_text(colour='black', size = 12),
                     legend.position = 'none',
                     #plot.margin = margin(r=25, l=1, b=0, t=1),
                     plot.title = element_text(size = 16),
                     plot.subtitle = element_text(size = 12),
                     panel.grid = element_blank())
      # Print
      print(p)
      
      # Export
      ggsave(paste(counter, '_', col, '.png'), path = 'plots/single_select/overall', 
             width = 16.5,
             height = 5, units = 'cm') # initially *1.3 
      
      # (5) OTHER
    } else { # reput to above line
      print(paste('Column', col, 'is a character; not plotted'))
    } # if-else end
  } # for-loop end
  
} # fun end

# Implement on full data
make_plots_overall(df)
# Implement on custom data
#make_plots_overall(df[df$City=='', ]) 

# Recode
levels(as.factor(df$BusinessSucess))
levels(as.factor(df$BusinessProfitability))

model1 <- df %>% mutate(
  BusinessSucess =
    case_when(BusinessSucess == 'Actuellement ouvert et les activités marchent bien' ~ 'High',
              BusinessSucess != 'Actuellement ouvert et les activités marchent bien' ~ 'Low'),
  BusinessProfitability =
    case_when(BusinessProfitability == 'Oui' ~ 'High',
              BusinessProfitability != 'Oui' ~ 'Low',)
)

levels(as.factor(model1$BusinessSucess))
levels(as.factor(model1$BusinessProfitability))

colSums(is.na(df))

# Export
write_excel_csv(model1, 'jamovi/model1.csv')

















