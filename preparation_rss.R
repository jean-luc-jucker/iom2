getwd()
library(readxl)
library(tidyverse)

# Reintegration Economic Survey ####
rss <- read_excel('data_raw/RSS Kobo  - All data set + MimosaV2 bis.xlsx',
                  na = c('N/A', 'NA', 'na'), skip = 2) 
warnings()
dim(rss) # 1,361 x 319
rss

# Subset ######################################################################
df <- rss %>% 
  
  # PULL ######################################################################
  select(
    # Metadata

    # Dependent variables
    "2. A quelle fréquence avez-vous dû réduire la quantité ou la qualité des aliments que vous mangez en raison de leur coût au cours du mois passe (des 30 derniers jours)?",
    "3. Avez vous la possibilité emprunter de l’argent si vous en avez besoin?\r\n(Perception de la disponibilité du crédit, quelle que soit la source - banque, famille, amis, système de prêts traditionnel, microcrédit, etc. - et peu importe si le répondant prend effectivement des prêts ou non)",
    "4. Empruntez-vous de l’argent? À quelle fréquence?\r\n(Comportement autodéclaré par le répondant, peu importe la source du crédit et le montant – même les très petits montants comptent)",
    "5. En moyenne, quel est le montant le plus élevé : vos dépenses chaque mois ou votre dette" 
    
    

    # Grouping variables

    ) %>% 
  
  # RENAME ####################################################################
  rename(
    # Metadata

    # Dependent variables
    "2_food" =
      "2. A quelle fréquence avez-vous dû réduire la quantité ou la qualité des aliments que vous mangez en raison de leur coût au cours du mois passe (des 30 derniers jours)?",
    "3_borrow" =
      "3. Avez vous la possibilité emprunter de l’argent si vous en avez besoin?\r\n(Perception de la disponibilité du crédit, quelle que soit la source - banque, famille, amis, système de prêts traditionnel, microcrédit, etc. - et peu importe si le répondant prend effectivement des prêts ou non)",
    "4_borrow_freq" = 
      "4. Empruntez-vous de l’argent? À quelle fréquence?\r\n(Comportement autodéclaré par le répondant, peu importe la source du crédit et le montant – même les très petits montants comptent)",
    "5_debt_ratio" = 
      "5. En moyenne, quel est le montant le plus élevé : vos dépenses chaque mois ou votre dette" 
    
    
    # Grouping variables

    ) %>% 
  
  # CODE TO NUMERIC ###########################################################
  mutate(`2_food_n` =
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
           recode(`5_debt_ratio`, # Read Note 1!
                  "Les dépenses sont plus importantes" = 1,
                  "La dette est plus grande" = 0,
                  "Je souhaite ne pas répondre" = 0.5
           ),
         
    ) %>% 
  
  # REPLACE NA  ###############################################################
  # According to documentation, all NAs should be coded 0.5
  mutate(`5_debt_ratio_n` = replace_na(`5_debt_ratio_n`, 1) # Read Note 1!
         
    ) %>% 
  
  # REPLACE NA  ###############################################################
  # According to documentation, all NAs should be coded 0.5

  
  # ADD WEIGHT ################################################################
  mutate(`2_food_dim_weight` = 0.12,
         `2_food_comp_weight` = 0.08,
         
         `3_borrow_dim_weight` = 0.08,
         `3_borrow_comp_weight` = 0.02,
         
         `4_borrow_freq_dim_weight` = 0.1,
         `4_borrow_freq_comp_weight` = 0.02,
         
         `5_debt_ratio_dim_weight` = 0.08,
         `5_debt_ratio_comp_weight` = 0.04,
  
    ) %>% 
  
  # COMPUTE SCORE #############################################################
  mutate(`2_food_dim_score` = `2_food_n` * `2_food_dim_weight`,
         `2_food_comp_score` = `2_food_n` * `2_food_comp_weight`,
         
         `3_borrow_dim_score` = `3_borrow_n` * `3_borrow_dim_weight`,
         `3_borrow_comp_score` = `3_borrow_n` * `3_borrow_comp_weight`,
         
         `4_borrow_freq_dim_score` = `4_borrow_freq_n` * `4_borrow_freq_dim_weight`,
         `4_borrow_freq_comp_score` = `4_borrow_freq_n` * `4_borrow_freq_comp_weight`,
         
         `5_debt_ratio_dim_score` = `5_debt_ratio_n` * `5_debt_ratio_dim_weight`,
         `5_debt_ratio_comp_score` = `5_debt_ratio_n` * `5_debt_ratio_comp_weight`,
    
    )
  


###############################################################################
names(rss)

rss %>% group_by(`5. En moyenne, quel est le montant le plus élevé : vos dépenses chaque mois ou votre dette`) %>% summarise(Count = n())

df %>% group_by(`5_debt_ratio_n`) %>% summarise(Count = n())
view(df)
###############################################################################




# TEMP ########################################################################

`XXX_n` =
  recode(`XXX`,
         "" = ,
         "" = ,
         "" = ,
         "" = ,
         "" = ,
         "" = 
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



view(rss)




# Notes ####

# (1) NA should normally be coded as 0.5. That said, the French version of RSS
# is missing an answer option compared to the English version, which is "I don't
# have debts". Since all of the NAs replied that they do not have debt in the
# previous question ("4b. Avez-vous actuellement une dette à rembourser?"), it
# is likely that the NA mean that the respondents do not have debts. If that is
# the case, they should be coded as 1 following the documentation, which is what
# was done here.
# However, we note that 657 who also replied not having debt in question 4b have
# an answer for question 5, which is odd. I therefore suggest to investigate
# further how question 5 was coded, since it might contain an anomaly, which might
# biase the final scores.





















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

















