getwd()
library(readxl)
library(tidyverse)


# Load res object as df, and convert strings to factors
df <- readRDS('data_clean/res_slim.rds')%>% 
  mutate(across(where(is.character), as.factor))
dim(df)

# Discard unplottable variables
df <- df %>% select(-c(MimosaID, InterviewDate, TrainingStart, TrainingEnd))
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



















