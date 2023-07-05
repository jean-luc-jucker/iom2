getwd()
library(readxl)
library(tidyverse)


# Load res object as df, and convert strings to factors
df <- readRDS('data_clean/res_slim.rds')%>% 
  mutate(across(where(is.character), as.factor))
dim(df) # 2,026 x 33

# Discard unplottable variables
df <- df %>% select(-c(MimosaID, InterviewDate))
dim(df)

df

df %>% group_by(Country) %>% summarise(count = n())


bs <- df %>%
  select(Country, BusinessSuccess) %>% 
  add_count(Country) %>% 
  filter(BusinessSuccess =='High')  %>% 
  group_by(Country, n) %>% 
  summarise(Count = n()) %>% 
  mutate(`High Business Success` = Count/n) %>% 
  select(Country, `High Business Success`)
bs


bp <- df %>%
  select(Country, BusinessProfitability) %>% 
  add_count(Country) %>% 
  filter(BusinessProfitability =='High')  %>% 
  group_by(Country, n) %>% 
  summarise(Count = n()) %>% 
  mutate(`High Business Profitability` = Count/n) %>% 
  select(Country, `High Business Profitability`)
bp


plot <- merge(bs, bp, by='Country')
plot

plot$Country <- fct_reorder(plot$Country, plot$`High Business Success`)

plot


plot <- plot %>% pivot_longer(-Country, names_to = 'Dimension', values_to = 'Score')
plot




ggplot(plot, aes(fill= Dimension, y=Score, x= Country, Score)) + 
  geom_bar(position="dodge", stat = "identity")+
  coord_flip()+
  guides(fill = guide_legend(reverse = TRUE))+
  scale_fill_manual(values = c('#3399FF', 'navyblue'))+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(colour='black', size = 18),
        axis.text.x = element_text(colour='black', size = 18),
        #axis.title.x = element_text(colour='black', size = 16),
        legend.position = 'right',
        #legend.position = c(0.95, 1.05),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        plot.margin = margin(r=25, l=1, b=0, t=1),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 18))
#panel.grid = element_blank())









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
#make_plots_overall(df[df$=='', ]) 


# 2. By BusinessSuccess ##################################################################

# Counter
counter <- 0

# Function
make_plots_by_BusinessSuccess <- function(df){ # fun start
  
  # FOR-LOOP
  
  for (col in colnames(df)){ # for start
    
    # (1) Some variables we'll need:
    # Count of non-na rows; needed for plot title
    N <- sum(!is.na(df[col]))
    # Count of missing observations; needed for plot title
    Missing <- sum(is.na(df[col]))
    # Number of levels for each factor; needed for saving plots in correct aspect
    # ratio
    n_levels <- dim(unique(df[col]))[1]
    # Number of grouping levels; needed for saving plot
    n_grouping <- dim(unique(df['BusinessSuccess']))[1]
    # Update counter; needed for plot numbering
    counter <- counter + 1
    
    # Skip items with 0 observations
    # This is crucial to avoid error when wrapping long level names (these cannot
    # be wrapped in there are none)
    if (N == 0) next
    
    # (2) FACTOR VARIABLES (i.e., factors that are not ordered)
    if (is.factor(df[[col]]) & !is.ordered(df[[col]])) { # if start
      
      p <- ggplot(data = df %>% filter(!is.na(df[col])),
                  aes(x = fct_rev(fct_infreq(!!sym(col))), fill =
                        fct_rev(BusinessSuccess))) # BusinessSuccess needs to be reversed
      # just so that guide_legend can then be reversed
      
      
      
      
      
      
      # NEW (replace the above):
      #p <- ggplot(data = df %>% filter(!is.na(df[col])),
      #            aes(fill = fct_rev(fct_infreq(!!sym(col))), x = 
      #                  fct_rev(BusinessSuccess))) # BusinessSuccess needs to be reversed
      # just so that guide_legend can then be reversed
      
      
      
      
      
      
      # NEW, Below, replaced all ..fill.. by ..x..
      p <- p + geom_bar(aes(y=..count../tapply(..count.., ..x.. ,sum)[..x..]), 
                        position='dodge', width = 0.75, size = 3) # width initially 0.5
      p <- p + geom_text(aes(y=..count../tapply(..count.., ..x.. ,sum)[..x..],
                             label=scales::percent(..count../tapply(..count.., ..x.., sum)
                                                   [..x..], accuracy = 0.1L)),
                         stat="count", position=position_dodge(0.7), hjust=-0.05)
      
      # NEW (no longer needed)
      #p <- p + scale_fill_manual(values = c('#49C16DFF', '#FBA238FF', '#38598CFF'))
      # Important to use fill here, not color!
      p <- p + guides(fill = guide_legend(reverse = TRUE))
      # Generate title and subtitle using string literals
      if (N < 100) {
        p <- p + labs(title = paste(col, '| by BusinessSuccess'),
                      subtitle = paste0('Single-select | ', 'N = ', format(N, big.mark = ','),
                                        ', interpret with caution!' ))
      } else {
        p <- p + labs(title = paste(col, '| by BusinessSuccess'),
                      subtitle = paste0('Single-select | ', 'N = ', format(N, big.mark = ',')
                      ))
      }
      # Wrap long level names
      p <- p + scale_x_discrete(labels = function(x) str_wrap(x, width = 30))
      
      # Aesthetics
      p <- p + theme_minimal()
      p <- p + theme(axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y = element_text(colour='black', size = 12),
                     legend.position = 'right',
                     #legend.position = c(0.95, 1.05),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 12),
                     plot.margin = margin(r=25, l=1, b=0, t=1),
                     plot.title = element_text(size = 16),
                     plot.subtitle = element_text(size = 12),
                     panel.grid = element_blank())
      p <- p + coord_flip(clip = 'off')
      print(p)
      # Export
      if (n_levels<15) {
        ggsave(paste(counter, '_', col, '.png'), path = 'plots/single_select/by_BusinessSuccess', 
               width = 16.5,
               height = n_levels * 1.8 + 1, units = 'cm')
        # If factor has more than 13 levels, it is better to use this proportion, though
      } else {
        ggsave(paste(counter, '_', col, '.png'), path = 'plots/single_select/by_BusinessSuccess', 
               width = 16.5,
               height = n_levels * 1.3, units = 'cm')  
      }
      
      # (3) ORDERED VARIABLES (i.e., factors that are ordered)
    } else if (is.factor(df[[col]]) & is.ordered(df[[col]])){
      
      p <- ggplot(data = df %>% filter(!is.na(df[col])),
                  aes(x = !!sym(col), fill = # No longer needs to be reversed
                        fct_rev(BusinessSuccess))) # BusinessSuccess needs to be reversed
      # just so that guide_legend can then be reversed
      p <- p + geom_bar(aes(y=..count../tapply(..count.., ..fill.. ,sum)[..fill..]),
                        position='dodge', width = 0.75, size = 3) # width initially 0.5
      p <- p + geom_text(aes(y=..count../tapply(..count.., ..fill.. ,sum)[..fill..],
                             label=scales::percent(..count../tapply(..count.., ..fill.., sum)
                                                   [..fill..], accuracy = 0.1L)),
                         stat="count", position=position_dodge(0.7), hjust=-0.05)#,
      # = 'A')  # dodge init 0.5
      p <- p + scale_fill_manual(values = c('#49C16DFF', '#FBA238FF', '#38598CFF'))
      # Important to use fill here, not color!
      p <- p + guides(fill = guide_legend(reverse = TRUE))
      
      # Generate title and subtitle using string literals
      if (N < 100) {
        p <- p + labs(title = paste(col, '| by BusinessSuccess'),
                      subtitle = paste0('Single-select | ', 'N = ', format(N, big.mark = ',')
                                        , ', interpret with caution!' ))
      } else {
        p <- p + labs(title = paste(col, '| by BusinessSuccess'),
                      subtitle = paste0('Single-select | ', 'N = ', format(N, big.mark = ',')
                      ))
      }
      # Wrap long level names
      p <- p + scale_x_discrete(labels = function(x) str_wrap(x, width = 30))
      
      # Aesthetics
      p <- p + theme_minimal()
      p <- p + theme(axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y = element_text(colour='black', size = 12),
                     legend.position = 'right',
                     #legend.position = c(0.95, 1.05),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 12),
                     plot.margin = margin(r=25, l=1, b=0, t=1),
                     plot.title = element_text(size = 16),
                     plot.subtitle = element_text(size = 12),
                     panel.grid = element_blank())
      p <- p + coord_flip(clip = 'off')
      print(p)
      # Export
      if (n_levels<15) {
        ggsave(paste(counter, '_', col, '.png'), path = 'plots/single_select/by_BusinessSuccess', 
               width = 16.5,
               height = n_levels * 1.8 + 1, units = 'cm')
        # If factor has more than 13 levels, it is better to use this proportion, though
      } else {
        ggsave(paste(counter, '_', col, '.png'), path = 'plots/single_select/by_BusinessSuccess', 
               width = 16.5,
               height = n_levels * 1.3, units = 'cm')  
      }
      
      # (3) NUMERIC VARIABLES
    }  else if (is.numeric(df[[col]])) {  # if end; else if start
      
      p <- ggplot(data = df, aes(y = !!sym(col), x = fct_rev(BusinessSuccess), 
                                 fill = BusinessSuccess))
      p <- p + geom_jitter(aes(color=BusinessSuccess), width = 0.2, alpha=1, size=0.6)
      p <- p +  geom_boxplot(alpha=0.9)
      p <- p + scale_fill_manual(values = c('#38598CFF', '#FBA238FF', '#49C16DFF')) # note
      # colors are reversed!
      p <- p + scale_color_manual(values = c('#38598CFF', '#FBA238FF', '#49C16DFF'))
      #p <- p + guides(fill = guide_legend(reverse = TRUE))
      p <- p + coord_flip()
      #p <- p + scale_y_continuous(breaks = round(seq(min(df[[col]]), max(df[[col]]), by = 10),1))
      # PUT above line once data are clean! Currently AmountSpent is infinite
      # Generate title and subtitle using string literals
      if (N < 100) {
        p <- p + labs(title = paste(col, '| by BusinessSuccess'),
                      subtitle = paste0('Single-select | ', 'N = ', format(N, big.mark = ',')
                                        , ', interpret with caution!' ))
      } else {
        p <- p + labs(title = paste(col, '| by BusinessSuccess'),
                      subtitle = paste0('Single-select | ', 'N = ', format(N, big.mark = ',')
                      ))
      }
      # Aesthetics
      p <- p + theme_minimal()  # base_ = 'A'
      p <- p + theme(axis.title.y = element_blank(),
                     axis.text.y = element_text(colour='black', size = 12),
                     axis.text.x = element_text(colour='black', size = 12),
                     axis.title.x = element_text(colour='black', size = 12),
                     legend.position = 'none',
                     #plot.margin = margin(r=25, l=1, b=0, t=1),
                     plot.title = element_text(size = 16),
                     plot.subtitle = element_text(size = 12),
                     panel.grid = element_blank())
      print(p)
      
      # Export
      ggsave(paste(counter, '_', col, '.png'), path = 'plots/single_select/by_BusinessSuccess', 
             width = 16.5,
             height = n_grouping * 3 + 1, units = 'cm') # initially *1.3 
      
      # OTHER VARIABLES  
    } else { # else if end; else start
      print(paste('Column', col, 'is a character; not plotted'))
    } # else end
    
  } # for end
  
} # func end

# Test on mini df
#make_plots_by_BusinessSuccess(mini)
# Implement on full data
make_plots_by_BusinessSuccess(df)
# Implement on custom data
#make_plots_by_BusinessSuccess(df[df$Gender=='Man', ]) 

















