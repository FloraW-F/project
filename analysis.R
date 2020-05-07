# analysis-setup
library(ggplot2)
library(magrittr)
individual <- readr::read_csv(here::here("data", "individual.csv")) %>%
  dplyr::select(stem_diameter, height, growth_form)
print(individual) 

# analysis-filter-data
analysis_df <- individual %>%
  dplyr::filter(!is.na(growth_form), growth_form != "liana") #removing-NAs-and-lianas

# analysis-set-factor-levels
gf_levels <- table(analysis_df$growth_form) %>% #table tabulates counts of a variable, producing vector of counts
  sort() %>% #sort counts in ascending order
  names() #gives the names in ascending order
#turning growth form in to a factor, with assignment pipe
analysis_df %<>%
  dplyr::mutate(growth_form = factor(growth_form,
                                     levels = gf_levels))

# analysis-fig1-barplot
analysis_df %>% 
  ggplot(mapping = aes(y = growth_form, colour = growth_form, fill = growth_form)) + #flip coordinates to stop labels overlapping
  geom_bar(alpha = 0.5, show.legend = FALSE) #can see scale through bars, and turn of legend

# analysis-fig2-violinplots
analysis_df %>%
  tidyr::pivot_longer(cols = c(stem_diameter, height), #stacking stem_diameter and height into same column
                      values_to = "value",   #call that column value
                      names_to = "var") %>%
  ggplot(aes(x = log(value), y = growth_form, colour = growth_form, fill = growth_form)) +
  geom_violin(alpha = 0.5, trim = TRUE, legend = F) +
  geom_boxplot(alpha = 0.7, legend = F) +
  facet_grid(~var)

# analysis-lm-overall
lm_overall <- lm(log(stem_diameter) ~ log(height), analysis_df)
lm_overall %>% 
  broom::glance() #gives p-value, R2
lm_overall %>% #gives std errors ect
  broom::tidy()

# analysis-lm-fig3-overall
analysis_df %>%
  ggplot(mapping = aes(x = log(height), y = log(stem_diameter))) +  #want x to be predictor variable
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm") #without mthod get fitted line

#analysis-lm-growth
lm_growth <- lm(log(stem_diameter) ~ log(height) * growth_form, analysis_df) #* gives interaction between height and growth
lm_growth %>%
  broom::glance() #has better rsq than height as explanatory alone
lm_growth %>%
  broom::tidy()

#analysis-lm-fig4-growth
analysis_df %>%
  ggplot(mapping = aes(x = log(height), y = log(stem_diameter), colour = growth_form)) +  #include growth_form as colour
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") #without mthod get fitted line