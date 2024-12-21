######################################################################
# Models and Visuals 
# Input: ESS_1to11_cleaned_imputed.rds 
# Output: 
# setwd("/Users/hyeriseo/Documents/GitHub/Public-Opinion-Fall-24")
######################################################################

rm(list=ls())
# Packages
library(dplyr)
library(tidyverse)
library(rio)
library(tidyr)
library(lubridate)
library(labelled)
library(modelsummary)
library(interplot)
library(texreg)
library(aod)
library(ggplot2)
library(dotwhisker)
library(gridExtra)
library(ggeffects)

# Directory and data
setwd("/Users/hyeriseo/Documents/GitHub/Public-Opinion-Fall-24")
data1 <- readRDS("Data/ESS_1to11_cleaned_imputed.rds")   #round 1 to 11

###### Setups
# factor variables 
data1$legislation_factor <- factor(data1$legislation)
data1$cntry_factor <- factor(data1$cntry)
data1$surveyyear_factor <- factor(data1$surveyyear)
data1$essround_factor <- factor(data1$essround)

###### multilevel modeling 
# Round 1 to 11 data
library(lme4)
model1 <- glmer(agree_gay ~ legislation_factor + male + agea_NAfill + income_imputed + education +
                married_NAfill + religiosity_NAfill + ideology_right10_NAfill + 
                (1|cntry_factor) + (1|essround_factor),
                data=data1, family=binomial(link = "logit"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(model1)
saveRDS(model1, file = "Data/multilevel-agree-legislation-male-age-income-education-married-religiosity-ideology.rds")

model2 <- glm(agree_gay ~ legislation_factor + male + agea_NAfill + income_imputed + education +
              married_NAfill + religiosity_NAfill + ideology_right10_NAfill + 
              cntry_factor + essround_factor,
              data=data1, family=binomial(link = "logit"))

summary(model2)
saveRDS(model2, file = "Data/twoway-agree-legislation-male-age-income-education-married-religiosity-ideology.rds")

# Interaction term
model3 <- glmer(agree_gay ~ legislation_factor + legislation_factor*galtan + galtan + male +
                agea_NAfill + income_imputed + education + married_NAfill +
                religiosity_NAfill + ideology_right10_NAfill + 
                (1|cntry_factor) + (1|essround_factor), data=data1, 
                family=binomial(link = "logit"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(model3)
saveRDS(model3, file = "Data/multilevel-agree-legislation-galtan-interaction-male-age-income-education-married-religiosity-ideology.rds")


model4 <- glm(agree_gay ~ legislation_factor + legislation_factor*galtan + galtan + male +
                  agea_NAfill + income_imputed + education + married_NAfill +
                  religiosity_NAfill + ideology_right10_NAfill + 
                  cntry_factor + essround_factor, data=data1, 
                family=binomial(link = "logit"))

summary(model4)
saveRDS(model4, file = "Data/twoway-agree-legislation-galtan-interaction-male-age-income-education-married-religiosity-ideology.rds")



###### Graphs
  # Attitudes toward homosexuality across countries by year
          
    country_legislation_year <- tibble(
      country_full_name = c("Netherlands", "Finland", "Ireland", "Switzerland", "Austria", "Belgium", "Germany", "Denmark",
                            "Spain", "France", "United Kingdom", "Norway", "Portugal", "Sweden", "Slovenia", "Iceland", "Luxembourg",
                            "Bulgaria", "Croatia", "Cyprus", "Czechia", "Estonia", "Greece", "Hungary", "Israel", "Italy",
                            "Latvia", "Lithuania", "Montenegro", "Poland", "Russia", "Serbia", "Slovakia", "Turkey", 
                            "Ukraine"),
      legislation_year = c(2001, 2017, 2015, 2022, 2019, 2003, 2017, 2012, 2005, 2013, 2014, 2009, 2010, 2009, 2022, 2010, 2015,
                           NA, NA, NA, NA, NA, NA, NA, NA, NA,
                           NA, NA, NA, NA, NA, NA, NA, NA, NA))
    
    df_filtered <- data1 %>%
      group_by(country_full_name, surveyyear, agree_gay) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(country_full_name, surveyyear) %>%
      mutate(proportion = count / sum(count)) %>%
      ungroup()
    
    df_filtered <- df_filtered %>%
      left_join(country_legislation_year %>%
                  mutate(country_full_name = as.character(country_full_name)), 
                by = "country_full_name") %>%
      mutate(legislation_year = ifelse(is.na(legislation_year), Inf, legislation_year)) %>%
      mutate(country_full_name = fct_reorder(country_full_name, legislation_year)) 
    
    levels(df_filtered$country_full_name)
    
    vline_data <- tibble(
      country_full_name = c("Iceland", "Denmark", "Luxembourg", "Slovenia"), 
      surveyyear = c(2010, 2012, 2015, 2022)  # The years I want to add vertical lines for
    )
    

    df_filtered$agree_disagree <- factor(df_filtered$agree_gay, levels = c(0, 1), labels = c("Neither/Disagree", "Agree"))
    
    # Create the plot
    ggplot(df_filtered %>% filter(!is.na(agree_disagree)), aes(x = surveyyear, y = proportion, 
                                                          color = factor(agree_gay), group = agree_gay)) +
      geom_point(na.rm = TRUE, size = 0.3) + geom_line() +
      facet_wrap(~ reorder(country_full_name, legislation_year), scales = "fixed", ncol = 5) +
      labs(
        title = "",
        x = "Year",
        y = "Proportion of Agree/Disagree",
        color = "Pro-Gay Attitude"
      ) +
      geom_vline(data = df_filtered %>% filter(country_full_name == "Finland" & surveyyear == 2017),
                 aes(xintercept = surveyyear), color = "black", size = 1, alpha = 0.6, linetype = "dotted") +
      geom_vline(data = df_filtered %>% filter(country_full_name == "Ireland" & surveyyear == 2015),
                 aes(xintercept = surveyyear), color = "black", size = 1, alpha = 0.6, linetype = "dotted") +
      geom_vline(data = df_filtered %>% filter(country_full_name == "Switzerland" & surveyyear == 2022),
                 aes(xintercept = surveyyear), color = "black", size = 1, alpha = 0.6, linetype = "dotted") +
      geom_vline(data = df_filtered %>% filter(country_full_name == "Austria" & surveyyear == 2019),
                 aes(xintercept = surveyyear), color = "black", size = 1, alpha = 0.6, linetype = "dotted") +
      geom_vline(data = df_filtered %>% filter(country_full_name == "Belgium" & surveyyear == 2003),
                 aes(xintercept = surveyyear), color = "black", size = 1, alpha = 0.6, linetype = "dotted") +
      geom_vline(data = df_filtered %>% filter(country_full_name == "Germany" & surveyyear == 2017),
                 aes(xintercept = surveyyear), color = "black", size = 1, alpha = 0.6, linetype = "dotted") +
      geom_vline(data = df_filtered %>% filter(country_full_name == "Spain" & surveyyear == 2005),
                 aes(xintercept = surveyyear), color = "black", size = 1, alpha = 0.6, linetype = "dotted") +
      geom_vline(data = df_filtered %>% filter(country_full_name == "France" & surveyyear == 2013),
                 aes(xintercept = surveyyear), color = "black", size = 1, alpha = 0.6, linetype = "dotted") +
      geom_vline(data = df_filtered %>% filter(country_full_name == "United Kingdom" & surveyyear == 2014),
                 aes(xintercept = surveyyear), color = "black", size = 1, alpha = 0.6, linetype = "dotted") +
      geom_vline(data = df_filtered %>% filter(country_full_name == "Norway" & surveyyear == 2009),
                 aes(xintercept = surveyyear), color = "black", size = 1, alpha = 0.6, linetype = "dotted") +
      geom_vline(data = df_filtered %>% filter(country_full_name == "Portugal" & surveyyear == 2010),
                 aes(xintercept = surveyyear), color = "black", size = 1, alpha = 0.6, linetype = "dotted") +
      geom_vline(data = df_filtered %>% filter(country_full_name == "Sweden" & surveyyear == 2009),
                 aes(xintercept = surveyyear), color = "black", size = 1, alpha = 0.6, linetype = "dotted") +
      geom_vline(data = df_filtered %>% 
          filter(country_full_name %in% vline_data$country_full_name),  # Filter for countries and years
        aes(xintercept = legislation_year), 
        color = "black", size = 1, alpha = 0.6, linetype = "dotted", inherit.aes = FALSE
      ) +
      theme_minimal() +
      theme(legend.box = "horizontal",   # Arrange the legend items horizontally
            legend.box.margin = margin(t = 10),  # Add some space between the legend and the plot
            legend.title = element_text(size = 10),  # Adjust the legend title size
            legend.text = element_text(size = 10),   # Adjust the legend text size
            legend.position = c(0.7, 0.05)) +
      scale_x_continuous(limits = c(2002, 2024), breaks = seq(2002, 2024, by = 4)) +
      guides(color = guide_legend(reverse = TRUE))


####### dotwhisker
m1 <- readRDS("Data/multilevel-agree-legislation-male-age-income-education-married-religiosity-ideology.rds")
m2 <- readRDS("Data/multilevel-agree-legislation-galtan-interaction-male-age-income-education-married-religiosity-ideology.rds")
    
x_limits <- c(-0.5, 0.8)

dwplot(list(m2, m1), 
       dodge_size = 0.4,
       line_args = list(alpha = 0.75, size = 2),
       whisker_args = list(size = 1),
       dot_args = list(size = 2.5),
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
       vars_order = c("legislation_factor1", "legislation_factor2",
                          "male", "agea_NAfill", "income_imputed", "education",
                          "married", "religiosity_NAfill", "ideology_right10_NAfill",
                       "galtan", "legislation_factor1:galtan", "legislation_factor2:galtan")) |>  
      relabel_predictors(
        c(legislation_factor1 = "Legislation without direct democracy",
          legislation_factor2 = "Legislation with direct democracy",
          male = "Male",
          agea_NAfill = "Age",
          income_imputed = "Income",
          religiosity_NAfill = "Religiosity",
          education = "Education",
          married = "Married",
          ideology_right10_NAfill = "Ideology-right",
          galtan = "Partisanship (GAL-TAN)",
          "legislation_factor1:galtan" = "Legislation without direct democracy \nx Partisanship (GAL-TAN)",
          "legislation_factor2:galtan" = "Legislation with direct democracy \nx Partisanship (GAL-TAN)"
          )) +
scale_color_manual(values = c("#F96B6C", "#FFAA7B"),
                   labels = c("Model 2", "Model 1"), 
                     guide = guide_legend(title = "Models")) +
  labs(caption = "The number of countries: 39\nThe number of ESS rounds: 11\nError bars indicate 95% confidence interval", size = 1) +
     theme_bw(base_size = 18) +
      xlab("Coefficient Estimate") + ylab("covariates") +
      geom_vline(xintercept = 0,
                 colour = "grey60",
                 linetype = 2) +
  coord_cartesian(xlim = x_limits) + 
      ggtitle("") +
      theme(
        plot.title = element_text(face = "plain", hjust = 0.5, size = 12),
        legend.position = c(0.8, 0.07),
        legend.justification = c(0.5, 0.5),
        legend.background = element_rect(colour = "grey80"),
        legend.title = element_blank(),
        plot.margin = margin(t = 9, r = 9, b = 9, l = 9),
        legend.text = element_text(size = 14),
        legend.box = "vertical"
      ) 




###### Exclude countries never adopted
countries_to_keep1 <- c("AT", "BE", "CH", "DE", "DK", "ES", "FI", "FR", "GB", "IE", "IS", "NO", "PT", "SE", "SI") # round 1 to 11
countries_to_keep2 <- c("AT", "BE", "DE", "DK", "ES", "FI", "FR", "GB", "IE", "IS", "NO", "PT", "SE") # round 1 to 10: Swiss and Slovenia excluded
df_1 <- data1 %>%
  filter(cntry %in% countries_to_keep1)
df_2 <- data1 %>%
  filter(cntry %in% countries_to_keep2)

m3 <- glmer(agree_gay ~ legislation_factor + male + agea_NAfill + income_imputed + education +
            married_NAfill + religiosity_NAfill + ideology_right10_NAfill + 
            (1|cntry_factor) + (1|essround_factor),
            data=df_1, family=binomial(link = "logit"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

m4 <- glmer(agree_gay ~ legislation_factor + legislation_factor*galtan + galtan + male +
              agea_NAfill + income_imputed + education + married_NAfill +
              religiosity_NAfill + ideology_right10_NAfill + 
              (1|cntry_factor) + (1|essround_factor), data=df_2, 
            family=binomial(link = "logit"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

saveRDS(m3, file="Data/multilevel-country-sub.rds")
saveRDS(m4, file="Data/multilevel-country-sub-interaction.rds")
