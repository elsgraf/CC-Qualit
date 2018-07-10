
# ELSG 10/07/2018
# This script was written as part of the Coding Club tutorial on handling qualitative data 
# https://ourcodingclub.github.io/2018/01/29/qualitative.html

# Set working directory
setwd("Y:/CodingClub/CC-Qualit")

# Load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(tidytext)
library(R.utils)
library(wordcloud)

# The survey responses
sust_data <- read.csv("sust_behaviour.csv")

# A lookup table which connects each column in `sust_data` to the actual question on the survey
sust_lookup <- read.csv("sust_lookup.csv")

# A list of boring and non-useful words, bundled with `tidytext`
data(stop_words)

# This orders the ordinal responses
sust_data$sustainability_daily_think <- factor(sust_data$sustainability_daily_think,
                                               levels = c("Never", "Rarely", "Sometimes", "Often", "All the time"), 
                                               ordered = TRUE)

sust_data$interest_sustainability <- factor(sust_data$interest_sustainability,
                                            levels = c("No", "Somewhat", "Yes"), 
                                            ordered = TRUE)

sust_data$energy_action_often <- factor(sust_data$energy_action_often,
                                            levels = c("Never", "Sometimes", "Often", "All the time"), 
                                            ordered = TRUE)

sust_data$water_action_often <- factor(sust_data$water_action_often,
                                        levels = c("Never", "Sometimes", "Often", "All the time"), 
                                        ordered = TRUE)

sust_data$food_action_often <- factor(sust_data$food_action_often,
                                        levels = c("Never", "Sometimes", "Often", "All the time"), 
                                        ordered = TRUE)

sust_data$waste_action_often <- factor(sust_data$waste_action_often,
                                        levels = c("Never", "Sometimes", "Often", "All the time"), 
                                        ordered = TRUE)

sust_data$other_action_often <- factor(sust_data$other_action_often,
                                        levels = c("Never", "Sometimes", "Often", "All the time"), 
                                        ordered = TRUE)

# This counts the number of sustainable actions for each response
sust_data$energy_action_n <- nchar(as.character(sust_data$energy_action))
sust_data$water_action_n <- nchar(as.character(sust_data$water_action))
sust_data$food_action_n <- nchar(as.character(sust_data$food_action))
sust_data$waste_action_n <- nchar(as.character(sust_data$waste_action))
sust_data$other_action_n <- nchar(as.character(sust_data$other_action))

# Make a summary dataframe for sustainability_daily_think using dplyr
sust_think_summ_wide <- sust_data %>%
  group_by(gender, sustainability_daily_think) %>%
  tally() %>%
  mutate(perc = n / sum(n) * 100) %>%
  dplyr::select(-n) %>%
  group_by(gender) %>%
  spread(sustainability_daily_think, perc)

# Use dplyr to split "Sometimes" responses into high & low group, so this group can straddle zero line in diverging stacked bar chart
sust_think_summ_hi_lo <- sust_think_summ_wide %>%
  mutate(midlow = Sometimes / 2,
         midhigh = Sometimes / 2) %>%
  dplyr::select(gender, Never, Rarely, midlow, midhigh, Often, `All the time`) %>%
  gather(key = response, value = perc, 2:7) %>%
  `colnames <-`(c("gender", "response", "perc"))



