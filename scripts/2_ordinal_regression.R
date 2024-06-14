# Author: Samvida S. Venkatesh
# Date: 14/06/24

library(tidyverse)
library(RColorBrewer)
library(MASS)
library(broom)
theme_set(theme_bw())

ou_df <- read.table("data/cleaned/oxford_all_years.txt",
                    sep = "\t", header = T, stringsAsFactors = F)

SALARY_BANDS <- c("Band_3", "Band_4", "Band_5", "Band_6")
SEXES <- c("Female", "Male")
FUNCTIONS <- c("Both teaching and research", "Teaching only", "Research only")

model_df <- ou_df %>%
  filter(Contract_levels != "All") %>%
  dplyr::select(all_of(c("Academic_employment_function", "Sex",
                  "Number", "Salary_Band"))) %>%
  filter(Sex %in% SEXES) %>%
  filter(Academic_employment_function %in% FUNCTIONS) %>%
  mutate(Salary_Band = factor(Salary_Band, levels = SALARY_BANDS,
                              ordered = T),
         Academic_employment_function = factor(Academic_employment_function,
                                               levels = FUNCTIONS)) %>%
  filter(Number > 0 & !is.na(Salary_Band))

# Since the data is aggregated, we need to disaggregate it into individual
# rows ("fake individuals")

expanded_df <- model_df[rep(row.names(model_df), model_df$Number), c(1,2,4)]

# Formulas to test:
# 1. how much variance in salary band can be explained by: academic role and sex
# 2. how much variance in salary band can be explained by: academic role, sex, and 
# the interaction between role and sex?

baseline_form <- "Salary_Band ~ Academic_employment_function"
interaction_form <- "Salary_Band ~ Academic_employment_function + Sex + Academic_employment_function:Sex"

baseline_mod <- polr(formula(baseline_form), 
                     data = expanded_df, Hess = T)

tidy(baseline_mod)

pred_df <- data.frame(Sex = rep(SEXES, times = 3),
                      Academic_employment_function = rep(FUNCTIONS), each = 2)
pred_df$predicted_prob <- predict(baseline_mod,
                                  pred_df, type = "p")



