# Author: Samvida S. Venkatesh
# Date: 14/06/24

library(tidyverse)
library(RColorBrewer)
library(lme4)
library(broom)
library(MASS)
theme_set(theme_bw())

ou_df <- read.table("data/cleaned/oxford_all_years.txt",
                    sep = "\t", header = T, stringsAsFactors = F)

SALARY_BANDS <- c("Band_1", "Band_2", "Band_3",
                 "Band_4", "Band_5", "Band_6")
SEXES <- c("Female", "Male")
FUNCTIONS <- c("Teaching only", "Research only",
               "Both teaching and research")

model_df <- ou_df %>%
  select(all_of(c("Academic_employment_function", "Sex",
                  "Number", "Salary_Band"))) %>%
  filter(Sex %in% SEXES) %>%
  filter(Academic_employment_function %in% FUNCTIONS) %>%
  mutate(Salary_Band = factor(Salary_Band, levels = SALARY_BANDS,
                              ordered = T))

# Since the data is aggregated, we need to disaggregate it into individual
# rows ("fake individuals")

expanded_df <- model_df %>%
  filter(Number > 0 & !is.na(Salary_Band)) %>%
  rowwise() %>%
  mutate(rep = list(replicate(Number, 
                              cur_data(), 
                              simplify = FALSE))) %>%
  unnest(rep) %>%
  select(-Number, -rep)

# Three formulas to test:
# 1. how much variance in salary band can be explained by: academic role and sex
# 2. how much variance in salary band can be explained by: academic role, sex, and 
# the interaction between role and sex?

baseline_form <- "Salary_Band ~ Academic_employment_function + Sex"
interaction_form <- "Salary_Band ~ Academic_employment_function + Sex + Academic_employment_function:Sex"

baseline_mod <- polr(formula(baseline_form), 
                     data = expanded_df, Hess = T)

print_res <- tidy(modeled_dat) %>%
  filter(term == "genotype") %>%
  rename(beta = estimate, se = std.error, tstat = statistic) %>%
  mutate(or = exp(beta), lci = exp(beta-1.96*se), uci = exp(beta+1.96*se),
         sample_size = nrow(dat),
         pval = pt(tstat, df = sample_size)) %>%
  dplyr::select(all_of(c("beta", "se", "or", "lci", "uci",
                         "tstat", "pval", "sample_size"))) 