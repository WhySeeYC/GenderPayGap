# Author: Samvida S. Venkatesh
# Date: 14/06/24

library(tidyverse)

# Read files ----

YEARS <- c("(2014-15)", "(2015-16)", "(2016-17)",
           "(2017-18)", "(2018-19)", "(2019-20)",
           "(2020-21)", "(2021-22)", "(2022-23)")

OU <- "The University of Oxford"

RUSSELL_GP <- c("The University of Birmingham", "The University of Bristol", 
                "The University of Cambridge", "Cardiff University", 
                "University of Durham", "The University of Edinburgh", 
                "The University of Exeter", "The University of Glasgow", 
                "Imperial College of Science, Technology and Medicine", 
                "King's College London", 
                "The University of Leeds", "The University of Liverpool", 
                "London School of Economics and Political Science", "The University of Manchester", 
                "Newcastle University", "University of Nottingham", 
                "The University of Oxford", "Queen Mary University of London", 
                "Queen's University Belfast", "The University of Sheffield", 
                "The University of Southampton", "University College London", 
                "The University of Warwick", "The University of York")

hesa_dat <- lapply(YEARS, function (yr) {
  df <- read.table(paste0("data/table-17-", yr, ".csv"),
                   sep = ",", skip = 13, header = T)
  colnames(df) <- c("UKPRN", "HE_Provider",	"Country_HE_provider",
                    "Region_HE_provider",	"Academic_employment_function",
                    "Contract_levels", "Sex", "Academic_Year", 
                    "Contract_salary", "Number")
  
  salary_bands <- c("Band_1", "Band_2", "Band_3",
                    "Band_4", "Band_5", "Band_6")
  names(salary_bands) <- unique(df$Contract_salary)[1:6]
  
  df <- df %>% 
    select(-any_of(c("Country_HE_provider", "Region_HE_provider"))) %>%
    mutate(Salary_Band = salary_bands[Contract_salary])
  
  ou_df <- df %>% filter(HE_Provider == OU)
  
  rg_df <- df %>% filter(HE_Provider %in% RUSSELL_GP)
  
  write.table(ou_df, 
              paste0("data/cleaned/oxford_", yr, ".txt"),
              sep = "\t", row.names = F, quote = F) 
  write.table(rg_df, paste0("data/cleaned/russell_group_", yr, ".txt"),
              sep = "\t", row.names = F, quote = F) 
  return (df)
})
hesa_dat <- bind_rows(hesa_dat)

full_ou_hesa <- hesa_dat %>% filter(HE_Provider == OU)
write.table(full_ou_hesa, 
            paste0("data/cleaned/oxford_all_years.txt"),
            sep = "\t", row.names = F, quote = F) 

full_rg_hesa <- hesa_dat %>% filter(HE_Provider %in% RUSSELL_GP)
write.table(full_rg_hesa, 
            paste0("data/cleaned/russell_group_all_years.txt"),
            sep = "\t", row.names = F, quote = F) 

