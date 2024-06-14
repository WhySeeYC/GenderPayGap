# Author: Samvida S. Venkatesh
# Date: 14/06/24

library(tidyverse)
library(RColorBrewer)
theme_set(theme_bw())

ou_df <- read.table("data/cleaned/oxford_all_years.txt",
                    sep = "\t", header = T, stringsAsFactors = F)

SEXES <- c("Female", "Male")
SALARY_BAND <- c("Band_1", "Band_2", "Band_3",
                 "Band_4", "Band_5", "Band_6")

sum_df <- ou_df %>%
  group_by(Academic_Year, Salary_Band, Sex) %>%
  summarise(sex_count = sum(Number))
  
prop_df <- sum_df %>%
  pivot_wider(id_cols = c(Academic_Year, Salary_Band),
              names_from = Sex,
              values_from = sex_count) %>%
  mutate(prop_female = Female/All)

rough_plot <- ggplot(prop_df, aes(x = Academic_Year, y = prop_female,
                    colour = Salary_Band, fill = Salary_Band,
                    group = Salary_Band)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "PuBuGn") +
  scale_fill_brewer(palette = "PuBuGn")

ggsave("plots/1_prop_women_bands_oxford.png",
       rough_plot)

