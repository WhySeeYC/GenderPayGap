# Author: Samvida S. Venkatesh, Daniel J. Phillips
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

prop_df <- prop_df[which(is.element(prop_df$Salary_Band,c("Band_3","Band_4","Band_5","Band_6"))),]


png("plots/1_prop_women_bands_oxford.png",
    width = 1400,height=1000, pointsize=23)
ggplot(prop_df, aes(x = Academic_Year, y = prop_female,
                    colour = Salary_Band, fill = Salary_Band,
                    group = Salary_Band)) +
  geom_point(cex=5) +
  geom_line(lwd=2) +
  scale_y_continuous(name="Proportion in band who are Female") +
  scale_color_manual(values=brewer.pal(n = 6, name = "Blues")[3:6]) +
  theme(text=element_text(size=23),
        axis.text.x=element_text(size=25),
        axis.text.y=element_text(size=25))
dev.off()

########
# Plot of number_of_women_in_band/total_number_of_women
oxford <- read.csv(paste0(getwd(),"/data/cleaned/oxford_all_years.txt"), sep="\t")
require(MASS)
out_df_band <- oxford
# ou_df <- oxford[apply(oxford,1,function(x){!any(is.element(x,c("All","Total")))}),]
# ou_df <- ou_df[which(is.element(ou_df$Sex,c("Male","Female","All"))),]
# ou_df <- ou_df[which(is.element(ou_df$Salary_Band,c("Band_3","Band_4","Band_5","Band_6"))),]

head(oxford_no_total)
out_df_band$Salary_Band[which(is.na(out_df_band$Salary_Band))] <- "All"
out_df_band <- out_df_band[which(is.element(out_df_band$Sex,c("Male","Female"))),]

sum_df_band <- out_df_band %>%
  group_by(Academic_Year, Sex, Salary_Band) %>%
  summarise(salary_count = sum(Number))

count_df_band <- sum_df_band %>%
  pivot_wider(id_cols = c(Academic_Year, Sex),
              names_from = Salary_Band,
              values_from = salary_count)
# prop_df_band[,4:9] <- prop_df_band[,4:9]/rowSums(prop_df_band[,4:9])
count_df_band <- count_df_band[,c(1,2,6:9)]

count_df_band_long <- count_df_band %>%
  pivot_longer(cols= c(Band_3,Band_4,Band_5,Band_6), names_to = "Band")

scale_color_brewer("Blues")

png("plots/2_count_across_band_women_plot.png",
    width = 1400,height=1000)
ggplot(count_df_band_long[which(count_df_band_long$Sex=="Female"),], aes(x = Academic_Year, y = value,
                                  colour = Band, fill = Band,
                                  group = Band)) +
  geom_point(cex=5) +
  geom_line(lwd=2) +
  scale_y_continuous(limits=c(0,25000), name = "Counts") +
  scale_color_manual(values=brewer.pal(n = 6, name = "Blues")[3:6]) +
  theme(text=element_text(size=23),
        axis.text.x=element_text(size=25),
        axis.text.y=element_text(size=25))
dev.off()

png("plots/2_count_across_band_men_plot.png",
    width = 1400,height=1000, pointsize=23)
ggplot(count_df_band_long[which(count_df_band_long$Sex=="Male"),], aes(x = Academic_Year, y = value,
                                                                       colour = Band, fill = Band,
                                                                       group = Band)) +
  geom_point(cex=5) +
  geom_line(lwd=2) +
  scale_y_continuous(limits=c(0,25000), name = "Counts") +
  scale_color_manual(values=brewer.pal(n = 6, name = "Blues")[3:6]) +
  theme(text=element_text(size=23),
        axis.text.x=element_text(size=25),
        axis.text.y=element_text(size=25))
dev.off()

