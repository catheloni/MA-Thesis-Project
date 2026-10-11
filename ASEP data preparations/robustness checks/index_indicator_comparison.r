# How much do the media index and the damage indicator overlap for Lower Austria?

#=====================================================#
####    ROBUSTNESS CHECK #2: Binary Outcome   ####
# 31.05.2026
#=====================================================#


# SETUP ####

# libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(nnet)
library(bife)
library(modelsummary)
library(broom)
library(mclogit)
library(marginaleffects)
library(ordinal)
library(emmeans)
library(gofcat)
library(lmtest)
library(sandwich)


#install.packages("ggeffects", repos = "https://cloud.r-project.org", type = "source")



setwd("/data/home/kburt/asep_ihs/Katharina/thesis/data")

data_total <- read.csv("data_total.csv")
noe_data <- read.csv("04_noe_damage_weighted.csv")


#~~~~~~~~~~~~~~~
# NOE DATA ####
#~~~~~~~~~~~~~~
# NOE data is the data source provided by the Lower Austrian Government.
# It contains info of the number of buildings damaged per town

# renaming the columns so that I can work with them
noe_data <- noe_data %>%
  rename(damage_flood_dummy = 'Gab.es.in.Ihrer.Gemeinde.Schäden.durch.das.Hochwasser.im.September.',
         damage_storm_dummy = 'Gab.es.in.Ihrer.Gemeinde.Schäden.durch.den.Sturm.im.September.2024.',
         nr_buildings_damaged = 'Anzahl.der.vom.Hochwasser.betroffenen.privaten.Objekte..Schätzwerte.wenn.möglich.auf.5.oder.10.gerundet.')
noe_data <- noe_data %>%
  rename(community_code = town_kz)

# created a weighted damage variable; per capita
noe_data[["weighted_damage"]] <- noe_data[["nr_buildings_damaged"]] / noe_data[["size"]] * 10 # times 10 to make it more readable

# plot weighted damage
ggplot(noe_data %>% filter(weighted_damage < 2), aes(x = weighted_damage)) +
  geom_histogram(color = "white", fill = "#1F78B4", bins = 60) +
  labs(title = "Damaged Buildings in Municipality (weighted)", x = "", y = "Count")

# make sure community code is a character vector
noe_data[["community_code"]] <- as.character(noe_data[["community_code"]])
data_total[["community_code"]] <- as.character(data_total[["community_code"]])


# add it to the asep_df
# left join it to everyone with the same community code
data_total <- data_total %>%
  left_join(noe_data[c("weighted_damage", "community_code")], by = "community_code")

# replace all people with NA with 0 --> they were not affected
data_total[["treated"]] <- NA
data_total[["treated"]] <- ifelse(is.na(data_total[["weighted_damage"]]), 0, data_total[["weighted_damage"]])


#===========================#
#   ADD Flood index
#===========================#

flood_count <- read.csv("03_flood_count_weighted.csv")

# make sure to keep the last community code
data_total <- data_total %>%
  group_by(fid) %>%
  mutate(community_code = last(community_code)) %>%
  ungroup()


# define "treated" variable ####
flood_count <- flood_count %>% rename(community_code = town_kz) %>% mutate(community_code = as.character(community_code))
data_total[["community_code"]] <- as.character(data_total[["community_code"]])

# make sure to keep the last community code
data_total <- data_total %>%
  group_by(fid) %>%
  mutate(community_code = last(community_code)) %>%
  ungroup()

# new flood count measure
flood_count$new_count <- NA
flood_count$new_count <- log(flood_count$Flood_Mention_Count + 1) / log(flood_count$size)


# apparently flood count has multiple rows for the same community code
flood_count <- flood_count %>%
  group_by(community_code) %>%
  dplyr::slice(1)

data_total <- data_total %>%
  left_join(flood_count[c("community_code", "flood_count_weighted_rec", "flood_count_weighted", "Flood_Mention_Count", "new_count")], join_by("community_code"))

data_total$flood_count_weighted_rec <- ifelse(is.na(data_total$flood_count_weighted_rec), 0, data_total$flood_count_weighted_rec)
data_total$flood_count_weighted <- ifelse(is.na(data_total$flood_count_weighted), 0, data_total$flood_count_weighted)
data_total$Flood_Mention_Count <- ifelse(is.na(data_total$Flood_Mention_Count), 0, data_total$Flood_Mention_Count)
data_total$new_count <- ifelse(is.na(data_total$new_count), 0, data_total$new_count)




#=========================================#
#    Limiting sample to lower Austria     #
#=========================================#
data_total <- data_total %>%
  dplyr::filter(bl == 3)

# drop outliers
data <- data_total %>%
  dplyr::filter(weighted_damage < 1)# %>%
  dplyr::filter(flood_count_weighted_rec < 10)

plot(data$flood_count_weighted, data$weighted_damage)

table(data$flood_count_weighted, useNA = "ifany")

cor(data$new_count, data$weighted_damage)
plot(data$hedu, data$hhinc)



