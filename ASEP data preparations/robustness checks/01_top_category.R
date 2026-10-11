#=====================================================#
####    ROBUSTNESS CHECK #1: REMOVE TOP CATEGORY   ####
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

length(data_total[which(data_total[["treated"]] >0),]$fid) 

length(data_total$fid)



#===============================#
####   Remove Top Category   ####
#===============================#

length(data_total$post)


data_total <- data_total %>%
  dplyr::filter(!(post == 0 & wor_clima == 3)) %>%
  group_by(fid) %>%
  mutate(n = n()) %>%
  dplyr::filter(n>1)
  
length(data_total$post)
table(data_total[which(data_total$post == 0),]$wor_clima)



#~~~~~~~~~~~~~~~~~~~~
# MODEL ####
#~~~~~~~~~~~~~~~~~~~~~~

# CLM - ordinary logit
for (i in c("wor_clima", "post", "sex_rec", "hedu", "partyprf", "phq4_worry", "urban")) {
  data_total[[i]] <- as.factor(data_total[[i]])
}

# with continous treatment
fit_state_mod <- clm(
  formula = wor_clima ~ post * treated + sex_rec + age + hedu + partyprf + phq4_worry + urban,
  data = data_total,
  link = "logit"
)

# Get robust standard errors clustered by individual
clm_coeftest <- coeftest(fit_state_mod, vcov = vcovCL(fit_state_mod, cluster = ~fid))

setwd("/data/home/kburt/asep_ihs/Katharina/thesis/checks/robustness-checks")
saveRDS(clm_coeftest, "01_logit_top_category.Rds")


# OLS
data_total[["wor_clima"]] <- as.numeric(data_total[["wor_clima"]])

fit_state_ols <- lm(
  formula = wor_clima ~ post * treated + sex_rec + age + hedu + partyprf + phq4_worry + urban,
  data = data_total
)

ols_coeftest <- coeftest(fit_state_ols, vcov = vcovCL(fit_state_ols, cluster = ~fid))

saveRDS(ols_coeftest, "01_ols_top_category.Rds")

