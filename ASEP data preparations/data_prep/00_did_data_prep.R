#::::::::::::::::::::::::::::::::::::::::::::::::#
###### DID DATA PREP #######
# This file prepares ASEP data so that it can be further used for running the models
# Note: this script also already runs a few models (couldn't hold myself back), but those will not be futher used.
# April - Mai 2025
#::::::::::::::::::::::::::::::::::::::::::::::::#

# SETUP ####

# libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(nnet)
#install.packages("plm", dependencies = TRUE, repos = "https://cloud.r-project.org")
library(plm)
library(fixest)
library(bife) # Note: bife is optimized for binary models


# setwd
setwd("/data/home/kburt/asep_ihs/Katharina/thesis/data")
# data
asep_2023 <- readRDS("asep_std_2023.rds")
asep_2024 <- readRDS("asep_std_2024.rds")
flood_count <- read.csv("03_flood_count_weighted.csv")


# NOTE: two-way FE requires the dataframe to be LONG

#::::::::::::::::::::::::::::::::::::::::::::::::#

# define "treated" variable ####

town_kz <- c()
for (i in flood_count[which(flood_count[["flood_count_weighted"]] > 0.15),][["town_kz"]]) {
  town_kz <- c(town_kz, i)
}

asep_2024[["treated"]] <- ifelse(asep_2024[["community_code"]] %in% town_kz, 1, 0)

# left join wor_clima 2023
asep_total <- left_join(asep_2024, asep_2023[,c("fid", "wor_clima")], by = "fid")

# delete rows with missings
asep_total <- asep_total %>%
  dplyr::filter(as.numeric(as.character(wor_clima.x))>0 & as.numeric(as.character(wor_clima.y))>0)


#::::::::::::::::::::::::::::::::::::::::::::::::#

#### pivot longer ####
asep_total <- pivot_longer(asep_total, c("wor_clima.x", "wor_clima.y"), names_to = "year", values_to = "wor_clima")

asep_total[["year"]] <- ifelse(asep_total[["year"]] == "wor_clima.x", 0, 1)

asep_total[["did"]] <- asep_total[["year"]] * asep_total[["treated"]]


#::::::::::::::::::::::::::::::::::::::::::::::::#

#### plm model #####
#plm(wor_clima ~ treated + year +  did + sex_rec + age, index = "fid", model = "within", data = asep_total)

asep_total[["wor_clima_rec"]] <- as.numeric(as.character(asep_total[["wor_clima"]]))
plm(wor_clima_rec ~ treated + year + did + sex_rec, data = asep_total)

# Source - https://stackoverflow.com/q/61399938
# Posted by Anna Bokun, modified by community. See post 'Timeline' for change history
# Retrieved 2026-04-20, License - CC BY-SA 4.0


#::::::::::::::::::::::::::::::::::::::::::::::::#

## 2.) FE Model      
FE_model <- plm(wor_clima_rec ~ treated:year, data = asep_total, index=c("fid", "year"), model="within")
summary(FE_model)

# 'fct' represents your fixed effects after the pipe |
FE_model <- feols(wor_clima_rec ~ did | fid + year, data = asep_total)
# View results with clustered standard errors by individual
summary(FE_model, cluster = ~fid)

asep_total[["wor_clima_rec2"]] <- ifelse(asep_total$wor_clima_rec == 1, 0, 1)
fe_logit <- bife(wor_clima_rec2 ~ did + year | fid, data = asep_total, model = "logit")
summary(fe_logit)

modelsummary(fe_logit, )
modelsummary(fe_logit, fmt = fmt_significant(3))
