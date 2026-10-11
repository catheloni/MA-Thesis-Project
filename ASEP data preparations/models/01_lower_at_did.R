###### DID - Lower Austria as treatment group #######
# 05.05.2026


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
library(marginaleffects)
library(lmtest)

#install.packages("brms", repos = "https://cloud.r-project.org", type = "source")


# setwd
setwd("/data/home/kburt/asep_ihs/Katharina/thesis/data")
# data
data_23 <- readRDS("asep_std_2023.rds")
data_24 <- readRDS("asep_std_2024.rds")
flood_count <- read.csv("03_flood_count_weighted.csv")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# two-way FE requires the dataframe to be LONG

# 1. Ensure we only keep individuals present in BOTH years (Inner Join)
# We find the common IDs first to filter both datasets
common_fids <- intersect(data_23$fid, data_24$fid)

# 2. Filter datasets for common IDs and align columns
data_24_clean <- data_24 %>%
  filter(fid %in% common_fids)

# Identify which columns from 24 are missing in 23
cols_to_keep <- colnames(data_24)

data_23_clean <- data_23 %>%
  filter(fid %in% common_fids) %>%
  # Drop columns NOT in 2024
  dplyr::select(any_of(cols_to_keep))

# 3. Combine the datasets (rbind equivalent)
# bind_rows automatically adds NAs for columns in '24' that weren't in '23'
data_total <- bind_rows(data_23_clean, data_24_clean) %>%
  # Optional: Arrange by fid and year to make it readable
  arrange(fid)

data_total[["post"]] <- as.factor(ifelse(data_total[["survey"]] == "ASEP23", 0, 1))


# define "treated" variable ####
data_total[["treated"]] <- as.factor(ifelse(data_total[["bl"]] == "3", 1, 0))


# replace NAs of all factor columns
data_total[] <- lapply(data_total, function(col) {
  if (is.factor(col)) {
    col <- as.character(col)
    col <- ifelse(grepl("^-", col), NA, col)
    return(as.factor(col))
  }
  return(col) })



# drop everyone that's NA in wor_clima and drop everyone that only answered in one year
data_total <- data_total %>%
  drop_na(wor_clima) %>%
  group_by(fid) %>%
  mutate(n = n()) %>%
  dplyr::filter(n>1)

table(data_total$wor_clima, useNA = "ifany")


# impute missing data
table(data_total$phq4_worry, useNA = "ifany")

# missing in sex_rec is non-binary that I'll replace with female
data_total$sex_rec <- ifelse(data_total$fid == "10029540_01", "2", data_total$sex_rec)
data_total$sex_rec <- as.factor(data_total$sex_rec)

# hedu: the person missing in hedu worked in retail in their previous job (occuplj)
data_total$hedu <- as.factor(ifelse(is.na(data_total$hedu), "5", data_total$hedu))

# phq4_worry: those missing get the average value: 1.45
data_total$phq4_worry <- as.factor(ifelse(is.na(data_total$phq4_worry), "1", data_total$phq4_worry))


# urban
data_total <- data_total %>%
  group_by(fid) %>%
  mutate(
    urban = case_when(
      # Condition: It's the 1st row AND community_codes match across both rows
      row_number() == 1 & (community_code[1] == community_code[2]) ~ urban[2],
      
      # Otherwise, keep the original value of urban
      TRUE ~ urban
    )
  ) %>%
  ungroup()


# looking at the community codes and assigning individuals based on that
cities = c("50101", "91201", "90701", "90501", "91101", "91901", "61111", "41822", "92191")
suburbs = c("41516", "80410", "50316")

data_total$urban <- ifelse(data_total$community_code %in% cities, "1", data_total$urban)
data_total$urban <- ifelse(data_total$community_code %in% suburbs, "2", data_total$urban)
data_total$urban <- ifelse(is.na(data_total$urban), "3", data_total$urban)
data_total$urban <- as.factor(data_total$urban)

# partyprf: create a new variable that is NA
data_total$partyprf <- ifelse(is.na(data_total$partyprf), "9999", data_total$partyprf)
data_total$partyprf <- as.factor(data_total$partyprf)


data_total %>%
  dplyr::filter(is.na(partyprf))
table(data_total$partyprf, useNA = "ifany")


## save data_total to use in all other models
# write.csv(data_total, "data_total.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~#
#     MODELS   #####
#~~~~~~~~~~~~~~~~~~#
## 1 #### Multinomial Logit model

# with continous treatment
fit_state_mod <- clm(
  formula = wor_clima ~ post * treated + sex_rec + age + hedu + partyprf + phq4_worry + urban,
  data = data_total,
  link = "logit")

# Get robust standard errors clustered by individual
clm_coeftest <- coeftest(fit_state_mod, vcov = vcovCL(fit_state_mod, cluster = ~fid))

getwd()
setwd("../models/01_bundesländer-vergleich")
#saveRDS(clm_coeftest, "01_logit_model.Rds")


# OLS
data_total[["wor_clima"]] <- as.numeric(data_total[["wor_clima"]])

fit_state_ols <- lm(
  formula = wor_clima ~ post * treated + sex_rec + age + hedu + partyprf + phq4_worry + urban,
  data = data_total
)

ols_coeftest <- coeftest(fit_state_ols, vcov = vcovCL(fit_state_ols, cluster = ~fid))

saveRDS(ols_coeftest, "01_ols_model.Rds")


# modelsummary

modelsummary(
  list("Model 1" = fit_panel2), 
  shape = term ~ statistic,
  statistic = "conf.int",
  exponentiate = TRUE, 
  conf_level = 0.95,
  # We tell it to explicitly use the Credible Interval for stars
 # stars = c('*' = .05),
  coef_omit = "^sd_|^lp__|^prior_",
 output = "model1.png"
)


# plot the average predictions
avg_preds <- avg_predictions(fit_panel2, 
                             by = c("group", "post", "treated"))

# Plot it!
# ggplot(avg_preds, aes(x = post, y = estimate, color = treated, group = treated)) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
#   facet_wrap(~group) + # Creates a separate box for each level of worry
#   theme_minimal() +
#   labs(title = "Treatment Effect on Climate Worry",
#        y = "Probability of being in category",
#        x = "Time (Pre vs Post)")

ggplot(avg_preds, aes(x = post, y = estimate, color = treated, group = treated)) +
  geom_line(linewidth = 1.2) + # Made lines slightly thicker for better visibility
  geom_point(size = 3) +       # Made points slightly larger
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  
  # THE KEY FIX: scales = "free_y" zooms into the relevant range for each category
  facet_wrap(~group, scales = "free_y") + 
  scale_color_manual(values=c("#1F78B4", "#33A02C")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",         # Moves legend to save horizontal space
    strip.text = element_text(size = 12, face = "bold"), # enlarges facet labels
    panel.spacing = unit(1, "lines")   # Adds slightly more space between facets
  ) +
  labs(
    title = "Treatment Effect on Climate Worry",
    y = "Predicted Probability",
    x = "Time (Pre vs Post)",
    color = "Status"
  )

