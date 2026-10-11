#================================================#
#### XGBOOST --> with weighted damage model  ####
# 07.05.2026
#================================================#


#============#
# SETUP ####
#============#

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
library(sandwich)
library(lmtest)
library(xgboost)
library(stringr)
library(fastDummies)
library(shapviz)
library(brant)

# install.packages("shapviz", repos = "https://cloud.r-project.org", type = "source")

# setwd
setwd("/data/home/kburt/asep_ihs/Katharina/thesis/data")

# data
data_23 <- readRDS("asep_std_2023.rds")
data_24 <- readRDS("asep_std_2024.rds")
noe_data <- read.csv("04_noe_damage_weighted.csv")


#=======================#
# DATA WRANGLING        #
#=======================#

# regular data wrangling of data_23 and data_24; creating a long df ####
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
  # Arrange by fid and year to make it readable
  arrange(fid)

# Create post == 0 if 2023 and 1 if 2024
data_total[["post"]] <- as.factor(ifelse(data_total[["survey"]] == "ASEP23", 0, 1))


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

# add it to the asep_df
# left join it to everyone with the same community code
data_total <- data_total %>%
  left_join(noe_data[c("weighted_damage", "community_code")], by = "community_code")

# replace all people with NA with 0 --> they were not affected
data_total[["weighted_damage"]] <- ifelse(is.na(data_total[["weighted_damage"]]), 0, data_total[["weighted_damage"]])

length(data_total[which(data_total[["weighted_damage"]] >0),]$fid) # 250 people are in the treatment group

# TODO: who moved away?

# three people moved in/out of treatment area
data_total %>%
  group_by(fid) %>%
  distinct(weighted_damage) %>%
  mutate(n = n()) %>%
  filter(n>1)

# Replace all values in the group with the second value - that should be the relevant one in terms of getting treated
data_total <- data_total %>%
  group_by(fid) %>%
  mutate(weighted_damage = last(weighted_damage)) %>%
  ungroup()

# replace NAs of all factor columns
data_total[] <- lapply(data_total, function(col) {
  if (is.factor(col)) {
    col <- as.character(col)
    col <- ifelse(grepl("^-", col), NA, col)
    return(as.factor(col))
  }
  return(col) })

# if numeric cols are <0, replace with NA
numeric_cols <- sapply(data_total, is.numeric)
data_total[numeric_cols] <- lapply(data_total[numeric_cols], function(x) ifelse(x<0,NA,x))


# Important Step: filter those that didn't answer wor_clima in both waves!
data_total <- data_total %>%
  filter(!is.na(wor_clima))

# and filter all those that only appear once
data_total <- data_total %>%
  group_by(fid) %>%
  mutate(n = n()) %>%
  filter(n>1)

# table(data_total$post, data_total$weighted_damage == 0)

#~~~~~~~~~~~~~~~~~~~~
# MODEL ####
#~~~~~~~~~~~~~~~~~~~~~~

# with continous treatment
fit_continuous <- clm(
  formula = wor_clima ~ post * weighted_damage + sex_rec + age + hedu,
  data = data_total,
  link = "logit"
)

# Get robust standard errors clustered by individual
coeftest(fit_continuous, vcov = vcovCL(fit_continuous, cluster = ~fid))



# check correlation between the controls and the treatment variable
data_total$sex_rec <- as.numeric(data_total$sex_rec)
data_total$sex_rec <- as.factor(data_total$sex_rec)
trial_data <- data_total
trial_data$hedu <- as.numeric(trial_data$hedu)
trial_data <- trial_data %>% filter(hedu <8) # I want to exclude 8 from the correlation part because those are unknown levels of education
plot(trial_data$hedu)

cor(data_total[which(!is.na(data_total$sex_rec) == TRUE),]$sex_rec, data_total[which(!is.na(data_total$sex_rec) == TRUE),]$weighted_damage)
cor(trial_data[which(!is.na(trial_data$hedu) == TRUE),]$hedu, trial_data[which(!is.na(trial_data$hedu) == TRUE),]$weighted_damage)


class(data_total$weighted_damage)

summary(fit_continuous_01)
summary(fit_continuous)

#=======================
# MODELSUMMARY
# =======================

# Define how you want the terms to look
cm <- c(
  "post1" = "Post-Treatment",
  "weighted_damage" = "Treated Group",
  "post1:weighted_damage" = "DID (Post x Treated)",
  "sex_rec2" = "Gender",
  "age" = "Age"#,
  #  "hedu" = "Education"
)

modelsummary(
  fit_continuous,
  coef_map = cm,
  stars = TRUE,
  exponentiate = TRUE, # Sets coefficients to Odds Ratios
  title = "Cumulative Link Mixed Model: Climate Worry; Continous Treatment",
  gof_omit = "AIC|BIC|Log.Lik", # Keeps it clean
  notes = "Note: Coefficients are presented as Odds Ratios. Random effects (fid) included. Education included as control."#,
  #  output = "continous_summary.png"
)


# 1. Define the 'doses' you want to visualize
# This picks the mean and one standard deviation above/below
treatment_levels <- round(c(
  0,
  mean(data_total$weighted_damage),
  mean(data_total$weighted_damage) + sd(data_total$weighted_damage)),4)

# 2. Get predictions
emm_cont <- emmeans(fit_continuous_01, ~ weighted_damage | post * wor_clima, 
                    at = list(weighted_damage = treatment_levels),
                    mode = "prob")

avg_preds_cont <- as.data.frame(emm_cont)

# 3. Plot
ggplot(avg_preds_cont, aes(x = factor(post), y = prob, 
                           color = factor(weighted_damage), 
                           group = weighted_damage)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) + ##
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) + ##
  facet_wrap(~wor_clima, scales = "free_y") + 
  theme_minimal(base_size = 14) +
  scale_color_manual(values=c("#1F78B4", "#33A02C", "darkblue"), name = "Treatment Intensity:", labels = c("none", "average", "high (mean + SD)")) +
  labs(
    title = "Treatment Effect on Climate Worry",
    x = "Year (0 = 2023, 1 = 2024)",
    y = "Predicted Probability"
  ) +
  theme(legend.position = "bottom")

# Requires the RColorBrewer package
RColorBrewer::brewer.pal(n = 6, name = "Paired")

length(data_total[which(data_total[["weighted_damage"]] > mean(data_total[["weighted_damage"]]) + 2*SD(data_total[["weighted_damage"]])),]$fid)
# das sind nur noch 50 Personen




#====================================#
#         XGBOOST                    #
#====================================#

# occup_rec: keep only first digit
data_total <- data_total %>%
  mutate(occup_rec = substr(occup_rec, 0, 1))

# create a total income variable
data_total <- data_total %>%
  mutate(total_inc = sum(across(c(pincemp, pincse, pincpens, wealthinc))))

data_xgb <- data_total %>%
  ungroup() %>%
  #dplyr::select(post, weighted_damage, sex_rec, age, hedu, mnacti, ctzship_rec, maritalstat, wor_clima) %>%
  mutate(
    target = as.numeric(as.factor(wor_clima)) - 1,
    post = as.numeric(post),
    # Create the interaction term explicitly
    did_interaction = post * weighted_damage
  ) %>%
  # Filter out rows ONLY if the target itself is missing
  dplyr::filter(!is.na(target)) %>%
  # One-hot encode your categorical variables
  dummy_cols(
    select_columns = c("mnacti", "ctzship_rec", "maritalstat", "partyprf_rec", "occup_rec",
                       "religaff_yn", "post", "sex_rec", "aspedu_yn", "chldex_yn"),
    remove_first_dummy = TRUE,       # Drops the first category to avoid the dummy variable trap
    remove_selected_columns = TRUE, # Removes the original character/factor columns
    ignore_na = FALSE               # Crucial! See the explanation below
  )

# 2. Separate predictors (X) and target (y)
# Include all your controls (even time-invariant ones) to see how XGBoost ranks them

data_xgb$chld

predictors <- data_xgb %>%
  dplyr::select(weighted_damage, did_interaction, 
                age, hedu, urban, wrktime,
                sblgsno, health, phq4_worry, total_inc,
                # smoke, alcfrq, physact, lifesat,
                # total_inc, big5_reserv, big5_trust,
                # big5_lazy, big5_relax, big5_noart,
                # big5_outg, big5_crit, big5_thoro,
                # big5_nerv, big5_imag, trsthealth_cmed,
                # phq4_lintr, phq4_fdown, phq4_nervs,
    # all_of() safely evaluates the character vector generated by paste0
    # all_of(paste0("maritalstat_", 2:4)),
    # all_of(paste0("ctzship_rec_", 2:5)),
    all_of(paste0("partyprf_rec_", 2:6)),
 #   all_of(paste0("occup_rec_", 2:9)),
    # dummies
    religaff_yn_1, post_2, sex_rec_2
    )

# predictors <- data_xgb %>%
#   dplyr::select(post, weighted_damage, did_interaction, sex_rec, age, hedu, paste0("mnacti_", 2:9))

# 1. Clean the data frame and force all columns to be strictly numeric
predictors_clean <- predictors %>%
  mutate(across(everything(), ~ {
    # If it's a character or factor, replace commas with dots first
    if (is.character(.) || is.factor(.)) {
      . <- str_replace_all(as.character(.), ",", ".")
    }
    # Force convert to numeric
    as.numeric(.)
  }))

# 2. Check if any NAs were accidentally introduced during conversion
if (any(is.na(predictors_clean))) {
  warning("Some values could not be converted to numbers and became NA!")
}

# 3. Convert to matrix (it will now be a clean numeric matrix)
X_matrix <- as.matrix(predictors_clean)

# 4. Double check the matrix class
print(mode(X_matrix)) # This should print "numeric" now!

# 5. Make sure your target is also strictly numeric
y_vector <- as.numeric(data_xgb$target)

# 4. Determine how many unique classes your ordinal scale has
num_classes <- length(unique(y_vector))

# Convert into the native XGBoost matrix format
dtrain <- xgb.DMatrix(data = X_matrix, label = y_vector)

# 1. Define the integer params (bypassing the decimal locale bug)
params <- list(
  objective = "multi:softprob",
  num_class = num_classes,
  eval_metric = "mlogloss",
  max_depth = 3,         
  eta = 1,               
  base_score = 0         
)

# 2. Train the model directly for a fixed number of rounds
# With eta = 1, 10 rounds is more than enough to capture the feature patterns
fit_xgb <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 10
)

# 3. Calculate and extract feature importance
importance_matrix <- xgb.importance(
  feature_names = colnames(X_matrix), 
  model = fit_xgb
)

# 4. View your results!
print(importance_matrix)
xgb.plot.importance(importance_matrix, main = "Variable Importance", rel_to_first = TRUE)

# shap plot
# 2. Compute SHAP values using shapviz
# Note: X_train must be the numeric matrix you used to create dtrain
shp <- shapviz(fit_xgb, X_pred = X_matrix)

# 3. Generate the SHAP Summary Plot (Beeswarm)
# This shows the impact of all features ordered by importances
sv_importance(shp, kind = "beeswarm") +
  theme_minimal() +
  labs(title = "SHAP Feature Importance Summary")

# Check the names of the classes generated
names(shp) 
# Explicitly plot only the summary for the 2nd level of your factor
sv_importance(shp[[3]], kind = "beeswarm") + 
  labs(title = "SHAP Summary for Outcome Level 3")

# 4. Generate a SHAP Dependence Plot for your interaction term
# This shows how the SHAP value changes across different values of the feature
sv_dependence(shp, v = "did_interaction") +
  theme_minimal() +
  labs(title = "SHAP Dependence Plot for DID Interaction")


#=========================================#
#             CONCLUSION                  #
#=========================================#

# most important predictors:
# hedu, partyprf, phq4_worry, lifesat, urban, age, sex
# some that are also consistently up there: smoke, health, physact, phq4_fdown, ...





#==========================================#
#          CHECK CORRELATIONS              #
#==========================================#

# just turn all factor variables to numerics
corcheck <- data_total
corcheck[] <- lapply(corcheck, function(col) {
  if (is.factor(col)) {
    col <- as.numeric(col)
  }
  return(col) })

corcheck$hedu <- as.numeric(corcheck$hedu)
table(corcheck$hedu)
corcheck <- corcheck %>% mutate(hedu = ifelse(hedu == 8, NA, hedu))# I want to exclude 8 from the correlation part because those are unknown levels of education

# sex_rec and wor_clima
cor(corcheck[which(!is.na(corcheck$sex_rec) == TRUE),]$sex_rec, corcheck[which(!is.na(corcheck$sex_rec) == TRUE),]$wor_clima)
# 0.1

# age and wor_clima
cor(corcheck[which(!is.na(corcheck$age) == TRUE),]$age, corcheck[which(!is.na(corcheck$age) == TRUE),]$wor_clima)
# 0.03

# lifesat and wor_clima
cor(corcheck[which(!is.na(corcheck$lifesat) == TRUE),]$lifesat, corcheck[which(!is.na(corcheck$lifesat) == TRUE),]$wor_clima)
# 0.03

# hedu and wor_clima
cor(corcheck[which(!is.na(corcheck$hedu) == TRUE),]$hedu, corcheck[which(!is.na(corcheck$hedu) == TRUE),]$wor_clima)
# 0.2

# phq4_worry and wor_clima
cor(corcheck[which(!is.na(corcheck$phq4_worry) == TRUE),]$phq4_worry, corcheck[which(!is.na(corcheck$phq4_worry) == TRUE),]$wor_clima)
# 0.03

# partyprf and wor_clima
cor(corcheck[which(!is.na(corcheck$partyprf) == TRUE),]$partyprf, corcheck[which(!is.na(corcheck$partyprf) == TRUE),]$wor_clima)
# 0.01

# urban and wor_clima
cor(corcheck[which(!is.na(corcheck$urban) == TRUE),]$urban, corcheck[which(!is.na(corcheck$urban) == TRUE),]$wor_clima)
# -0.1

# physact and wor_clima
cor(corcheck[which(!is.na(corcheck$physact) == TRUE),]$physact, corcheck[which(!is.na(corcheck$physact) == TRUE),]$wor_clima)
# 0.08

# physact and hedu
cor(corcheck[which(!is.na(corcheck$physact) == TRUE & !is.na(corcheck$hedu) == TRUE),]$physact,
    corcheck[which(!is.na(corcheck$physact) == TRUE & !is.na(corcheck$hedu) == TRUE),]$hedu)
# 0.1

# wrktime and wor_clima
cor(corcheck[which(!is.na(corcheck$wrktime) == TRUE),]$wrktime, corcheck[which(!is.na(corcheck$wrktime) == TRUE),]$wor_clima)
# -0.07

# health and wor_clima
cor(corcheck[which(!is.na(corcheck$health) == TRUE),]$health, corcheck[which(!is.na(corcheck$health) == TRUE),]$wor_clima)
# -0.07


                        