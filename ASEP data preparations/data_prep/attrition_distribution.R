


# libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(flextable)

# SETUP
setwd("/data/home/kburt/asep_ihs/Katharina/thesis/data")
# data
data_23 <- readRDS("asep_std_2023.rds")
data_24 <- readRDS("asep_std_2024.rds")
flood_count <- read.csv("03_flood_count_weighted.csv")

##2ß23

## replace NAs of all factor columns
data_23[] <- lapply(data_23, function(col) {
  if (is.factor(col)) {
    col <- as.character(col)
    col <- ifelse(grepl("^-", col), NA, col)
    return(as.factor(col))
  }
  return(col) })

# if numeric cols are <0, replace with NA
numeric_cols <- c()
for (i in colnames(data_23)) {
  if (is.numeric(data_23[[i]])) {
    numeric_cols <- c(numeric_cols, i)
  }
}

data_23 <- data_23 %>% mutate(across(all_of(numeric_cols), \(x) ifelse(x < 0, NA, x)))

# 2024

## replace NAs of all factor columns
data_24[] <- lapply(data_24, function(col) {
  if (is.factor(col)) {
    col <- as.character(col)
    col <- ifelse(grepl("^-", col), NA, col)
    return(as.factor(col))
  }
  return(col) })

# if numeric cols are <0, replace with NA
numeric_cols <- c()
for (i in colnames(data_24)) {
  if (is.numeric(data_24[[i]])) {
    numeric_cols <- c(numeric_cols, i)
  }
}

data_24 <- data_24 %>% mutate(across(all_of(numeric_cols), \(x) ifelse(x < 0, NA, x)))


common_fids <- intersect(data_23$fid, data_24$fid)

data_24_fid <- data_24 %>%
  filter(fid %in% common_fids)

data_24_fid <- data_24_fid %>%
  filter(is.na(wor_clima))

# those replied in both waves but didn't give a valid answer in wor_clima
na_fids <- data_24_fid$fid

## ATTRITION DATA ####

attr_data <- data_23 %>%
  filter(!fid %in% common_fids | fid %in% na_fids)

length(attr_data$fid)

reten_data <- data_23 %>%
  filter(fid %in% common_fids & !(fid %in% na_fids))

length(reten_data$fid)
#===================================================#


attr <- round(proportions(table(attr_data$wor_clima)),2)
reten <- round(proportions(table(reten_data$wor_clima)), 2)

df <- data.frame("Attrition" = attr, "Retention" = reten)

df <- df %>%
  dplyr::select(!Retention.Var1)

df <- df %>%
  mutate(Attrition.Var1 = c("No Worries", "Some Worries", "Big Worries"))


t <- flextable(df)
t <- set_header_labels(t, "Attrition.Var1" = "", "Attrition.Freq" = "Attrition", "Retention.Freq" = "Retention")
t <- width(t, j=1, width = 1.5)

save_as_image(t, "/data/home/kburt/asep_ihs/Katharina/thesis/data-prep/attr_reten_table.png")
