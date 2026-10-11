###### DID DATA PREP #######
# 27.04.2026

# This script wrangles the ASEP 2023 and 2024 data to prepare it for the DID analysis
# Because I'm impatient, it also includes some first models


# SETUP ####

# libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(nnet)
library(fixest)
library(MASS)
library(bife)
library(modelsummary)
library(broom)
library(tinytable)
library(sandwich)
library(lmtest)
library(tree)
library(MASS)
library(randomForest)
library(gbm)
library(ipred)
library(xtable)
library(vip)
library(lime)
library(DALEX)
library(DALEXtra)
library(caret)
library(ranger)
library(rattle)
library(Hmisc)
library(knitr)
library(kableExtra)
library(xgboost)
library(doParallel)
library(officer)
library(flextable)
library(survey)
library(gt)



####### is RF really the best choice? ######
#install.packages("officer", repos = "https://cloud.r-project.org", type = "source")

# SETUP
setwd("/data/home/kburt/asep_ihs/Katharina/thesis/data")
# data
data_23 <- readRDS("asep_std_2023.rds")
data_24 <- readRDS("asep_std_2024.rds")
flood_count <- read.csv("03_flood_count_weighted.csv")


##2023

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



#data[["wor_clima_rec"]] <- as.numeric(as.character(data[["wor_clima"]]))

data_24$envpwill_htax <- NA
# data_24$envpwill_htax <- ifelse(data_24$envpwill_v1_htax == "1" | data_24$envpwill_v2_htax == "5" |
#                                   data_24$envpwill_v3_htax == "1" | data_24$envpwill_v4_htax == "5" , "1", data_24$envpwill_htax)
# 
# data_24$envpwill_htax <- ifelse(data_24$envpwill_v1_htax == "2" | data_24$envpwill_v2_htax == "4" |
#                                   data_24$envpwill_v3_htax == "2" | data_24$envpwill_v4_htax == "4" , "2", data_24$envpwill_htax)


data_24 <- data_24 %>%
  mutate(envpwill_htax = case_when(
    # Priority 1: If any of these are true, set to "1"
    envpwill_v1_htax == "1" | envpwill_v2_htax == "5" |
      envpwill_v3_htax == "1" | envpwill_v4_htax == "5" ~ "1",
    
    # Priority 2: If the above wasn't true, but these are true, set to "2"
    envpwill_v1_htax == "2" | envpwill_v2_htax == "4" |
      envpwill_v3_htax == "2" | envpwill_v4_htax == "4" ~ "2",
    
    envpwill_v1_htax == "3" | envpwill_v2_htax == "3" |
      envpwill_v3_htax == "3" | envpwill_v4_htax == "3" ~ "3",
    
    envpwill_v1_htax == "4" | envpwill_v2_htax == "2" |
      envpwill_v3_htax == "4" | envpwill_v4_htax == "2" ~ "4",

    
    envpwill_v1_htax == "5" | envpwill_v2_htax == "1" |
      envpwill_v3_htax == "5" | envpwill_v4_htax == "1" ~ "5",
    
    # Optional catch-all: Anything else becomes NA
    TRUE ~ NA_character_
  ))



data_24 <- data_24 %>%
  mutate(envpwill_hpcs = case_when(
    # Priority 1: If any of these are true, set to "1"
    envpwill_v1_hpcs == "1" | envpwill_v2_hpcs == "5" |
      envpwill_v3_hpcs == "1" | envpwill_v4_hpcs == "5" ~ "1",
    
    # Priority 2: If the above wasn't true, but these are true, set to "2"
    envpwill_v1_hpcs == "2" | envpwill_v2_hpcs == "4" |
      envpwill_v3_hpcs == "2" | envpwill_v4_hpcs == "4" ~ "2",
    
    envpwill_v1_hpcs == "3" | envpwill_v2_hpcs == "3" |
      envpwill_v3_hpcs == "3" | envpwill_v4_hpcs == "3" ~ "3",
    
    envpwill_v1_hpcs == "4" | envpwill_v2_hpcs == "2" |
      envpwill_v3_htax == "4" | envpwill_v4_hpcs == "2" ~ "4",
    
    
    envpwill_v1_hpcs == "5" | envpwill_v2_hpcs == "1" |
      envpwill_v3_hpcs == "5" | envpwill_v4_hpcs == "1" ~ "5",
    
    # Optional catch-all: Anything else becomes NA
    TRUE ~ NA_character_
  ))


data_24 <- data_24 %>%
  mutate(envpwill_lstlv = case_when(
    # Priority 1: If any of these are true, set to "1"
    envpwill_v1_lstlv == "1" | envpwill_v2_lstlv == "5" |
      envpwill_v3_lstlv == "1" | envpwill_v4_lstlv == "5" ~ "1",
    
    # Priority 2: If the above wasn't true, but these are true, set to "2"
    envpwill_v1_lstlv == "2" | envpwill_v2_lstlv == "4" |
      envpwill_v3_lstlv == "2" | envpwill_v4_lstlv == "4" ~ "2",
    
    envpwill_v1_lstlv == "3" | envpwill_v2_lstlv == "3" |
      envpwill_v3_lstlv == "3" | envpwill_v4_lstlv == "3" ~ "3",
    
    envpwill_v1_lstlv == "4" | envpwill_v2_lstlv == "2" |
      envpwill_v3_lstlv == "4" | envpwill_v4_lstlv == "2" ~ "4",
  
    envpwill_v1_lstlv == "5" | envpwill_v2_lstlv == "1" |
      envpwill_v3_lstlv == "5" | envpwill_v4_lstlv == "1" ~ "5",
    
    # Optional catch-all: Anything else becomes NA
    TRUE ~ NA_character_
  ))

table(data_24$envpwill_v2_hpcs, useNA = "ifany")
table(data_24$envpwill_htax, useNA = "ifany")

data_24$envpwill_htax <- as.factor(data_24$envpwill_htax)
data_24$envpwill_lstlv <- as.factor(data_24$envpwill_lstlv)
data_24$envpwill_hpcs <- as.factor(data_24$envpwill_hpcs)


#==========================================#
#           Sample Size                    #
#==========================================#


sample_23 <- data_23 %>%
  dplyr::filter(as.numeric(as.character(wor_clima))>0) %>%
  rename("wor_clima_23" = "wor_clima")

sample_24 <- data_24 %>%
  dplyr::filter(as.numeric(as.character(wor_clima))>0) %>%
  rename("wor_clima_24" = "wor_clima")

sample <- left_join(sample_23, sample_24[, c("pid", "wor_clima_24")],
                    join_by("pid"))

sample$wor_clima_24 = ifelse(as.numeric(as.character(sample$wor_clima_24))<0, NA, as.numeric(as.character(sample$wor_clima_24)))

sample$wor_clima_24 <- as.factor(sample$wor_clima_24)



climate <- sample %>%
  pivot_longer(
    cols = c(wor_clima_23, wor_clima_24),
    names_to = "year", 
    values_to = "wor_clima",
    values_drop_na = TRUE
  )

# Source - https://stackoverflow.com/a/40250493
# Posted by alistaire, modified by community. See post 'Timeline' for change history
# Retrieved 2026-06-01, License - CC BY-SA 4.0

# 
# ggplot(climate, aes(x = year, fill = factor(wor_clima))) +
#   geom_bar(position = "fill") +
#   labs(y = "Count", fill = "") +
#   theme_minimal() +
#   geom_text(aes(label = paste0(plot_data$pct*100,"%")), 
#             position = position_stack(vjust = 0.5), size = 2)
#   scale_fill_brewer(palette = "Paired", labels = c("No Worries", "Some Worries", "Great Worries")) +
#  # ggtitle("Worry about the Consequences of Climate Change") +
#   xlab("Year") +
#   scale_x_discrete(labels=c("2023", "2024")) +
#   theme(axis.text=element_text(size=16),
#         legend.text = element_text(size=14),
#         axis.title.x = element_text(size = 15),
#         axis.title.y = element_text(size = 15),
#        # legend.title = element_text(size= 14),
#         title = element_text(size = 15))

  
  # 1. Prepare data and calculate percentages per year
  plot_data <- climate %>%
    count(year = factor(year), wor_clima = factor(wor_clima)) %>%
    group_by(year) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup()
  
  # 2. Create the filled stacked bar plot
  ggplot(plot_data, aes(x = year, y = pct, fill = wor_clima)) +
    geom_col(position = "fill") +
    # Add percentage labels dynamically using the 'pct' column
    geom_text(aes(label = paste0(round(pct * 100, 1), "%")), 
              position = position_fill(vjust = 0.5), 
              size = 6) + # Increased size slightly from 2 for better visibility
    scale_fill_brewer(palette = "Paired", 
                      labels = c("No Worries", "Some Worries", "Great Worries")) +
    scale_x_discrete(labels = c("2023", "2024")) +
    labs(x = "Year", y = "Percentage", fill = "") +
    theme_minimal() +
    theme(axis.text = element_text(size = 16),
          legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          title = element_text(size = 15))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# PERSONENDATENSÄTZE ERSTELLEN ####

# # tax dummy: those willing to pay higher taxes for the environment are 1
# data_24$envpwill_v1_htax <- as.numeric(data_24$envpwill_v1_htax)
# data_24$envpwill_v2_htax <- as.numeric(data_24$envpwill_v2_htax)
# data_24$envpwill_v3_htax <- as.numeric(data_24$envpwill_v3_htax)
# data_24$envpwill_v4_htax <- as.numeric(data_24$envpwill_v4_htax)
# 
# data_24 <- data_24 %>%
#   mutate(tax_dummy = ifelse(envpwill_v1_htax %in% c(1,2) |
#                               envpwill_v2_htax %in% c(1,2) |
#                               envpwill_v3_htax %in% c(1,2) |
#                               envpwill_v4_htax %in% c(1,2), 1, 0))


# keep only those in both yearx
common_fids <- intersect(data_23$fid, data_24$fid)

# create a party preference variable


data_24_na <- as.data.frame(data_24)
data_23_na <- as.data.frame(data_23)
data_23_na$ind_cal_weight <- ifelse(is.na(data_23_na$ind_cal_weight), 3428.367, data_23_na$ind_cal_weight)

# citizenship- binary
data_23_na[["ctzship_dummy"]] <- as.factor(ifelse(data_23_na[["ctzship_rec"]] == "1", 0, 1))

# university yes/no
data_23_na[["tertiary_dummy"]] <- as.factor(ifelse(data_23_na[["hedu"]] == "7", 1, 0))

# kids in HH
data_23_na <- data_23_na %>%
  group_by(hid) %>%
  mutate(
    chld_in_hh = if_else(any(age < 18, na.rm = TRUE), "1", "0")
  ) %>%
  ungroup()
# make sure that noone under the age of 18 is assigned 1
data_23_na[["chld_in_hh"]] <- as.factor(ifelse(data_23_na[["age"]] < 18, 0, data_23_na[["chld_in_hh"]]))

# worktime > 30h/week
data_23_na[["fulltime_dummy"]] <- as.factor(ifelse(data_23_na[["wrktime"]] > 30, 1, 0))

attr_data <- data_23_na %>%
  filter(! fid %in% common_fids)

reten_data <- data_23_na %>%
  filter(fid %in% common_fids)

data_list <- list(data_24_na, data_23_na, attr_data, reten_data)

for (idx in seq_along(data_list)) {
  
  # Pull the dataframe out
  df <- data_list[[idx]]
  
  # 1. Replace numeric < 0 with NA
  numeric_cols <- sapply(df, is.numeric)
  df[numeric_cols] <- lapply(df[numeric_cols], function(x) ifelse(x < 0, NA, x))
  
  # 2. Handle factors (Regex check and convert to numeric)
  df[] <- lapply(df, function(col) {
    if (is.factor(col)) {
      col <- as.character(col)
      col <- ifelse(grepl("^-", col), NA, col)
    #  return(as.numeric(as.factor(col))) # Combine your logic here
    }
    return(col)
  })
  
  # SAVE THE CHANGES back to the list
  data_list[[idx]] <- df
}

data_list[[1]]$envpwill_v1_htax <- as.factor(data_list[[1]]$envpwill_v1_htax)


data_24_na.s <- svydesign(id = ~1, weights = ~ind_cal_weight, data = data_list[[1]])
data_23_na.s <- svydesign(id = ~1, weights = ~ind_cal_weight, data = data_list[[2]])
attr_data.s <- svydesign(id = ~1, weights = ~ind_cal_weight, data = data_list[[3]])
reten_data.s <- svydesign(id = ~1, weights = ~ind_cal_weight, data = data_list[[4]])


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# final sample

sample <- data_23 %>%
  filter(fid %in% common_fids) %>%
  drop_na(wor_clima)

data_24 <- data_24 %>%
  drop_na(wor_clima)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# share of people that are not worried in both waves
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# xx <- data_23 %>%
#   dplyr::filter(wor_clima == 1)
# 
# yy <- data_24 %>%
#   dplyr::filter(fid %in% c(xx$fid)) %>%
#   dplyr::filter(wor_clima == 1)
# 
# length(yy$fid)
# 
# 73 / 1620 * 2

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Verteilung climchex
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# weighted distribution climate change existence
round(100*prop.table(
  svytable(~climchex,
           data_24_na.s)),1)

library(ggplot2)
library(scales) # Needed for clean percentage labels

# 1. Convert your svytable to a clean, usable data frame
df_plot <- as.data.frame(
  round(100 * prop.table(svytable(~climchex, data_24_na.s)), 1)
)

# Rename columns for clarity (adjust "Freq" or "Freq_col" based on your svytable output)
colnames(df_plot) <- c("Category", "Percentage")

# 2. Add descriptive, clean labels for your levels
levels_map <- c(
  "1" = "Natural processes",
  "2" = "Mainly natural processes",
  "3" =  "About equally natural and human",
  "4" = "Mainly human processes",
  "5" = "Human processes",
  "6" = "Climate change is not taking place"
)

df_plot$Category_Label <- factor(
  df_plot$Category, 
  levels = names(levels_map), 
  labels = levels_map
)

# 3. Create the visualization
climchex_plot <- 
  ggplot(df_plot, aes(x = Percentage, y = reorder(Category_Label, desc(Category_Label)))) +
  # Draw sleek bars with a soft blue hue
  geom_col(fill = "#1F78B4", width = 0.7) +
  # Add exact value labels just past the edge of each bar
  geom_text(
    aes(label = paste0(Percentage, "%")), 
    hjust = -0.2, 
    size = 3.5, 
    fontface = "bold", 
    color = "#333333"
  ) +
  # Give the chart breathing room on the X-axis for labels
  scale_x_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Perceptions of Climate Change Causes",
    subtitle = "Weighted population percentages",
    x = "Percentage of Respondents",
    y = NULL
  ) +
  # Clean, minimal academic formatting
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(), # Removes unnecessary horizontal gridlines
    panel.grid.minor = element_blank(),
#    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
#    plot.subtitle = element_text(color = "gray40", margin = margin(b = 15)),
    axis.text.y = element_text(color = "#222222", face = "plain")
  )

ggsave("/data/home/kburt/asep_ihs/Katharina/thesis/data-prep/climchex_plot.png", climchex_plot)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Verteilung climchex
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# weighted distribution climate change existence
round(100*prop.table(
  svytable(~climchex,
           data_24_na.s)),1)

library(ggplot2)
library(scales) # Needed for clean percentage labels

# 1. Convert your svytable to a clean, usable data frame
df_plot <- as.data.frame(
  round(100 * prop.table(svytable(~climchex, data_24_na.s)), 1)
)

# Rename columns for clarity (adjust "Freq" or "Freq_col" based on your svytable output)
colnames(df_plot) <- c("Category", "Percentage")

# 2. Add descriptive, clean labels for your levels
levels_map <- c(
  "1" = "Natural processes",
  "2" = "Mainly natural processes",
  "3" =  "About equally natural and human",
  "4" = "Mainly human processes",
  "5" = "Human processes",
  "6" = "Climate change is not taking place"
)

df_plot$Category_Label <- factor(
  df_plot$Category, 
  levels = names(levels_map), 
  labels = levels_map
)

# 3. Create the visualization
climchex_plot <- 
  ggplot(df_plot, aes(x = Percentage, y = reorder(Category_Label, desc(Category_Label)))) +
  # Draw sleek bars with a soft blue hue
  geom_col(fill = "#1F78B4", width = 0.7) +
  # Add exact value labels just past the edge of each bar
  geom_text(
    aes(label = paste0(Percentage, "%")), 
    hjust = -0.2, 
    size = 3.5, 
    fontface = "bold", 
    color = "#333333"
  ) +
  # Give the chart breathing room on the X-axis for labels
  scale_x_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Perceptions of Climate Change Causes",
    subtitle = "Weighted population percentages",
    x = "Percentage of Respondents",
    y = NULL
  ) +
  # Clean, minimal academic formatting
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(), # Removes unnecessary horizontal gridlines
    panel.grid.minor = element_blank(),
#    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
#    plot.subtitle = element_text(color = "gray40", margin = margin(b = 15)),
    axis.text.y = element_text(color = "#222222", face = "plain")
  )

ggsave("/data/home/kburt/asep_ihs/Katharina/thesis/data-prep/climchex_plot.png", climchex_plot)


#===============================================#
#     wor_clima + taxes                         #
#===============================================#

data_24 <- data_24 %>%
  drop_na(tax_dummy) %>%
  drop_na(wor_clima)

# 01
ggplot(data_24, aes(x = factor(tax_dummy), fill = factor(wor_clima))) +
  geom_bar(position = "fill") +
  labs(y = "Share", fill = "") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired", labels = c("No Worries", "Some Worries", "Great Worries")) +
  ggtitle("Worry about Climate Change by Willingness to Pay Higher Taxes") +
  xlab("Willingness to Pay Higher Taxes for the Environment") +

  scale_x_discrete(labels = c("No", "Yes")) +
  
  theme(axis.text=element_text(size=14),
        legend.text = element_text(size=13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        # legend.title = element_text(size= 14),
        title = element_text(size = 14))


# 02
htax_worry_plot <- 
  ggplot(data_24, aes(x = factor(wor_clima), fill = factor(tax_dummy))) +
  geom_bar(position = "fill") +
  labs(y = "Share", fill = "Willingness to Pay Taxes") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired", labels =  c("No", "Yes")) +
  #ggtitle(" Willingness to Pay Higher Taxes by Climate Worry") +
  xlab("Climate Worry") +
  
  scale_x_discrete(labels = c("No Worries", "Some Worries", "Great Worries")) +
  
  theme(axis.text=element_text(size=16),
        legend.text = element_text(size=16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        # legend.title = element_text(size= 14),
        title = element_text(size = 16))



data_24_na.s <- svydesign(id = ~1, weights = ~ind_cal_weight, data = data_24_na)

results <- round(svyby(~wor_clima, 
            by = ~tax_dummy, 
            design = data_24_na.s, 
            svymean, 
            na.rm = TRUE)[-1],3)[1:3]

htax_worry_plot <- results %>%
  # 1. Create a new column for the Row Labels (Yes/No) based on tax_dummy
  mutate(Willingness = c("No", "Yes")) %>% 
  # Move it to the front so it acts as the first column
  relocate(Willingness, .before = everything()) %>% 
  
  # 2. Start the gt table
  gt() %>%
  
  # 3. Rename your columns for presentation
  cols_label(
    Willingness = "Willing to Pay Higher Taxes?",
    # Replace these strings with the exact original column names in your 'results' data
    wor_clima1 = "No Worries",   
    wor_clima2 = "Some Worries", 
    wor_clima3 = "Great Worries" 
  ) %>%
  
  # 4. Your existing styling
  data_color(
    columns = starts_with("wor_clima"),
    palette = c("white", "lightblue", "dodgerblue", "darkblue"),
    domain = c(0, 1),
    na_color = "grey90"
  ) %>%
  fmt_percent(columns = starts_with("wor_clima"), decimals = 1) %>%
  tab_style(style = cell_borders(sides = c("left"),  weight = px(0.5), color = "grey90"),
            locations = cells_body(rows = 1:2, columns = 2))

#gtsave(htax_worry_plot, "/data/home/kburt/asep_ihs/Katharina/thesis/data-prep/htax_worry_plot.png")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Verteilung envpwill_v1_htax
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

round(100*prop.table(
  svytable(~envpwill_v3_htax,
           data_24_na.s)),1)


# 1. Convert your svytable to a clean, usable data frame
df_plot <- as.data.frame(
  round(100 * prop.table(svytable(~envpwill_v3_htax, data_24_na.s)), 1)
)

# Rename columns for clarity (adjust "Freq" or "Freq_col" based on your svytable output)
colnames(df_plot) <- c("Category", "Percentage")

# 2. Add descriptive, clean labels for your levels
levels_map <- c(
  "1" = "Completely Acceptable",
  "2" = "Somewhat Acceptable",
  "3" =  "Depends",
  "4" = "Rather Unacceptable",
  "5" = "Completely Uncceptable"
)

df_plot$Category_Label <- factor(
  df_plot$Category, 
  levels = names(levels_map), 
  labels = levels_map
)

# 3. Create the visualization
htax_plot <- 
  ggplot(df_plot, aes(x = Percentage, y = reorder(Category_Label, desc(Category_Label)))) +
  # Draw sleek bars with a soft blue hue
  geom_col(fill = "#1F78B4", width = 0.7) +
  # Add exact value labels just past the edge of each bar
  geom_text(
    aes(label = paste0(Percentage, "%")), 
    hjust = -0.2, 
    size = 3.5, 
    fontface = "bold", 
    color = "#333333"
  ) +
  # Give the chart breathing room on the X-axis for labels
  scale_x_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Willingness to Pay Higher Taxes to protect Environment",
    subtitle = "Weighted population percentages",
    x = "Percentage of Respondents",
    y = NULL
  ) +
  # Clean, minimal academic formatting
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(), # Removes unnecessary horizontal gridlines
    panel.grid.minor = element_blank(),
    #    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    #    plot.subtitle = element_text(color = "gray40", margin = margin(b = 15)),
    axis.text.y = element_text(color = "#222222", face = "plain")
  )

ggsave("/data/home/kburt/asep_ihs/Katharina/thesis/data-prep/htax_plot.png", htax_plot)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# wor_clima nach urban level

results <- svyby(~wor_clima, by=~urban, design=data_24_na.s, svymean, na.rm=TRUE)[1:4]

urban_worry_plot <- results %>%
  rename('Urban Level' = urban) %>%
  mutate('Urban Level' = c("urban", "suburban", "rural")) %>%
  gt() %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%

  # 2. Make the first column bold
  tab_style(
    style = cell_text(weight = "bold"),
    # Replace 'your_first_column_name' with your actual column name
    locations = cells_body(columns = "Urban Level") 
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "all", 
      color = "grey", 
      style = "solid", 
      weight = px(1)
    ),
    locations = list(
      cells_column_labels(),
      cells_body()
    ) # This targets all data cells
  ) %>%
  fmt_percent(columns = starts_with("wor_clima"), decimals = 1) %>%
  cols_label(
   # Willingness = "Urban Level",
    # Replace these strings with the exact original column names in your 'results' data
    wor_clima1 = "No Worries",   
    wor_clima2 = "Some Worries", 
    wor_clima3 = "Great Worries" 
  ) %>%
 cols_width(everything() ~ px(150))

gtsave(urban_worry_plot, "/data/home/kburt/asep_ihs/Katharina/thesis/data-prep/urban_worry_plot.png")

length(data_24[which()]$fid)
table(data_23$wor_clima, useNA = "ifany")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# wor_clima nach party preference


results <- svyby(~wor_clima, by=~partyprf, design=data_24_na.s, svymean, na.rm=TRUE)[1:4]

party_worry_plot <- results %>%
  rename('Party Preference' = partyprf) %>%
  mutate('Party Preference' = c("ÖVP", "SPÖ", "FPÖ", "NEOS", "Greens", "Not eligible", "Wouldn't vote", "Vote invalid")) %>%
  gt() %>%
  data_color(
    columns = starts_with("wor_clima"),
    palette = c("white", "lightblue", "darkblue"),
    # We set non-linear breaks: the color shifts faster at the low end
    # Example: White at 0, Lightblue at 0.05, Blue at 0.2, Darkblue at 1.0
    domain = c(0, 1),
    na_color = "grey90"
  ) %>%
  fmt_percent(columns = starts_with("wor_clima"), decimals = 1) %>%
  cols_label(
    # Willingness = "Urban Level",
    # Replace these strings with the exact original column names in your 'results' data
    wor_clima1 = "No Worries",   
    wor_clima2 = "Some Worries", 
    wor_clima3 = "Great Worries" 
  ) %>%
  tab_footnote(
#  data,
  "Individual weights applied",
  locations = NULL,
  placement = c("auto")
)


results <- svyby(~envpwill_htax, by=~partyprf, design=data_24_na.s, svymean, na.rm=TRUE)[1:6]
results <- svyby(~envpwill_hpcs, by=~partyprf, design=data_24_na.s, svymean, na.rm=TRUE)[1:6]
results <- svyby(~envpwill_lstlv, by=~partyprf, design=data_24_na.s, svymean, na.rm=TRUE)[1:6]


results <- cbind(results[1], round(results[2:6], 3))

party_htax_plot <- results %>%
  rename('Party Preference' = partyprf) %>%
  mutate('Party Preference' = c("ÖVP", "SPÖ", "FPÖ", "NEOS", "Greens", "Not eligible", "Wouldn't vote", "Vote invalid")) %>%
  gt() %>%
  data_color(
    columns = starts_with("envpwill"),
    palette = c("white", "lightblue", "darkblue", "black"),
    # We set non-linear breaks: the color shifts faster at the low end
    # Example: White at 0, Lightblue at 0.05, Blue at 0.2, Darkblue at 1.0
    domain = c(0, 1),
    na_color = "grey90"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "all", 
      color = "grey", 
      style = "solid", 
      weight = px(1)
    ),
    locations = list(
      cells_column_labels(),
      cells_body()
    ) # This targets all data cells
  ) %>%
  fmt_percent(columns = starts_with("wor_clima"), decimals = 1) %>%
  cols_label(
    # Willingness = "Urban Level",
    # Replace these strings with the exact original column names in your 'results' data
    # envpwill_htax1 = "Completely acceptable",
    # envpwill_htax2 = "Somewhat acceptable",
    # envpwill_htax3 = "Depends",
    # envpwill_htax4 = "Rather unacceptable",
    # envpwill_htax5 = "Completely unacceptable"
    # 
    # envpwill_hpcs1 = "Completely acceptable",
    # envpwill_hpcs2 = "Somewhat acceptable",
    # envpwill_hpcs3 = "Depends",
    # envpwill_hpcs4 = "Rather unacceptable",
    # envpwill_hpcs5 = "Completely unacceptable"

    envpwill_lstlv1 = "Completely acceptable",
    envpwill_lstlv2 = "Somewhat acceptable",
    envpwill_lstlv3 = "Depends",
    envpwill_lstlv4 = "Rather unacceptable",
    envpwill_lstlv5 = "Completely unacceptable"
    ) %>%
  tab_footnote(
    #  data,
    "Individual weights apply",
    locations = NULL,
    placement = c("auto")
  )

gtsave(party_htax_plot, "/data/home/kburt/asep_ihs/Katharina/thesis/data-prep/envpwill_plots/party_lstlv_plot.png")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Vergleich mit der unweightes distribution
round(proportions(table(data_24_na$climchex)) * 100, 1)

data_24_na[["climchex"]] <- as.factor(data_24_na[["climchex"]])
data_24_na.s <- svydesign(id = ~1, weights = ~ind_cal_weight, data = data_24_na)

round(svyby(~climchex, 
            by = ~bl, 
            design = data_24_na.s, 
            svymean, 
            na.rm = TRUE)[-1],3)

library(gt)
library(scales) # for the col_bin or col_numeric functions

# 1. Run your svyby
results <- svyby(~climchex, by=~urban, design=data_24_na.s, svymean, na.rm=TRUE)[1:7]
results <- svyby(~climchex, by=~hedu, design=data_24_na.s, svymean, na.rm=TRUE)[1:7]
results <- svyby(~climchex, by=~sex_rec, design=data_24_na.s, svymean, na.rm=TRUE)[1:7]
results <- svyby(~climchex, by=~mnacti, design=data_24_na.s, svymean, na.rm=TRUE)[1:7]
# 1 = erwerbstätig, 2 = AL, 3 = Pens, 4 = disab (nur 17P), 5 = Edu, 6 = Zivi (nur 6P!), 7 = Pleave, 8 = Hausarbeit
results <- svyby(~climchex, by=~urban, design=data_24_na.s, svymean, na.rm=TRUE)[1:7]
# 1 = Stadt, 3 = Land
results <- svyby(~climchex, by=~ctzship_rec, design=data_24_na.s, svymean, na.rm=TRUE)[1:7]


# 2. Create the table with enhanced contrast for small numbers
results %>%
  gt() %>%
  data_color(
    columns = starts_with("climchex"),
    palette = c("white", "lightblue", "dodgerblue", "darkblue"),
    # We set non-linear breaks: the color shifts faster at the low end
    # Example: White at 0, Lightblue at 0.05, Blue at 0.2, Darkblue at 1.0
    domain = c(0, 1),
    na_color = "grey90"
  ) %>%
  fmt_percent(columns = starts_with("climchex"), decimals = 1)

# data <-  data_24_na %>% dplyr::filter(climchex != "6" & !is.na(climchex) & !is.na(age))
length(data_24_na %>% filter(climchex != 6))
class(data_24_na$climchex)
data_24_na %>% filter(climchex != "6")

# simple descriptive OLS
data_24_na[["climchex"]] <- as.numeric(data_24_na[["climchex"]])
ols1 <- lm(climchex ~ urban + hedu + sex_rec + mnacti + ctzship_rec + pinctotal, data = data_24_na %>% filter(climchex != 6), weights = ind_cal_weight)
modelsummary(ols1,# stars == TRUE,
             estimate = "{estimate}{stars}",
             statistic = NULL,
             coef_rename = c("urban2" = "suburban", "urban3" = "rural", "hedu2" = "SEKII"),
             gof_map = c("nobs", "aic", "bic"))

# missings
# allen newresps und proxys wurde die Frage nicht gestellt - die hohe Zahl der missings erklärt sich u.a. auch daraus (484 Personen wurden so gefiltert; 48 haben geskipped/abgebrochen)
length(data_24_na[which(is.na(data_24_na$wor_clima) == TRUE & is.na(data_24_na$climchex) == TRUE),]$fid)


# correlation coefficient between climchex and wor_clima
cor(data_24_na[which(!is.na(data_24_na$wor_clima) == TRUE & !is.na(data_24_na$climchex) == TRUE),]$climchex,
    data_24_na[which(!is.na(data_24_na$wor_clima) == TRUE & !is.na(data_24_na$climchex) == TRUE),]$wor_clima)
# 44%



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Verteilung climchcm_resp

data_24_na[["climchcm_resp_rec"]] <- ifelse(data_24_na[["climchcm_resp"]] < 5, 0, 
                                            ifelse(data_24_na[["climchcm_resp"]] > 4 & data_24_na[["climchcm_resp"]] < 8
                                                   , 1, 2))
data_24_na[["climchcm_resp_rec"]] <- as.factor(data_24_na[["climchcm_resp_rec"]])
data_24_na.s <- svydesign(id = ~1, weights = ~ind_cal_weight, data = data_24_na)


results <- svyby(~climchcm_resp_rec, by=~bl, design=data_24_na.s, svymean, na.rm=TRUE)[1:4]
results <- svyby(~climchcm_resp_rec, by=~hedu, design=data_24_na.s, svymean, na.rm=TRUE)[1:4]
results <- svyby(~climchcm_resp_rec, by=~mnacti, design=data_24_na.s, svymean, na.rm=TRUE)[1:4]

results %>%
  gt() %>%
  data_color(
    columns = starts_with("climchcm_resp_rec"),
    palette = c("white", "lightblue", "darkblue"),
    # We set non-linear breaks: the color shifts faster at the low end
    # Example: White at 0, Lightblue at 0.05, Blue at 0.2, Darkblue at 1.0
    domain = c(0, 1),
  ) %>%
  fmt_percent(columns = starts_with("climchcm_resp_rec"), decimals = 1)

table(data_24_na$mnacti)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Verteilung wor_clima

data_24_na[["climchcm_resp_rec"]] <- ifelse(data_24_na[["climchcm_resp"]] < 5, 0, 
                                            ifelse(data_24_na[["climchcm_resp"]] > 4 & data_24_na[["climchcm_resp"]] < 8
                                                   , 1, 2))
data_24_na[["wor_clima"]] <- as.factor(data_24_na[["wor_clima"]])
data_24_na.s <- svydesign(id = ~1, weights = ~ind_cal_weight, data = data_24_na)


results <- svyby(~wor_clima, by=~bl, design=data_24_na.s, svymean, na.rm=TRUE)[1:4]
results <- svyby(~wor_clima, by=~hedu, design=data_24_na.s, svymean, na.rm=TRUE)[1:4]
results <- svyby(~wor_clima, by=~sex_rec, design=data_24_na.s, svymean, na.rm=TRUE)[1:4]


results %>%
  gt() %>%
  data_color(
    columns = starts_with("wor_clima"),
    palette = c("white", "lightblue", "darkblue"),
    # We set non-linear breaks: the color shifts faster at the low end
    # Example: White at 0, Lightblue at 0.05, Blue at 0.2, Darkblue at 1.0
    domain = c(0, 1),
  ) %>%
  fmt_percent(columns = starts_with("wor_clima"), decimals = 1)

table(data_24_na$mnacti)


# correlation coefficient between climchchm_resp and wor_clima
# data_24_na[["climchcm_resp"]] <- as.numeric(data_24_na[["climchcm_resp"]])
# data_24_na[["wor_clima"]] <- as.numeric(data_24_na[["wor_clima"]])

cor(data_24_na[which(!is.na(data_24_na$wor_clima) == TRUE & !is.na(data_24_na$climchcm_resp) == TRUE),]$climchcm_resp,
    data_24_na[which(!is.na(data_24_na$wor_clima) == TRUE & !is.na(data_24_na$climchcm_resp) == TRUE),]$wor_clima)
# 29%

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Verteilung envpwill

# htax
# weighted distribution climate change existence
round(100*prop.table(
  svytable(~envpwill_htax,
           data_24_na.s)),1)

data_24_na[["envpwill_hpcs"]] <- as.factor(data_24_na[["envpwill_hpcs"]])
data_24_na.s <- svydesign(id = ~1, weights = ~ind_cal_weight, data = data_24_na)

results <- svyby(~envpwill_v1_htax, by=~climchex, design=data_24_na.s, svymean, na.rm=TRUE)[1:6]

round(100*prop.table(
  svytable(~envpwill_v1_htax,
           data_24_na.s)),1)

results %>%
  gt() %>%
  data_color(
    columns = starts_with("envpwill_v1_htax"),
    palette = c("white", "lightblue", "dodgerblue", "darkblue"),
    # We set non-linear breaks: the color shifts faster at the low end
    # Example: White at 0, Lightblue at 0.05, Blue at 0.2, Darkblue at 1.0
    domain = c(0, 1),
  ) %>%
  fmt_percent(columns = starts_with("envpwill_v1_htax"), decimals = 1)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ? what next?

# correlation wor_envir mit wor_clima
# correlation coefficient between climchex and wor_clima
cor(data_24_na[which(!is.na(data_24_na$wor_clima) == TRUE & !is.na(data_24_na$wor_envir) == TRUE),]$wor_envir,
    data_24_na[which(!is.na(data_24_na$wor_clima) == TRUE & !is.na(data_24_na$wor_envir) == TRUE),]$wor_clima)
# 78%

cor(data_24_na[which(!is.na(data_24_na$wor_clima) == TRUE & !is.na(data_24_na$wor_crime) == TRUE),]$wor_crime,
    data_24_na[which(!is.na(data_24_na$wor_clima) == TRUE & !is.na(data_24_na$wor_crime) == TRUE),]$wor_clima)
# -0.04%

# gibt's generell Personen, die stärker worried sind, regardless?

# xgboost?









#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# attrition rate descriptives ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# some people don't have weights, which can't be true;
# whatever <- data_23 %>% filter(!is.na(ind_cal_weight))
# mean(whatever$ind_cal_weight)
# # [1] 3428,367

# start tables
sex <- 
  cbind("group" = c("Male", "Female"),
  "total" = round(100*prop.table(
  svytable(~sex_rec,
           data_23_na.s)),1),
"retention" = round(100*prop.table(
  svytable(~sex_rec,
           reten_data.s)),1),
"attrition" = round(100*prop.table(
  svytable(~sex_rec,
           attr_data.s)),1))

age <- 
cbind("group" = "Average age",
      "total" = round(svymean(~age, data_23_na.s, na.rm = TRUE),1),
      "retention" = round(svymean(~age, reten_data.s, na.rm = TRUE),1),
      "attrition" = round(svymean(~age, attr_data.s, na.rm = TRUE),1))

hhinc <- 
  cbind("group" = "Average HH income",
        "total" = round(svymean(~hhinc, data_23_na.s, na.rm = TRUE),1),
        "retention" = round(svymean(~hhinc, reten_data.s, na.rm = TRUE),1),
        "attrition" = round(svymean(~hhinc, attr_data.s, na.rm = TRUE),1))

ctzship <- 
  cbind("group" = c("Citizenship AT", "Other citizenship"),
        "total" = round(100*prop.table(
          svytable(~ctzship_dummy,
                   data_23_na.s)),1),
        "retention" = round(100*prop.table(
          svytable(~ctzship_dummy,
                   reten_data.s)),1),
        "attrition" = round(100*prop.table(
          svytable(~ctzship_dummy,
                   attr_data.s)),1))

wrktime <- 
  cbind("group" = c("Employment > 30h", "Employment < 30h"),
        "total" = round(100*prop.table(
          svytable(~fulltime_dummy,
                   data_23_na.s)),1),
        "retention" = round(100*prop.table(
          svytable(~fulltime_dummy,
                   reten_data.s)),1),
        "attrition" = round(100*prop.table(
          svytable(~fulltime_dummy,
                   attr_data.s)),1))

tertiary_educ <- 
  cbind("group" = c("No tertiary education", "Tertiary education"),
        "total" = round(100*prop.table(
          svytable(~tertiary_dummy,
                   data_23_na.s)),1),
        "retention" = round(100*prop.table(
          svytable(~tertiary_dummy,
                   reten_data.s)),1),
        "attrition" = round(100*prop.table(
          svytable(~tertiary_dummy,
                   attr_data.s)),1))

child_in_hh <- 
  cbind("group" = c("No child in HH", "Min. 1 child in HH"),
        "total" = round(100*prop.table(
          svytable(~chld_in_hh,
                   data_23_na.s)),1),
        "retention" = round(100*prop.table(
          svytable(~chld_in_hh,
                   reten_data.s)),1),
        "attrition" = round(100*prop.table(
          svytable(~chld_in_hh,
                   attr_data.s)),1))


characteristics_table <- rbind(sex, tertiary_educ, ctzship, wrktime, child_in_hh, age, hhinc)
characteristics_table <- as.data.frame(characteristics_table)
rownames(characteristics_table) <- NULL

# 1. Define the line style (width, color, etc.)
my_border <- fp_border(color = "black", width = 1)

# 2. Identify the even rows
even_rows <- seq(2, nrow(characteristics_table), by = 2)

# 3. Apply the border
t <- flextable(characteristics_table) %>%
  hline(i = even_rows, border = my_border, part = "body") %>%
  autofit()

t <-bold(t, part = "header", j = 1:4)


save_as_image(t, "/data/home/kburt/asep_ihs/Katharina/thesis/data/attrition_table.png", expand = 10, res = 200, choices = "png")

