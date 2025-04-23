library(tidyverse)

# load original dataset
path <- "../"
filename <- "synthetic_data_stats_competition_2025_final.xlsx"

data <- read_excel(paste(path, filename, sep=""), sheet = "in")

# load data dictionary
filename <- "Data dictionary_stats competition 2025_final.xlsx"
data_dict <- read_excel(filename)

# rename some mismatched cols 
data <- data %>% rename (obstructive_sleep_apnea_icd10=obstructive._sleep_apnea_icd10)

# convert boolean variables to factors
cat_vars <- data_dict %>%
  filter(`Variable type` == "boolean") %>%
  pull(`Variable name`)

data_clean <- data

for (var in cat_vars) {
  data_clean[[var]] <- as.factor(data_clean[[var]])
}

write.csv(data_clean, paste(path, 'cleaned_CSSC_data.csv', sep=""))
