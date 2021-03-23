# LOAD LIBRARIES
library(tidyverse)
library(readxl)
library(readr)

# IMPORT DATA
winequality_red   <- read_csv("data/winequality-red.csv") %>% 
  mutate(type = "red")
winequality_white <- read_delim("data/winequality-white.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(type = "white")

# COMBINE RED AND WHITE
# CREATE CATEGORY (Low Quality: 3, 4; Normal: 5, 6, 7; High Quality: 8, 9)
winequality <- rbind(winequality_red, winequality_white) %>%
  mutate(type = factor(type),
         qualityclass = as.factor(ifelse(quality <= 4, "Low",
                                         ifelse(quality <= 7, "Normal",
                                                "High")))) %>%
  mutate(qualityclass = factor(qualityclass, levels = c("Low", "Normal", "High"))) %>%
  select(quality, qualityclass, type, names(winequality_red)[1:11])

# CHECK TO MAKE SURE COLUMNS ARE FORMATTED CORRECT
summary(winequality)

# EXPORT FULL DATA
write.csv(winequality, file = "data/winequality-all.csv", row.names = F, na = "")
