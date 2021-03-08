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
# CREATE CATEGORY (Quality >= 6.5: 1 = "good", Quality < 6.5: 0 = "bad")
winequality <- rbind(winequality_red, winequality_white) %>%
  mutate(type = factor(type),
         class = as.factor(ifelse(quality >= 6.5, 1, 0))) %>%
  select(quality, class, type, names(winequality_red)[1:11])

# CHECK TO MAKE SURE COLUMNS ARE FORMATTED CORRECT
summary(winequality)

# EXPORT FULL DATA
write.csv(winequality, file = "data/winequality-all.csv", row.names = F, na = "")