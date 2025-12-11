###############################################################################
## SCRIPT ID:
## Estudio análisis de uso del tiempo y relación con perfiles de participación cultural
## November and December 2025
## R version 4.5.0 (2025-04-11) -- "How About a Twenty-Six"

## Author: Bastián González-Bustamante
## Email: bastian.gonzalezbustamante@empirialab.cl
## Website: https://empirialab.cl
###############################################################################

###############################################################################
## 1. Clean environment
###############################################################################

## Clean environment
rm(list = ls())

###############################################################################
## 2. Packages
###############################################################################

library(pins)
library(purrr)
library(dplyr)

###############################################################################
## 3. Analysis by group
###############################################################################

## Change the target and iterate the code
## target_var <- "NSE"
## target_var <- "grupos_etarios"
## target_var <- "nivel_educ" ## Filter nivel_educ == 0 at the end
target_var <- "sexo"

###############################################################################
## 4. Import profiles
###############################################################################

## Open the board
board_profiles <- board_folder("data/tidy/enpccl_profiles_board")
pin_names <- pin_list(board_profiles)
overall_names <- pin_names[grepl(paste0("_", target_var, "$"), pin_names)]

###############################################################################
## 5. Processing
###############################################################################

## Read overall profiles
overall_profiles_list <- overall_names %>%
  set_names() %>%
  map(~ pin_read(board_profiles, .x))

overall_df <- imap_dfr(
  overall_profiles_list,
  ~ .x %>% mutate(pin_name = .y)
)

## Kind-of pivoting
overall_long <- overall_df %>%
  rowwise() %>%
  ## Column name (e.g. t_ap_ds)
  mutate(
    var_name = paste0(time_set, "_", time_component)
  ) %>%
  ## Pull mean and SEs
  mutate(
    mean_times = get(var_name),
    se_times   = get(paste0("se")),
    indicator   = recode(
      time_component,
      "veces"  = "Cantidad anual"
    )
  ) %>%
  ungroup() %>%
  select(
    profile_label, time_set, {{target_var}},
    indicator, mean_times, se_times 
  ) ## %>%
  ## filter(nivel_educ != 0) ## Only for educational level

## Save CSV
write.csv(overall_long, paste0("data/tidy/enpccl_profiles_", target_var, ".csv"), fileEncoding = "UTF-8", row.names = FALSE)
