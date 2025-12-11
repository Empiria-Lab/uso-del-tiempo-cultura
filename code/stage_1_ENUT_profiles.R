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

library(dplyr)
library(survey)
library(haven)
library(pins)

###############################################################################
## 3. Import data
###############################################################################

## ENUT II
enut <- readRDS("data/raw/250403-ii-enut-bdd-r-v2.RDS")

## Board to store parquet
board_profiles <- board_folder("data/tidy/enut_profiles_board")

## Cleaning missing values 
vec_vars_ppales <- na.omit(unique(stringr::str_extract(
  string  = names(enut),
  pattern = "^(p|t)\\_.*\\_(ds|fds|dt)$"
)))

## Missing in age is -96
## enut <- enut %>%
## mutate(across(all_of(vec_vars_ppales), ~if_else(. == 96, NA, .)))

## Preprocessing
enut <- enut %>%
  mutate(
    ## Age: if -96 is missing
    edad = as.numeric(edad),
    edad = ifelse(edad == -96, NA_real_, edad),
    ## Disability: 1 = yes, 2 = no, 96 = missing
    pesd = as.numeric(pesd),
    pesd = case_when(
      pesd == 1  ~ 1,
      pesd == 2  ~ 0,
      pesd == 96 ~ NA_real_,
      TRUE       ~ NA_real_
      ),
    ## Mirroring ENUT-ENPCCL
    grupos_etarios = case_when(
      edad >= 15 & edad <= 29 ~ 1, 
      edad >= 30 & edad <= 64 ~ 2,
      edad >= 65 ~ 3,
      TRUE ~ NA
      ),
    nivel_educ = case_when(
      nivel_educ == 2 ~ 1,
      nivel_educ == 3 ~ 2,
      nivel_educ == 4 ~ 2,
      nivel_educ == 5 ~ 3,
      TRUE ~ NA)
    )

###############################################################################
## 4. Profiles
###############################################################################

## Household-level flags
enut_hh_flags <- enut %>%
  group_by(id_hog) %>%
  summarise(
    hh_n = n(),
    hh_has_elderly   = any(edad >= 65, na.rm = TRUE), ## New threshold
    hh_has_child_0_5 = any(edad <= 5,  na.rm = TRUE),
    hh_has_disabled  = any(pesd == 1, na.rm = TRUE)
  )

## Profiles elaboration
enut2 <- enut %>%
  left_join(enut_hh_flags, by = "id_hog") %>%
  mutate(
    ## 1. Personas en hogares con personas mayores
    g1_person_with_elderly = as.integer(hh_has_elderly),
    ## 2. Personas en hogares con personas en situación de discapacidad
    g2_person_with_disabled = as.integer(hh_has_disabled),
    ## 3. Personas en hogares con hijos de 0-5 años
    g3_person_with_child0_5 = as.integer(hh_has_child_0_5),
    ## 4. Personas mayores
    g4_elderly = as.integer(edad >= 65), ## New threshold
    ## 5. Personas adultas en hogares unipersonales
    g5_single_adult = as.integer(hh_n == 1 & edad >= 30 & edad < 65), ## New range
    ## Useful flags
    ## is_adult        = edad >= 18, ## Redundant
    is_young_adult  = edad >= 18 & edad <= 29, ## Parejas jóvenes, Observatorio Social
    is_minor        = edad < 18,
    is_couple       = c8a == 1 ## New variable
  )

## Young couple w/o children
enut_young_couple_hh <- enut2 %>%
  group_by(id_hog) %>%
  summarise(
    hh_n = first(hh_n),
    ## all_adults       = all(is_adult, na.rm = TRUE), ## Redundant
    any_minor        = any(is_minor, na.rm = TRUE),
    all_young_adults = all(is_young_adult, na.rm = TRUE),
    couple           = all(is_couple, na.rm = TRUE)
  ) %>%
  mutate(
    hh_young_couple_no_children =
      ## hh_n == 2 & all_adults & !any_minor & all_young_adults ## Redundant
      hh_n == 2 & !any_minor & all_young_adults ## Including new variable
  ) %>%
  select(id_hog, hh_young_couple_no_children)

## Merge
enut3 <- enut2 %>%
  left_join(enut_young_couple_hh, by = "id_hog") %>%
  mutate(
    ## 6. Parejas jóvenes sin hijos/as
    g6_young_couple_no_children =
      as.integer(hh_young_couple_no_children)
  )

###############################################################################
## 5. Survey design
###############################################################################

## Filter
enut_cut <- enut3 %>%
  filter(tiempo == 1)

## Sampling design
design_enut <- svydesign(
  data    = enut_cut,
  strata  = ~ varstrat,
  ids     = ~ varunit,
  weights = ~ fe_cut
)

## Single-PSU strata
options(survey.lonely.psu = "certainty")

###############################################################################
## 6. Time variables per profile
###############################################################################

## Time-variable pairs
time_sets <- list(
  t_ap    = c("t_ap_ds", "t_ap_fds"),
  t_cgt   = c("t_cgt_ds", "t_cgt_fds"),
  t_to    = c("t_to_ds", "t_to_fds"),
  t_tnr   = c("t_tnr_ds", "t_tnr_fds"),
  t_tvaoh = c("t_tvaoh_ds", "t_tvaoh_fds"),
  t_tdnr  = c("t_tdnr_ds", "t_tdnr_fds"),
  t_tcnr  = c("t_tcnr_ds", "t_tcnr_fds")
)

## Profiles
profiles <- data.frame(
  id    = 1:6,
  var   = c(
    "g1_person_with_elderly",
    "g2_person_with_disabled",
    "g3_person_with_child0_5",
    "g4_elderly",
    "g5_single_adult",
    "g6_young_couple_no_children"
  ),
  label = paste0("Profile_", 1:6),
  stringsAsFactors = FALSE
)

## Grouping variables
group_vars <- c("NSE", "grupos_etarios", "nivel_educ", "sexo")

## Loop over profiles and time sets
for (i in seq_len(nrow(profiles))) {
  
  p_id    <- profiles$id[i]
  p_var   <- profiles$var[i]
  p_label <- profiles$label[i]
  
  design_p <- subset(design_enut, get(p_var) == 1)
  if (nrow(design_p$variables) == 0) next
  
  for (set_name in names(time_sets)) {
    
    vars <- time_sets[[set_name]]
    
    ## Loop over each component (ds/fds) separately
    for (v in vars) {
      
      ## Labelling and pin names
      comp <- if (grepl("_ds$", v)) {
        "ds"
      } else if (grepl("_fds$", v)) {
        "fds"
      } else {
        "other"
      }
      
      form <- as.formula(paste("~", v))
      
      ## Overall mean for profile × time-set × component
      est <- svymean(form, design_p, na.rm = TRUE)
      
      df_overall <- data.frame(t(coef(est)))
      se_overall <- data.frame(t(SE(est)))
      names(se_overall) <- paste0("se_", names(se_overall))
      
      df_overall <- cbind(
        profile_id     = p_id,
        profile_label  = p_label,
        time_set       = set_name,
        time_var       = v,
        time_component = comp,
        df_overall,
        se_overall
      )
      
      pin_name_overall <- sprintf("profile_%d_%s_%s_overall",
                                  p_id, set_name, comp)
      
      pin_write(
        board = board_profiles,
        x     = df_overall,
        name  = pin_name_overall,
        type  = "parquet"
      )
      
      ## By-group means for NSE, sexo, etc.
      for (gv in group_vars) {
        
        form_group <- as.formula(paste0("~", gv))
        
        df_g <- svyby(
          formula = form,
          by      = form_group,
          design  = design_p,
          FUN     = svymean,
          na.rm   = TRUE
        )
        
        df_g <- as.data.frame(df_g)
        
        df_g$profile_id     <- p_id
        df_g$profile_label  <- p_label
        df_g$time_set       <- set_name
        df_g$time_var       <- v
        df_g$time_component <- comp
        df_g$group_var      <- gv
        
        ## Reorder columns
        df_g <- df_g[, c(
          "profile_id",
          "profile_label",
          "time_set",
          "time_var",
          "time_component",
          "group_var",
          gv,
          setdiff(
            names(df_g),
            c("profile_id", "profile_label",
              "time_set", "time_var", "time_component",
              "group_var", gv)
          )
        )]
        
        pin_name_group <- sprintf("profile_%d_%s_%s_by_%s",
                                  p_id, set_name, comp, gv)
        
        pin_write(
          board = board_profiles,
          x     = df_g,
          name  = pin_name_group,
          type  = "parquet"
        )
      }
    }
  }
}

## Sanity check
svyby(
  ~ t_ap_ds,
  ## Profiles
  ~ g1_person_with_elderly,
  design_enut,
  svymean,
  na.rm = TRUE
)

## Sanity check
svyby(
  ~ t_ap_ds,
  ## Profiles
  ~ g1_person_with_elderly + NSE,
  design_enut,
  svymean,
  na.rm = TRUE
)