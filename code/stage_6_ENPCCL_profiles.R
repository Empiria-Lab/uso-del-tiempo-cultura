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

## ENPCCL
load("data/raw/enpccl_puf.RData")

## Board to store parquet
board_profiles <- board_folder("data/tidy/enpccl_profiles_board")

## Preprocessing
enpccl <- enpccl %>%
  mutate(
    ## NSE
    rph_nse = as.numeric(rph_nse),
    rph_nse = ifelse(rph_nse == 96, NA_real_, rph_nse),
    ## Age 
    edad_aux = as.numeric(edad_aux),
    edad_aux = ifelse(edad_aux %in% c(-96, 96), NA_real_, edad_aux),
    ## Education
    rph_nivel_consolidado = as.numeric(rph_nivel_consolidado),
    rph_nivel_consolidado = ifelse(rph_nivel_consolidado == 96, NA_real_, rph_nivel_consolidado),
    ## Sex
    rph_sexo = as.numeric(rph_sexo),
    rph_sexo = ifelse(rph_sexo == 96, NA_real_, rph_sexo),
    ## Disability
    rph_discapacidad = case_when(
      rph_disc_hog %in% c(0, 1) ~ as.numeric(rph_disc_hog),
      rph_disc_hog %in% c(96) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    ## Age flags
    is_adult = edad_aux %in% c(2), ## 30–64
    is_elder = edad_aux %in% c(3), ## 65+
    ## Mirroring ENUT-ENPCCL
    NSE = rph_nse,
    grupos_etarios = edad_aux,
    nivel_educ = rph_nivel_consolidado,
    sexo = rph_sexo
    ## Cultural participation NAs are in order
    )

###############################################################################
## 4. Profiles
###############################################################################

## Household-level flags
enpccl_hh_flags <- enpccl %>%
  group_by(idhogar) %>%
  summarise(
    hh_n = n(),
    hh_has_elderly   = any(h_am == 1, na.rm = TRUE),
    hh_has_child_0_5 = any(niño == 1,  na.rm = TRUE),
    hh_has_disabled  = any(rph_disc_hog == 1, na.rm = TRUE),
    hh_tipo_hogar    = dplyr::first(tipo_hogar),
    .groups = "drop"
  )

## Kish respondents 
enpccl_kish <- enpccl %>%
  filter(kish_dummy == 1) %>%
  left_join(enpccl_hh_flags, by = "idhogar")

## Profiles elaboration
enpccl_kish <- enpccl_kish %>%
  mutate(
    ## 1. Personas en hogares con personas mayores
    g1_person_with_elderly = as.integer(hh_has_elderly),
    ## 2. Personas en hogares con personas en situación de discapacidad
    g2_person_with_disabled = as.integer(hh_has_disabled),
    ## 3. Personas en hogares con hijos de 0-5 años
    g3_person_with_child0_5 = as.integer(hh_has_child_0_5),
    ## 4. Personas mayores
    g4_elderly = as.integer(is_elder),
    ## 5. Personas adultas en hogares unipersonales
    g5_single_adult = as.integer(hh_n == 1 & is_adult),
    ## 6. Parejas jóvenes sin hijos/as (tipo_hogar == 2 at household level)
    g6_young_couple_no_children = as.integer(hh_tipo_hogar == 2)
  )

###############################################################################
## 5. Survey design
###############################################################################

## Sampling design
design_enpccl <- svydesign(
  ids    = ~ Varunit,
  strata = ~ Estrato_ENPC,
  weights = ~ fexp_p,
  check.strata = TRUE,
  data   = enpccl_kish
)

## Single-PSU strata
options(survey.lonely.psu = "certainty")

###############################################################################
## Quick sanity checks (DROP)
###############################################################################

# counts by profile
enpccl_kish %>% count(g1_person_with_elderly)
enpccl_kish %>% count(g2_person_with_disabled)
enpccl_kish %>% count(g3_person_with_child0_5)
enpccl_kish %>% count(g4_elderly)
enpccl_kish %>% count(g5_single_adult)
enpccl_kish %>% count(g6_young_couple_no_children)

###############################################################################
## 6. Time variables per profile
###############################################################################

## Cultural activities
culture_sets <- list(
  teatro    = c("teatro_veces"),
  danza     = c("danza_veces"),
  musica    = c("musica_veces"),
  cine      = c("cine_veces"),
  artesania = c("artesania_veces"),
  arte      = c("arte_veces"),
  ppoo      = c("ppoo_veces"),
  religion  = c("religion_veces")
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
  
  design_p <- subset(design_enpccl, get(p_var) == 1)
  if (nrow(design_p$variables) == 0) next
  
  for (set_name in names(culture_sets)) {
    
    vars <- culture_sets[[set_name]]
    
    ## Loop over each component (ds/fds) separately
    for (v in vars) {
      
      ## Labelling and pin names
      comp <- if (grepl("_ds$", v)) {
        "ds"
      } else if (grepl("_fds$", v)) {
        "fds"
      } else {
        "veces"
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
  ~ teatro_veces,
  ## Profiles
  ~ g1_person_with_elderly,
  design_enpccl,
  svymean,
  na.rm = TRUE
)

## Sanity check
svyby(
  ~ teatro_veces,
  ## Profiles
  ~ g1_person_with_elderly + NSE,
  design_enpccl,
  svymean,
  na.rm = TRUE
)