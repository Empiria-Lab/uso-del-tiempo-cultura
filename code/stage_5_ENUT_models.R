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
library(pins)
library(lmtest)
library(plm)
library(stargazer)
library(fmsb)
library(performance)
library(brms)
library(sjPlot)

###############################################################################
## 3. Import profiles
###############################################################################

## Aggregated estimates
df1 <- read.csv("data/tidy/enut_profiles_NSE.csv")
df2 <- read.csv("data/tidy/enut_profiles_grupos_etarios.csv")
df3 <- read.csv("data/tidy/enut_profiles_nivel_educ.csv")
df4 <- read.csv("data/tidy/enut_profiles_sexo.csv")

## Sociodemographic flag
df1$subsample <- "NSE"
df2$subsample <- "Age"
df3$subsample <- "Edu"
df4$subsample <- "Sex"

## Merge
merged_df <- bind_rows(df1, df2, df3, df4)

## Reference category
merged_df <- merged_df %>%
  mutate(
    profile_label = relevel(factor(profile_label), ref = "Profile_6")
  )

###############################################################################
## 4. OLS models
###############################################################################

## Naive and FEs models
ols_1 <- lm(mean_hours ~ profile_label, data = subset(merged_df, time_set == "t_ap"))
ols_2 <- lm(mean_hours ~ profile_label + day_type + subsample, data = subset(merged_df, time_set == "t_ap"))
ols_3 <- lm(mean_hours ~ profile_label, data = subset(merged_df, time_set == "t_cgt"))
ols_4 <- lm(mean_hours ~ profile_label + day_type + subsample, data = subset(merged_df, time_set == "t_cgt"))
ols_5 <- lm(mean_hours ~ profile_label, data = subset(merged_df, time_set == "t_to"))
ols_6 <- lm(mean_hours ~ profile_label + day_type + subsample, data = subset(merged_df, time_set == "t_to"))
ols_7 <- lm(mean_hours ~ profile_label, data = subset(merged_df, time_set == "t_tnr"))
ols_8 <- lm(mean_hours ~ profile_label + day_type + subsample, data = subset(merged_df, time_set == "t_tnr"))

## Save models
pin_write(board_folder("results/models"), ols_1, name = "ENUT_ols_1", versioned = TRUE)
pin_write(board_folder("results/models"), ols_2, name = "ENUT_ols_2", versioned = TRUE)
pin_write(board_folder("results/models"), ols_3, name = "ENUT_ols_3", versioned = TRUE)
pin_write(board_folder("results/models"), ols_4, name = "ENUT_ols_4", versioned = TRUE)
pin_write(board_folder("results/models"), ols_5, name = "ENUT_ols_5", versioned = TRUE)
pin_write(board_folder("results/models"), ols_6, name = "ENUT_ols_6", versioned = TRUE)
pin_write(board_folder("results/models"), ols_7, name = "ENUT_ols_7", versioned = TRUE)
pin_write(board_folder("results/models"), ols_8, name = "ENUT_ols_8", versioned = TRUE)

###############################################################################
## 5. Breusch-Pagan test
###############################################################################

## Breusch-Pagan test for heteroscedasticity (it's not in the table)
## p-value 95% suggests robust SEs
bptest(ols_1) ## Robust SEs are NOT necessary
bptest(ols_2) ## Robust SEs are necessary
bptest(ols_3) ## Robust SEs are necessary
bptest(ols_4) ## Robust SEs are necessary
bptest(ols_5) ## Robust SEs are NOT necessary
bptest(ols_6) ## Robust SEs are necessary
bptest(ols_7) ## Robust SEs are NOT necessary
bptest(ols_8) ## Robust SEs are NOT necessary
## I am going to use H1 anyway because of the size of the sample and \hat{y}

###############################################################################
## 6. Generate models table
###############################################################################

## Reestimating p-values
pval_list <- list(
  coeftest(ols_1, vcov = vcovHC(ols_1, type = "HC1"))[, 4],
  coeftest(ols_2, vcov = vcovHC(ols_2, type = "HC1"))[, 4],
  coeftest(ols_3, vcov = vcovHC(ols_3, type = "HC1"))[, 4],
  coeftest(ols_4, vcov = vcovHC(ols_4, type = "HC1"))[, 4],
  coeftest(ols_5, vcov = vcovHC(ols_5, type = "HC1"))[, 4],
  coeftest(ols_6, vcov = vcovHC(ols_6, type = "HC1"))[, 4],
  coeftest(ols_7, vcov = vcovHC(ols_7, type = "HC1"))[, 4],
  coeftest(ols_8, vcov = vcovHC(ols_8, type = "HC1"))[, 4]
)

## LaTeX table
stargazer(
  ols_1, ols_2, ols_3, ols_4, ols_5, ols_6, ols_7, ols_8,
  type            = "latex",
  header          = FALSE,
  style           = "ajps",
  out             = "results/tables/ENUT_OLS_models.tex",
  title           = "Efecto de los perfiles en el uso del tiempo",
  model.names     = FALSE,
  dep.var.labels  = c("Horas de actividad"),
  se = list(
    coeftest(ols_1, vcov = vcovHC(ols_1, type = "HC1"))[, 2],
    coeftest(ols_2, vcov = vcovHC(ols_2, type = "HC1"))[, 2],
    coeftest(ols_3, vcov = vcovHC(ols_3, type = "HC1"))[, 2],
    coeftest(ols_4, vcov = vcovHC(ols_4, type = "HC1"))[, 2],
    coeftest(ols_5, vcov = vcovHC(ols_5, type = "HC1"))[, 2],
    coeftest(ols_6, vcov = vcovHC(ols_6, type = "HC1"))[, 2],
    coeftest(ols_7, vcov = vcovHC(ols_7, type = "HC1"))[, 2],
    coeftest(ols_8, vcov = vcovHC(ols_8, type = "HC1"))[, 2]
  ),
  p = pval_list,
  notes.align     = "c",
  model.numbers   = FALSE,
  omit.stat       = c("f", "ser"),
  column.labels   = c("AP", "AP", "CGT", "CGT", "TO", "TO", "TNR", "TNR"),
  omit            = c(
    "day_type", "subsample"
  ),
  add.lines = list(
    c("Errores robustos", "Sí", "Sí", "Sí", "Sí", "Sí", "Sí", "Sí", "Sí"),
    c("Tipo de día", "No", "Sí", "No", "Sí", "No", "Sí", "No", "Sí"),
    c("Sociodemográficos", "No", "Sí", "No", "Sí", "No", "Sí", "No", "Sí"),
    c("VIF",
      format(round(as.numeric(VIF(ols_1)), 3), nsmall = 3),
      format(round(as.numeric(VIF(ols_2)), 3), nsmall = 3),
      format(round(as.numeric(VIF(ols_3)), 3), nsmall = 3),
      format(round(as.numeric(VIF(ols_4)), 3), nsmall = 3),
      format(round(as.numeric(VIF(ols_5)), 3), nsmall = 3),
      format(round(as.numeric(VIF(ols_6)), 3), nsmall = 3),
      format(round(as.numeric(VIF(ols_7)), 3), nsmall = 3),
      format(round(as.numeric(VIF(ols_8)), 3), nsmall = 3)
    ),
    c("AIC",
      format(round(as.numeric(model_performance(ols_1)[1]), 3), nsmall = 3),
      format(round(as.numeric(model_performance(ols_2)[1]), 3), nsmall = 3),
      format(round(as.numeric(model_performance(ols_3)[1]), 3), nsmall = 3),
      format(round(as.numeric(model_performance(ols_4)[1]), 3), nsmall = 3),
      format(round(as.numeric(model_performance(ols_5)[1]), 3), nsmall = 3),
      format(round(as.numeric(model_performance(ols_6)[1]), 3), nsmall = 3),
      format(round(as.numeric(model_performance(ols_7)[1]), 3), nsmall = 3),
      format(round(as.numeric(model_performance(ols_8)[1]), 3), nsmall = 3)
    )
  ),
  covariate.labels = c(
    "Perfil 1",
    "Perfil 2",
    "Perfil 3",
    "Perfil 4",
    "Perfil 5"
  )
)

###############################################################################
## 7. Joint models
###############################################################################

## Incorporating SEs and extra residual variance (sigma)
bform <- bf(
  mean_hours | se(se_hours, sigma = TRUE) ~ profile_label + day_type + subsample
)

## Priors informed by profiles
global_mu <- mean(merged_df$mean_hours, na.rm = TRUE)
global_sd <- sd(merged_df$mean_hours, na.rm = TRUE)
priors <- c(
  ## prior(normal(global_mu, global_sd * 2), class = "Intercept"),
  prior(normal(6.3, 10), class = "Intercept"),
  ## prior(normal(0, global_sd / 2), class = "b"),
  prior(normal(0, 2.5), class = "b"),
  ## prior(exponential(1 / global_sd), class = "sigma")
  prior(exponential(0.2), class = "sigma")
)

## Bayesian models

bayesian_1 <- brm(
  formula = bform,
  data    = subset(merged_df, time_set == "t_ap"),
  family  = gaussian(),
  prior   = priors,
  chains  = 4,
  cores   = 4,
  iter    = 4000,
  seed    = 2004
)

bayesian_2 <- brm(
  formula = bform,
  data    = subset(merged_df, time_set == "t_cgt"),
  family  = gaussian(),
  prior   = priors,
  chains  = 4,
  cores   = 4,
  iter    = 4000,
  seed    = 2004
)

bayesian_3 <- brm(
  formula = bform,
  data    = subset(merged_df, time_set == "t_to"),
  family  = gaussian(),
  prior   = priors,
  chains  = 4,
  cores   = 4,
  iter    = 4000,
  seed    = 2004
)

bayesian_4 <- brm(
  formula = bform,
  data    = subset(merged_df, time_set == "t_tnr"),
  family  = gaussian(),
  prior   = priors,
  chains  = 4,
  cores   = 4,
  iter    = 4000,
  seed    = 2004
)

## Friendly-export
tab_model(bayesian_1, bayesian_2, bayesian_3, bayesian_4,
          digits = 3, collapse.ci = TRUE, file = "results/tables/ENUT_Bayesian_models.html",
          dv.labels = c("AP", "CGT", "TO", "TNR"),
          pred.labels = c("Constant ","Profile 1", "Profile 2", "Profile 3", "Profile 4", "Profile 5"),
          rm.terms = c("day_typeDíadesemana", "subsampleEdu", "subsampleNSE", "subsampleSex"))

