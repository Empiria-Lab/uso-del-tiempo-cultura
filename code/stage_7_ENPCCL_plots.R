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
library(ggplot2)
library(tidyr)
library(tibble)

###############################################################################
## 3. Import profiles
###############################################################################

## Open the board
board_profiles <- board_folder("data/tidy/enpccl_profiles_board")
pin_names <- pin_list(board_profiles)
overall_names <- pin_names[grepl("_overall$", pin_names)]

###############################################################################
## 3. Pre-processing
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
    se_times   = get(paste0("se_", var_name)),
    indicator  = recode(
      time_component,
      "veces"  = "Cantidad anual"
    )
  ) %>%
  ungroup() %>%
  select(
    profile_label, time_set, 
    indicator, mean_times, se_times
  )

## Save CSV
write.csv(overall_long, "data/tidy/enpccl_profiles_overall.csv", fileEncoding = "UTF-8", row.names = FALSE)

###############################################################################
## 4. Cartesian plot
###############################################################################

## Labels
cartesian_plot <- overall_long %>%
  mutate(
    ## Activity labels
    time_set = recode(
      time_set,
      teatro    = "Obra de teatro",
      danza     = "Espectáculo de danza",
      musica    = "Concierto o recital",
      cine      = "Cine",
      artesania = "Exposición de artesanía",
      arte      = "Exposición de arte",
      ppoo      = "Ceremonia PPOO",
      religion  = "Fiesta religiosa"
    ),
    ## time_set = factor(
      ## time_set,
      ## levels = c(
        ## "Act. personales", "Carga global", "Trabajo en ocupación",
        ## "Trabajo no remunerado", "Trabajo voluntario",
        ## "Doméstico no remunerado", "Cuidados no remunerado"
      ## )
    ## ),
    ## Profile labels
    profile_label = recode(
      profile_label,
      "Profile_1" = "Perfil 1",
      "Profile_2" = "Perfil 2",
      "Profile_3" = "Perfil 3",
      "Profile_4" = "Perfil 4",
      "Profile_5" = "Perfil 5",
      "Profile_6" = "Perfil 6"
    ),
    profile_label = factor(
      profile_label,
      levels = c(
        "Perfil 1",
        "Perfil 2",
        "Perfil 3",
        "Perfil 4",
        "Perfil 5",
        "Perfil 6"
      )
    )
  )

pd <- position_dodge(width = 0.5)

## Build the plot in A4/portrait layout
p_overall <- 
  ggplot(
    cartesian_plot,
    aes(
      x      = reorder(time_set, mean_times),
      y      = mean_times ##,
      ## colour = day_type,
      ## shape  = day_type
    )
  ) +
  geom_point(size = 2.6, position = pd, colour = "#fd7014") +
  geom_errorbar(
    aes(
      ymin = mean_times - 1.96 * se_times,
      ymax = mean_times + 1.96 * se_times
    ),
    width     = 0.2,
    linewidth = 0.5,
    position  = pd,
    colour = "#fd7014"
  ) +
  coord_flip() +
  facet_wrap(~ profile_label, ncol = 2) + ## Two profiles per row
  ## Empiria Lab palette
  ## scale_colour_manual(
    ## values = c("#fd7014", "#179bae", "#fd8f47", "#dc5802"),
    ## name   = NULL
  ## ) +
  scale_shape_manual(
    values = c(16, 17),
    name   = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y        = element_blank(),
    axis.title.x        = element_text(margin = margin(t = 6)),
    axis.text.y         = element_text(hjust = 1),
    panel.grid.major.y  = element_blank(),
    panel.grid.minor    = element_blank(),
    legend.position     = "bottom",
    legend.title        = element_text(size = 10),
    legend.text         = element_text(size = 10),
    strip.background    = element_rect(fill = "#393e46", colour = NA),
    strip.text          = element_text(size = 10, face = "bold", colour = "snow"),
    ## plot.title          = element_text(face = "bold"),
    ## plot.caption        = element_text(size = 10, colour = "grey40", hjust = 0)
  ) +
  labs(y = "Cantidad de veces que ha asistado en el último año")

## Print plot
p_overall

## Save as PNG in A4 portrait (300 dpi)
ggsave(
  filename = "results/figures/ENPCCL_overall_profiles.png",
  plot     = p_overall,
  dpi      = 300,
  width    = 18,
  height   = 26,
  units    = "cm"
)

## Save as PDF in A4 portrait (300 dpi)
ggsave(
  filename = "results/figures/ENPCCL_overall_profiles.pdf",
  plot     = p_overall,
  dpi      = 300,
  width    = 18,
  height   = 26,
  units    = "cm",
  device   = cairo_pdf
)

###############################################################################
## 5. Correlations
###############################################################################

## Activity labels
activity_labels <- c(
  teatro    = "Obra de teatro",
  danza     = "Espectáculo de danza",
  musica    = "Concierto o recital",
  cine      = "Cine",
  artesania = "Exposición de artesanía",
  arte      = "Exposición de arte",
  ppoo      = "Ceremonia PPOO",
  religion  = "Fiesta religiosa"
)

## Function to build wide data
make_wide <- function(measure) {
  overall_long %>%
    filter(indicator == measure) %>%
    select(profile_label, time_set, mean_times) %>%
    pivot_wider(
      id_cols    = c(profile_label),
      names_from = time_set,
      values_from = mean_times
    )
}

annual_wide <- make_wide("Cantidad anual")

## Correlation matrices for Pearson
annual_mat <- annual_wide %>%
  ## select(starts_with("t_")) %>%
  select(where(is.numeric)) %>% 
  as.matrix()

cor_annual <- cor(annual_mat, use = "pairwise.complete.obs", method = "pearson")

## Function to convert to long format for plotting
cor_to_long <- function(cor_mat) {
  as.data.frame(cor_mat) %>%
    rownames_to_column("var1") %>%
    pivot_longer(
      cols = -var1,
      names_to = "var2",
      values_to = "r"
    ) %>%
    mutate(
      var1_label = activity_labels[var1],
      var2_label = activity_labels[var2]
    )
}

cor_annual_long <- cor_to_long(cor_annual)

## Correlations heatmap
p_cor_annual <-
  ggplot(
    cor_annual_long,
    aes(x = var1_label, y = var2_label, fill = r)
  ) +
  geom_tile(colour = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
  scale_fill_gradient2(
    limits   = c(-1, 1),
    low      = "#179bae", ## Empiria turquoise for negative
    mid      = "white",
    high     = "#fd7014", ## Empiria pumpkin for positive
    name     = "Pearson r"
  ) +
  coord_fixed() +
  theme_minimal(base_size = 12) +
  theme(
    axis.title    = element_blank(),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    panel.grid    = element_blank(),
    legend.position = "right",
    legend.title  = element_text(size = 10),
    legend.text   = element_text(size = 10),
    ## plot.title    = element_text(face = "bold"),
    ## plot.caption  = element_text(size = 8, colour = "grey40", hjust = 0)
  )

## Print plot
p_cor_annual

## Save as PNG (300 dpi)
ggsave(
  filename = "results/figures/ENPCCL_correlations.png",
  plot     = p_cor_annual,
  dpi      = 300,
  width    = 20,
  height   = 14,
  units    = "cm"
)

## Save as PDF (300 dpi)
ggsave(
  filename = "results/figures/ENPCCL_correlations.pdf",
  plot     = p_cor_annual,
  dpi      = 300,
  width    = 20,
  height   = 14,
  units    = "cm",
  device   = cairo_pdf
)
