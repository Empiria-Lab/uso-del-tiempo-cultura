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
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)

###############################################################################
## 3. Analysis of profiles
###############################################################################

## Overall profiles
df <- read.csv("data/tidy/enpccl_profiles_overall.csv")

## Variable to save plots
scree_plot_flag <- "ENPCCL_scree"
pca_plot_flag <- "ENPCCL_acp"

###############################################################################
## 4. PCA and clustering on profiles
###############################################################################

## Labels
df_sub <- df %>%
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
        "Perfil 1", "Perfil 2", "Perfil 3",
        "Perfil 4", "Perfil 5", "Perfil 6"
      )
    )
  )

## Profiles and activities matrix
profile_activity_mat <- df_sub %>%
  select(profile_label, time_set, mean_times) %>%
  pivot_wider(
    names_from  = time_set,
    values_from = mean_times,
    values_fill = 0
  )

profile_labels <- profile_activity_mat$profile_label

X <- profile_activity_mat %>%
  select(-profile_label) %>%
  as.matrix()

## Standardise activities
X_scaled <- scale(X)

## PCA on profiles
pca_profiles   <- prcomp(X_scaled, center = FALSE, scale. = FALSE)
var_explained  <- pca_profiles$sdev^2 / sum(pca_profiles$sdev^2)

## Scree data frame
scree_df <- data.frame(
  PC  = seq_along(var_explained),
  var = var_explained
)

## PCA labels
pc1_lab <- paste0("Componente Principal 1 (", percent(var_explained[1], accuracy = 1), ")")
pc2_lab <- paste0("Componente Principal 2 (", percent(var_explained[2], accuracy = 1), ")")

## Empiria Lab palette for clusters
pal_empiria  <- c("#fd7014", "#179bae", "#fd8f47", "#dc5802")
shapes_emp   <- c(16, 17, 15, 18)

###############################################################################
## 5. Scree plot
###############################################################################

## Scree plot
p_scree <-
  ggplot(scree_df, aes(x = PC, y = var)) +
  geom_line(linewidth = 0.6, colour = "#179bae") +
  geom_point(size = 2.6, colour = "#179bae", shape = 15) +
  scale_x_continuous(breaks = scree_df$PC) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x       = element_text(margin = margin(t = 6)),
    panel.grid         = element_blank(),
    legend.position    = "none",
  ) +
  labs(
    x     = "Componente principal",
    y     = "Varianza explicada"
  )

## Print plot
p_scree

## Save as PNG (300 dpi)
ggsave(
  filename = paste0("results/figures/", scree_plot_flag, ".png"),
  plot     = p_scree,
  dpi      = 300,
  width    = 20,
  height   = 14,
  units    = "cm"
)

## Save as PDF (300 dpi)
ggsave(
  filename = paste0("results/figures/", scree_plot_flag, ".pdf"),
  plot     = p_scree,
  dpi      = 300,
  width    = 20,
  height   = 14,
  units    = "cm",
  device   = cairo_pdf
)

###############################################################################
## 6. Clustering on first PCs
###############################################################################

## Scores and number of PCs and clusters
scores <- as.data.frame(pca_profiles$x) %>%
  mutate(profile = profile_labels)

n_pc <- 3

dist_profiles <- dist(scores[, 1:n_pc], method = "euclidean")
hc_profiles   <- hclust(dist_profiles, method = "ward.D2")

k <- 3
profile_clusters <- cutree(hc_profiles, k = k)

scores_clustered <- scores %>%
  mutate(
    profile = factor(profile, levels = levels(df_sub$profile_label)),
    cluster = factor(profile_clusters)
  )

## Seed for labels
set.seed(2024)

## PCA plot
p_pca <-
  ggplot(scores_clustered,
         aes(x = PC1, y = PC2, colour = cluster, shape = cluster)) +
  geom_point(size = 5) +
  ggrepel::geom_text_repel(
    aes(label = profile),
    size        = 4,
    show.legend = TRUE,
    max.overlaps = Inf
  ) +
  scale_colour_manual(
    values = pal_empiria,
    name   = "Clúster"
  ) +
  scale_shape_manual(
    values = shapes_emp,
    name   = "Clúster"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x     = element_text(margin = margin(t = 6)),
    panel.grid       = element_blank(),
    legend.position  = "right",
    legend.title  = element_text(size = 10),
    legend.text      = element_text(size = 10)
  ) +
  labs(
    x = pc1_lab,
    y = pc2_lab
  ) +
  expand_limits(
    x = range(scores_clustered$PC1) + c(-0.2, 0.2),
    y = range(scores_clustered$PC2) + c(-0.2, 0.2)
  )

## Print plot
p_pca

## Save as PNG (300 dpi)
ggsave(
  filename = paste0("results/figures/", pca_plot_flag, ".png"),
  plot     = p_pca,
  dpi      = 300,
  width    = 20,
  height   = 14,
  units    = "cm"
)

## Save as PDF (300 dpi)
ggsave(
  filename = paste0("results/figures/", pca_plot_flag, ".pdf"),
  plot     = p_pca,
  dpi      = 300,
  width    = 20,
  height   = 14,
  units    = "cm",
  device   = cairo_pdf
)
