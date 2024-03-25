library(readr)
library(tidyverse)
library(dbplyr)
library(ggplot2)
library(patchwork)
library(tidyverse)
library(Matrix)
library(lme4)
library(emmeans)
library(ISwR)
library(ggridges)
library(MatchIt)
library(dplyr)
library(jtools)
library(broom)
library(forestmangr)
library(WeightIt)
library(wesanderson)
library(RNOmni)
library(MASS)
library(viridis)
library(gridExtra)
library(grid)

# Read and prepare your dataset
df.sc <- read_csv("your data.csv") %>% na.omit()

# Function to create plots for each adiposity feature
create_adiposity_plots <- function(df, adiposity_features, y_limits, labels) {
  plots <- map2(adiposity_features, y_limits, ~{
    ggplot(df) +
      geom_jitter(aes_string(x = "age_at_mri", y = .x, color = "factor(sex)"), shape = 16, size = 1.5, alpha = 0.1) +
      geom_smooth(aes_string(x = "age_at_mri", y = .x), method = "lm", size = 0.5, color = "black") +
      geom_density2d(aes_string(x = "age_at_mri", y = .x), alpha = 1, color = "black") +
      ylim(.y) +
      labs(x = "Age (years)", y = labels[[.x]], color = "Sex") +
      scale_color_viridis_d() +
      theme_minimal() +
      theme(panel.grid = element_blank(), panel.background = element_blank())
  })
  return(plots)
}

# Specify adiposity features and limits
adiposity_features <- c("Visceral_adipose_tissue_volume", "Subcutaneous_fat_body", "Trunk_fat_mass", "Whole_body_fat_mass", "Muscle_fat_infiltration", "Total_abdominal_adipose_tissue_index")
y_limits <- list(c(0, 15), c(0, 25), c(0, 42), c(0, 80), c(0, 21), c(0, 14))
labels <- setNames(c("VAT", "ASAT", "TTFM", "WBFM", "MFI", "TAAT"), adiposity_features)

# Create and print adiposity plots
adiposity_plots <- create_adiposity_plots(df.sc, adiposity_features, y_limits, labels)
print(adiposity_plots)

# Create a function for biochemical and endocrine biomarker plots if needed, following a similar pattern.
# Combine plots using patchwork or grid.arrange as needed. For example:
combined_plot <- reduce(adiposity_plots, `+`) + plot_layout(ncol = 3)
print(combined_plot)

# Note: Specific details for biochemical_endocrine_biomarkers_m and biochemical_endocrine_biomarkers_f are omitted for brevity.
# Adapt the create_adiposity_plots function similarly for these datasets.
