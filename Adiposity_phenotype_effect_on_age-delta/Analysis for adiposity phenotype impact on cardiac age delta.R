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
library(RNOmni)
library(ggalluvial)
library(ggpubr)

# Read and prepare your dataset
df.sc <- read_csv("your data.csv") %>% na.omit()

# Analyze the effect of adiposity phenotypes, age, age squared, and sex-specific BMI interaction on cardiovascular aging using linear regression
fitobes <- lm(ca_delta ~ adiposity phenotype of choice + age_at_mri + poly(age_at_mri, 2) + sex_BMI, fat.peri)
summary(fitobes) # Display summary statistics of the model
confint(fitobes, level = .95) # Display 95% confidence intervals for model coefficients

# Perform the same analysis focusing only on females
fitobes_female <- lm(ca_delta ~ poly(age_at_mri, 2) + sex_BMI*BMI, fat.peri) # Interaction term considers sex-specific effect
summary(fitobes_female) # Summary for female-specific model
confint(fitobes_female, level = .95) # 95% confidence intervals for female-specific model

# Perform the analysis focusing only on males
fitobes_male <- lm(ca_delta ~ poly(age_at_mri, 2) + I(1-sex_BMI)*BMI, fat.peri) # Interaction term adjusted for males
summary(fitobes_male) # Summary for male-specific model
confint(fitobes_male, level = .95) # 95% confidence intervals for male-specific model
# Note: Complete analysis sequence for each adiposity phenotype

# Extracting tidy output with confidence intervals for various models
out_conf1 <- tidy(fitobes, conf.int = TRUE)
out_conf1_female <- tidy(fitobes_female, conf.int = TRUE)
out_conf1_male <- tidy(fitobes_male, conf.int = TRUE)

# Round the coefficient dataframes and remove the intercept row for cleaner output
lm_model_out1 <- round_df(out_conf1, digits=10)
lm_model_out1 <- lm_model_out1[2,] # Remove the intercept for overall
lm_model_out_conf1_female <- round_df(out_conf1_female, digits=10)
lm_model_out_conf1_female <- lm_model_out_conf1_female[5,] # Remove the intercept for females
lm_model_out_conf1_male <- round_df(out_conf1_male, digits=10)
lm_model_out_conf1_male <- lm_model_out_conf1_male[5,] # Remove the intercept for males

# Combine the processed data into intermediate dataframes and assign group labels for overall, female, and male analyses
lm_intermediate_1 <- rbind(lm_model_out1, lm_model_out_conf1_female, lm_model_out_conf1_male) 
lm_intermediate_1$group <- c("Overall", "Female", "Male")

# Assuming similar steps were taken for other models (not shown), combine all intermediate dataframes
lm_intermediate_overall <-  rbind(lm_intermediate_1, lm_intermediate_2, lm_intermediate_3, lm_intermediate_4...)

#Creating a forest plot

# 'lm_intermediate_overall' is assumed to be a dataset containing regression analysis results from previous script
forestlm_ <- ggplot(lm_intermediate_overall, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, fill = group, col = group)) + 
  geom_errorbar(linewidth = 0.5, position = position_dodge(width = 0.5), width = 0) + # Draws error bars for confidence intervals
  geom_point(size = 2, shape = 21, colour = "white", stroke = 0.5, position = position_dodge(width = 0.5), na.rm = TRUE) + # Adds the point estimates
  scale_fill_manual(values = c("Female" = "#404080", "Male" = "#00798c", "Overall" = "grey50")) + # Custom colors for the groups
  scale_color_manual(values = c("Female" = "#404080", "Male" = "#00798c", "Overall" = "grey50")) + # Ensures consistency in group coloring
  scale_x_discrete(name = " ") + # Removes x-axis title
  scale_y_continuous(name = "Beta coefficient", limits = c(-1.5, 1.7)) + # Sets y-axis title and limits
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Adds a horizontal line at zero for reference
  coord_flip() + # Flips the coordinates to make it easier to read
  theme(
    legend.position = "none", # Hides the legend
    panel.background = element_blank(), # Removes background
    panel.grid.major = element_blank(), # Removes major grid lines
    panel.grid.minor = element_blank(), # Removes minor grid lines
    axis.text.x = element_text(size=6, color='black'), # Customizes x-axis text
    axis.text.y = element_text(size=6, color='black'), # Customizes y-axis text
    axis.line.x = element_line(size = 0.8, linetype = "solid"), # Customizes x-axis line
    axis.line.y = element_blank(), # Removes y-axis line
    axis.title.x = element_text(size=6, vjust=0.3, face="plain", color = 'black'), # Customizes x-axis title
    axis.title.y = element_text(size=6, face = "plain", vjust=0.9, angle = 90, colour = 'black'), # Customizes y-axis title
    plot.margin = margin(l = 0,r=0) # Adjusts plot margins
  )

# Print the forest plot
print(forestlm_bysex2)

# Creates a second plot for displaying p-values and confidence intervals next to the forest plot
# Assumes 'lm_intermediate_overall' dataset and uses the 'term' as x-axis and an arbitrary y-axis position for labels
p_right <- ggplot(lm_intermediate_overall) +
  geom_text(size = 6 /.pt,
            aes(x=term, y = 1, group=group, label = paste(round(p.value,9), ", ", round(estimate,3)," (", round(conf.low,3), " - ", round(conf.high,3), ") ",  sep = "")),
            hjust = 1, position=position_dodge(width = 0.5)
  ) +
  coord_flip() + # Flips the coordinates to align with the main forest plot
  theme( legend.position = "none", # Hides the legend
         panel.background = element_blank(), # Removes background
         panel.grid.major = element_blank(), # Removes major grid lines
         panel.grid.minor = element_blank(), # Removes minor grid lines
         axis.text.x  = element_text(size=0.005, color='white'), # Makes x-axis text invisible
         axis.text.y  = element_text(size=0.005, color='white'), # Makes y-axis text invisible
         axis.line.x = element_line(size = 0.8, linetype = "solid", color="white"), # Makes x-axis line invisible
         axis.line.y = element_blank(), # Removes y-axis line
         axis.title.x  = element_text(size=0.005, vjust=0.3, face="plain", color = 'white'), # Makes x-axis title invisible
         axis.title.y  = element_text(size=0.005, face = "plain", vjust=0.9, angle = 90, colour = 'white'), # Makes y-axis title invisible
         axis.ticks.x = element_line(color="white"), # Makes x-axis ticks invisible
         axis.ticks.y = element_line(color="white"), # Makes y-axis ticks invisible
         panel.border = element_blank(), # Removes panel border
         panel.spacing = unit(c(0, 0, 0, 0), "null"), # Adjusts panel spacing
         plot.margin = margin(l = 0,r=0) # Adjusts plot margins
  )

# Adjusts margins for the forest plot to make room for the p-values plot
forestlm_ <- forestlm_ + 
  theme(plot.margin = margin(l = 5, r = -20, t = 10, b = 10, unit = "pt")) 

# Adjusts margins for the p-values plot to align it properly with the forest plot
p_right <- p_right + 
  theme(plot.margin = margin(l = -80, r = 80, t = 6, b = 14, unit = "pt"))

# Combines the forest plot and the p-values plot side by side with adjusted widths for each
grid_name <- plot_grid(forestlm_bysex2, p_right, rel_widths = c(1, 1))
