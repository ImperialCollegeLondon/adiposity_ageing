library(ggplot2)
library(ggridges)
library(cowplot)
library(tidyr)
library(RNOmni)
library(gridExtra)
library(dplyr)
library(grid)

# Load dataset from a specified path
data_ridge <- read.csv("C:/path_to_file/your_data.csv")
# Remove rows with missing values
data_ridge <- na.omit(data_ridge)
# Display column names of the dataset
colnames(data_ridge)

# Apply a global theme to all plots with a base font size of 6
theme_set(theme_minimal(base_size = 6))

# Convert dataset from wide to long format, excluding specified columns
# This prepares the data for ridgeline plots, focusing on selected variables
your_data_long <- gather(data_ridge, key = "var_1", value = "var_2", -var_3, -var_4, -var_5, -var_6, -var_7, -var_8, -var_9, -var_10)

# Create the first ridgeline plot for adiposity phenotypes
gg1 <- ggplot(your_data_long, aes(x = var_2, y = var_1, fill = var_5)) +
  geom_density_ridges(alpha = 0.7) +
  scale_fill_manual(values = c("Male" = "#00798c", "Female" = "#404080")) +
  theme_ridges() +
  theme_minimal() +
  labs(title = "Adiposity phenotypes") +
  theme(plot.title = element_text(size = 6))
print(gg1)

# Prepare data for a second ridgeline plot by excluding different set of variables
your_data_long_2 <- gather(data_ridge, key = "var_1", value = "var_2", -var_3, -var_4, -var_5, -var_6, -var_7, -var_11, -var_12, -var_13, -var_14, -var_15)

# Create the second ridgeline plot
gg2 <- ggplot(your_data_long_2, aes(x = var_2, y = var_1, fill = var_5)) +
  geom_density_ridges(alpha = 0.7) +
  scale_fill_manual(values = c("Male" = "#00798c", "Female" = "#404080")) +
  theme_ridges() +
  theme_minimal() +
  labs(title = "BMI and MRI assessed adiposity") +
  theme(plot.title = element_text(size = 6))
print(gg2)

# Prepare data for a third ridgeline plot, focusing on normalized variables
your_data_long_3 <- gather(data_ridge, key = "var_1", value = "var_2", -var_3, -var_4, -var_5, -var_6, -var_7, -var_8, -var_9, -var_10)
# Display column names of the reshaped data frame
colnames(your_data_long_3)

# Create the third ridgeline plot for normalized adiposity phenotypes
gg3 <- ggplot(your_data_long_3, aes(x = var_2, y = var_1, fill = var_5)) +
  geom_density_ridges(alpha = 0.7) +
  scale_fill_manual(values = c("Male" = "#00798c", "Female" = "#404080")) +
  theme_ridges() +
  theme_minimal() +
  labs(title = "Adiposity phenotypes (normalized)") +
  theme(plot.title = element_text(size = 6))
print(gg3)

# Prepare data for the fourth ridgeline plot, excluding a different set of variables
your_data_long_4 <- gather(data_ridge, key = "var_1", value = "var_2", -var_3, -var_4, -var_5, -var_6, -var_7, -var_11, -var_12, -var_13, -var_14, -var_15)
# Display column names of the reshaped data frame for the fourth plot
colnames(your_data_long4)

# Create the fourth ridgeline plot for BMI and MRI assessed adiposity (normalized)
gg4 <- ggplot(your_data_long_4, aes(x = var_2, y = var_1, fill = var_5)) +
  geom_density_ridges(alpha = 0.7) +
  scale_fill_manual(values = c("Male" = "#00798c", "Female" = "#404080")) +
  theme_ridges() +
  theme_minimal() +
  labs(title = "BMI and MRI assessed adiposity (normalized)") +
  theme(plot.title = element_text(size = 6))
print(gg4)

# Create the fifth ridgeline plot for android and gynoid adipose tissue deposition
gg5 <- ggplot(your_data_long_5, aes(x = var_2, y = var_1, fill = var_6)) +
  geom_density_ridges(alpha = 0.7) +
  scale_fill_manual(values = c("Male" = "#00798c", "Female" = "#404080")) +
  theme_ridges() +
  theme_minimal() +
  labs(title = "Android and gynoid adipose tissue deposition") +
  theme(plot.title = element_text(size = 6))
print(gg5)

# Prepare data for the sixth ridgeline plot
your_data_long_6 <- gather(data_angy_na, key = "var_1", value = "var_2", -var_6, -var_7)
# Display column names of the reshaped data frame for the sixth plot
colnames(your_data_long_6)

# Create the sixth ridgeline plot for normalized android and gynoid adipose tissue deposition
gg6 <- ggplot(your_data_long_6, aes(x = var_2, y = var_1, fill = var_6)) +
  geom_density_ridges(alpha = 0.7) +
  scale_fill_manual(values = c("Male" = "#00798c", "Female" = "#404080")) +
  theme_ridges() +
  theme_minimal() +
  labs(title = "Android and gynoid adipose tissue deposition (normalized)") +
  theme(plot.title = element_text(size = 6))
print(gg6)

# Adjust plots to remove legends and blank axis titles, and to customize grid lines and text sizes
# Detailed customization options are applied to improve visual aesthetics and clarity of the plots

# Combine plots into a grid layout, arranging them in rows and columns
# The layout includes a top row, middle row, bottom row, and combines them into a final comprehensive plot
# Each row may include multiple plots, organized for coherent presentation of the study's findings

# The final step prints the combined plot, showcasing the study's results in a visually engaging format
print(combined_plot)

# Arrange multiple plots in a grid, adjusting the layout to fit all plots cohesively
combined_plot <- grid.arrange(gg1, gg3, gg2, gg4, gg5, gg6, ncol = 2, nrow = 3)