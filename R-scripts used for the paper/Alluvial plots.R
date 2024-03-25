#Assuming you have done categirisation for contineous variables. In our approach MRI derived whole body fat mass was categorised accordint to median quantiles for BMI classes lower trhechoald values ('Normal weight', 'Overweight', 'Obese')

library(ggplot2)
library(ggalluvial)
library(dplyr)

# Read the data
data_for_plot <- read.csv('C:/path_to_file/your_data.csv')
# Convert sex to a readable format (optional)
data_for_plot$Sex <- ifelse(data_for_plot$sex == 0, 'Female', 'Male')

# Group and count the data
grouped_data <- data_for_plot %>%
  group_by(BMI_Category, Sex, MRI_Category) %>%
  summarise(Counts = n(), .groups = 'drop')


theme_set(theme_minimal(base_size = 6))

# Function to create alluvial plot
create_alluvial_plot <- function(data, title) {
  grouped_data <- data %>%
    group_by(BMI_Category, MRI_Category) %>%
    summarise(Counts = n(), .groups = 'drop')
  
  ggplot(grouped_data,
         aes(axis1 = BMI_Category, axis2 = MRI_Category, y = Counts)) +
    geom_alluvium(aes(fill = BMI_Category)) +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 6) +
    theme_minimal() +
    theme(text = element_text(size = 6),
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(),  # Remove minor grid lines
          panel.background = element_blank()) + # Remove panel background
    ggtitle(title)
}

# Filter data for male and female
data_male <- data_for_plot[data_for_plot$Sex == 'Male', ]
data_female <- data_for_plot[data_for_plot$Sex == 'Female', ]

# Create and display the plot for male
plot_male <- create_alluvial_plot(data_male, "Alluvial Plot for Female Participants")
print(plot_male)

# Create and display the plot for female
plot_female <- create_alluvial_plot(data_female, "Alluvial Plot for male Participants")
print(plot_female)

quantiles_info <- data_for_plot %>%
  group_by(BMI_Category) %>%
  summarise(
    Q25 = quantile(Whole_body_fat_mass, 0.25),
    Median = quantile(Whole_body_fat_mass, 0.5),
    Q75 = quantile(Whole_body_fat_mass, 0.75)
  ) %>%
  ungroup()

# Function to create alluvial plot with ordered BMI categories
create_alluvial_plot <- function(data, title) {
  # Convert BMI_Category to a factor with specified levels
  data$BMI_Category <- factor(data$BMI_Category, levels = c('Normal weight', 'Overweight', 'Obese'))
  
  grouped_data <- data %>%
    group_by(BMI_Category, MRI_Category) %>%
    summarise(Counts = n(), .groups = 'drop')
  
  ggplot(grouped_data,
         aes(axis1 = BMI_Category, axis2 = MRI_Category, y = Counts)) +
    geom_alluvium(aes(fill = BMI_Category)) +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 6) +
    theme_minimal() +
    theme(text = element_text(size = 6),
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(),  # Remove minor grid lines
          panel.background = element_blank()) + # Remove panel background
    ggtitle(title)
}

# Filter data for male and female
data_male <- data_for_plot[data_for_plot$Sex == 'Male', ]
data_female <- data_for_plot[data_for_plot$Sex == 'Female', ]

# Create and display the plot for male
plot_female <- create_alluvial_plot(data_male, "Alluvial Plot for Female Participants")
print(plot_female)

# Create and display the plot for female
plot_male <- create_alluvial_plot(data_female, "Alluvial Plot for Male Participants")
print(plot_male)

# Recalculate grouped data including Counts for plotting
grouped_data <- data_for_plot %>%
  group_by(Sex, BMI_Category, MRI_Category) %>%
  summarise(Counts = n(), .groups = 'drop')

# Updated function to create alluvial plots with quantiles in titles
create_alluvial_plot_with_quantiles <- function(grouped_data, sex, quantiles_info) {
  # Filter grouped data for the specified sex
  data_sex <- filter(grouped_data, Sex == sex)
  
  # Dynamically generate title with quantiles info
  title_text <- paste("Alluvial Plot for", sex, "Participants \ Quantiles for Whole Body Fat Mass:",
                      paste(quantiles_info$BMI_Category, 
                            "Q25:", quantiles_info$Q25, 
                            "Median:", quantiles_info$Median, 
                            "Q75:", quantiles_info$Q75, 
                            collapse = " | "))
  
  # Create the plot
  plot <- ggplot(data_sex,
                 aes(axis1 = BMI_Category, axis2 = MRI_Category, y = Counts)) +
    geom_alluvium(aes(fill = BMI_Category)) +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 6) +
    theme_minimal() +
    theme(text = element_text(size = 6),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    ggtitle(title_text)
  
  return(plot)
}

# Ensure quantiles_info is calculated as shown previously

# Create and display the plot for male and female, with quantiles in titles
plot_male_with_quantiles <- create_alluvial_plot_with_quantiles(grouped_data, "Male", quantiles_info)
plot_female_with_quantiles <- create_alluvial_plot_with_quantiles(grouped_data, "Female", quantiles_info)
print(plot_male_with_quantiles)
print(plot_female_with_quantiles)