# Load the necessary libraries
library(ggplot2)     # For creating plots
library(ggalluvial)  # For creating alluvial plots
library(dplyr)       # For data manipulation
library(readr)       # For reading CSV files
library(viridis)     # For color palettes

# Load your dataset
# For anonymity, the actual path is not displayed. Replace 'your_file_path.csv' with the actual file path.
data <- read_csv("your_file_path.csv")

# Filter out specific categories that should not be included in the analysis
data <- data %>%
  filter(BMI_category != "Above 55.2", Whole_body_fat_mass_category != "Below 5.00")

# Ensure categories are ordered as factors to maintain the specified order in plots
data$BMI_category <- factor(data$BMI_category, levels = c("40.0 - 55.2", "30.0 - 39.9", "25.0 - 29.9", "18.5 - 24.9", "14.5 - 18.5"))
data$Whole_body_fat_mass_category <- factor(data$Whole_body_fat_mass_category, levels = c("51.40 - 96.50", "30.20 - 51.40", "20.40 - 30.20", "8.10 - 20.40", "5.00 - 8.10"))

# Function to create and save the alluvial plot for a specific sex
create_alluvial_plot_by_sex <- function(df, sex_value = NULL, sex_label = "Overall") {
  if (!is.null(sex_value)) {
    data_sex <- df %>% filter(sex == sex_value)
    sex_name <- ifelse(sex_value == 0, "Males", "Females")
    file_name <- paste("alluvial_plot_", sex_name, ".pdf", sep = "")
    plot_title <- paste("Alluvial Diagram of BMI and Whole Body Fat Mass Categories for", sex_name)
  } else {
    data_sex <- df
    file_name <- "alluvial_plot_Overall.pdf"
    plot_title <- "Alluvial Diagram of BMI and Whole Body Fat Mass Categories (Overall)"
  }
  
  # Prepare data for the alluvial plot by counting occurrences of each category combination
  alluvial_data <- data_sex %>%
    count(BMI_category, Whole_body_fat_mass_category) %>%
    filter(!is.na(Whole_body_fat_mass_category))  # Ensure no NA categories are included
  
  # Create the alluvial plot
  gg <- ggplot(data = alluvial_data,
               aes(axis1 = BMI_category, axis2 = Whole_body_fat_mass_category, y = n, fill = Whole_body_fat_mass_category)) +
    geom_alluvium() +
    geom_stratum(fill = "white") +  # Set the stratum columns to white
    geom_text(stat = 'stratum', aes(label = after_stat(stratum))) +
    scale_fill_viridis(discrete = TRUE, option = "D") +  # Use viridis color palette
    theme_minimal() +
    ggtitle(plot_title) +
    xlab("BMI Category") +
    ylab("Count") +
    theme(panel.grid = element_blank(),  # Remove grid lines
          legend.position = "none")  # Remove legend
  
  # Print the plot
  print(gg)
  
  # Save the plot to a file
  ggsave(file_name, plot = gg, device = "pdf", width = 12, height = 8)
}

# Generate and save plots
create_alluvial_plot_by_sex(data)            # Overall plot
create_alluvial_plot_by_sex(data, 0, "Males") # Plot for Males
create_alluvial_plot_by_sex(data, 1, "Females") # Plot for Females
