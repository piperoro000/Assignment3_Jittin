
library(tidyverse)
library(dplyr)
library(ggplot2)
platelet_data <- read_delim("RawData/PlateletHW .tsv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)

# Overview of the data
summary(data)

# Checking ADP summary
summary(data$ADP)

# Filter out only negative value
platelet_data_clean <- platelet_data %>% 
  filter(ADP >= 0)

# Eliminate and show the number of outliers after that
total_rows <- nrow(platelet_data_clean)
Q1 <- quantile(platelet_data_clean$ADP, 0.25, na.rm = TRUE)
Q3 <- quantile(platelet_data_clean$ADP, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
platelet_data_clean_filtered <- platelet_data_clean %>%
  filter(ADP >= lower_bound & ADP <= upper_bound)

num_outliers <- total_rows - nrow(platelet_data_clean_filtered)

cat("Number of outliers by IQR method:", num_outliers, "\n")

summary(platelet_data_clean$ADP)

# Show data befor and after cleaning
platelet_data$Status <- "Before"
platelet_data_clean$Status <- "After"

combined_data <- rbind(
  platelet_data %>% select(Status, ADP),
  platelet_data_clean %>% select(Status, ADP)
)

combined_data$Status <- factor(combined_data$Status, levels = c("Before", "After"))

ggplot(combined_data, aes(x = Status, y = ADP, fill = Status)) +
  geom_boxplot() +
  labs(title = "Box Plot: Before and After Cleaning",
       x = "Data Status",
       y = "ADP Values") +
  theme_minimal() +
  scale_fill_manual(values = c("Before" = "red", "After" = "blue"))