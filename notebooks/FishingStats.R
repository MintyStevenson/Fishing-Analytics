
library(readxl)

library(dplyr)

library(ggplot2)

library(psych)

library(ggmap)

# Set as Working Directory
data <- read_excel("2023FishingStatistics.xlsx")

# Assuming your "Time" column is in a data frame named "data"
# Convert the "Time" column to POSIXlt format
data$Time <- as.POSIXlt(data$Time, format = "%Y-%m-%d %H:%M:%S")

# Format the "Time" column to display only the time
data$Time <- format(data$Time, format = "%H:%M:%S")

################################################################################
# Group the data by "Name" and compute descriptive statistics for the "Weight (lbs)" column
weight_stats <- data %>%
  group_by(Name) %>%
  summarise(Count = n(),
            Min = min(`Weightlbs`),
            Q1 = quantile(`Weightlbs`, 0.25),
            Median = median(`Weightlbs`),
            Q3 = quantile(`Weightlbs`, 0.75),
            Max = max(`Weightlbs`),
            Mean = mean(`Weightlbs`),
            SD = sd(`Weightlbs`))

# Print the descriptive statistics for each name
weight_stats
################################################################################

# Create a box plot of weight by location with enhanced visualizations
ggplot(data, aes(x = Location, y = `Weightlbs`)) +
  geom_boxplot(fill = "skyblue", color = "black", outlier.color = "red") +
  labs(x = "Location", y = "Weight (lbs)",
       title = "Box Plot of Weight by Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
################################################################################

# Create a box plot of weight by name
ggplot(data, aes(x = Name, y = `Weightlbs`)) +
  geom_boxplot() +
  labs(x = "Name", y = "Weight (lbs)",
       title = "Box Plot of Weight by Name") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
################################################################################

ggplot(data, aes(`Weightlbs`)) +
  geom_histogram(binwidth = 1, fill = 'skyblue', color = 'black') +
  labs(x = "Weight (lbs)", y = "Frequency", title = "Histogram of Weight") +
  theme_minimal()
###############################################################################


# Assuming the Time column is in date-time format
weight_time <- data %>%
  group_by(Date) %>%
  summarise(Avg_Weight = mean(`Weightlbs`))

ggplot(weight_time, aes(x = Date, y = Avg_Weight)) +
  geom_line(color = 'steelblue') +
  labs(x = "Date", y = "Average Weight (lbs)", title = "Trend of Weight over Time") +
  theme_minimal()
################################################################################

ggplot(data, aes(x = Species, y = `Weightlbs`, fill = Species)) +
  geom_boxplot() +
  facet_wrap(~ `Lake_Pond`) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Species", y = "Weight (lbs)", title = "Box Plot of Weight by Species per Location") +
  theme_minimal()
###############################################################################

# Create a boxplot of weight by Bait Type
ggplot(data, aes(x = `BaitType`, y = `Weightlbs`)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Bait Type", y = "Weight (lbs)", 
       title = "Box Plot of Weight by Bait Type") +
  theme_minimal()
###############################################################################

# Create a boxplot of weight by name and species
ggplot(data, aes(x = Name, y = `Weightlbs`, fill = Species)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Name", y = "Weight (lbs)", title = "Box Plot of Weight by Name and Species") +
  theme_minimal()
###############################################################################

# Ensure your "Date" column is in date format
data$Date <- as.Date(data$Date)

# Group the data by "Date" and compute the average temperature for each date
temp_time <- data %>%
  group_by(Date) %>%
  summarise(Avg_Temp = mean(`WaterTemperature`))

# Create a line graph of temperature over time
ggplot(temp_time, aes(x = Date, y = Avg_Temp)) +
  geom_line(color = 'steelblue', size = 1) +
  geom_point(color = 'darkred', size = 2) +
  labs(x = "Date", y = "Average Water Temperature", title = "Trend of Water Temperature over Time") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
        axis.title = element_text(face = "bold", size = 12), 
        axis.text = element_text(size = 10))
###############################################################################

# Calculate the correlation between WaterTemperature and Weightlbs
correlation <- cor(data$`WaterTemperature`, data$`Weightlbs`, use = "complete.obs")
print(paste("Correlation between water temperature and weight: ", correlation))

# Create a scatter plot of weight by water temperature
ggplot(data, aes(x = `WaterTemperature`, y = `Weightlbs`)) +
  geom_point(color = 'steelblue') +
  geom_smooth(method = lm, se = FALSE, color = "darkred") +
  labs(x = "Water Temperature", y = "Weight (lbs)",
       title = "Scatter plot of Weight by Water Temperature") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
        axis.title = element_text(face = "bold", size = 12), 
        axis.text = element_text(size = 10))
###############################################################################

# Create a scatter plot of Weightlbs vs hPa
ggplot(data, aes(x = `hPa`, y = `Weightlbs`)) +
  geom_point(color = 'steelblue') +
  geom_smooth(method = lm, color = "darkred", se = TRUE) +
  labs(x = "Pressure (hPa)", y = "Weight (lbs)",
       title = "Scatter plot of Weight by Atmospheric Pressure") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
        axis.title = element_text(face = "bold", size = 12), 
        axis.text = element_text(size = 10))

################################################################################

# Create a box plot of weight by moon phase
ggplot(data, aes(x = `MoonPhase`, y = `Weightlbs`)) +
  geom_boxplot(fill = 'skyblue', color = 'black', outlier.color = 'red') +
  labs(x = "Moon Phase", y = "Weight (lbs)",
       title = "Box Plot of Weight by Moon Phase") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
###############################################################################

# Create a scatter plot of weight vs outside temperature
ggplot(data, aes(x = `OutsideTemperature`, y = `Weightlbs`)) +
  geom_point(color = 'steelblue') +
  geom_smooth(method = lm, color = "darkred", level = 0.95, linetype = "solid", fill = "lightgray") +
  labs(x = "Outside Temperature", y = "Weight (lbs)",
       title = "Scatter plot of Weight by Outside Temperature") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
        axis.title = element_text(face = "bold", size = 12), 
        axis.text = element_text(size = 10))

#################################################################################

# Create a box plot of weight by weather
ggplot(data, aes(x = `Weather`, y = `Weightlbs`)) +
  geom_boxplot() +
  labs(x = "Weather", y = "Weight (lbs)", 
       title = "Box Plot of Weight by Weather") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
        axis.title = element_text(face = "bold", size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10))

# Create a violin plot of weight by weather
ggplot(data, aes(x = `Weather`, y = `Weightlbs`)) +
  geom_violin() +
  labs(x = "Weather", y = "Weight (lbs)", 
       title = "Violin Plot of Weight by Weather") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
        axis.title = element_text(face = "bold", size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10))
################################################################################

# Filter the data for "Captains Pond"
data_captains <- data %>% filter(Lake_Pond == "Captains Pond")

# Create a box plot of weight by location for "Captains Pond"
ggplot(data_captains, aes(x = Location, y = `Weightlbs`)) +
  geom_boxplot(fill = "skyblue", color = "black", outlier.color = "red") +
  labs(x = "Location", y = "Weight (lbs)",
       title = "Box Plot of Weight at Captains Pond") +
  theme_minimal()


library(dplyr)

# Filter the data for "Captains Pond"
data_captains <- data %>% filter(Lake_Pond == "Captains Pond")

# Overall descriptive statistics for `Weight` across all locations
overall_stats <- summary(data_captains$Weightlbs)

print(overall_stats)

# Descriptive statistics for `Weight` for each location
location_stats <- data_captains %>%
  group_by(Location) %>%
  summarize(
    Count = n(),
    Mean = mean(Weightlbs, na.rm = TRUE),
    Median = median(Weightlbs, na.rm = TRUE),
    Min = min(Weightlbs, na.rm = TRUE),
    Max = max(Weightlbs, na.rm = TRUE),
    SD = sd(Weightlbs, na.rm = TRUE)
  )

print(location_stats)










