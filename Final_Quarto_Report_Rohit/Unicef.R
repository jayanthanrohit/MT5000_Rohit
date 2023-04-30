
#########################################################################################################################################################
#----Loading libraries----#
#########################################################################################################################################################

#Load libraries
library(readxl)
library(dplyr)
library(readr)
library(tidyverse)
library(lmtest)

#########################################################################################################################################################
#----Data import----#
#########################################################################################################################################################

#import USD-INR exchange rate data 
unicef_data1 <- read_csv("unicef_indicator_1.csv")
unicef_data2 <- read_csv("unicef_indicator_2.csv")
View(unicef_data1)
View(unicef_data2)

#########################################################################################################################################################
#----Data Preparation----#
#########################################################################################################################################################

head(unicef_data1)
head(unicef_data2)

# Choosing total %  in sex to compare between countries
unicef_data1_total <- unicef_data1 %>% filter(sex == "Total")

#number of countries in given range of children_developed
country_counts <- unicef_data1_total %>%
  mutate(score_group = cut(Children_developed, breaks = c(50, 60, 70, 80, 90, 100))) %>%
  group_by(score_group) %>%
  summarize(count = n())

#average index across years
df_avg <- unicef_data1_total %>% group_by(Year) %>% summarize(avg_children_dev = mean(Children_developed))



#########################################################################################################################################################
#----Data Visualization----#
#########################################################################################################################################################

# 1. World Map
library(maps)
library(ggplot2)

world_map <- map_data("world")

# Merge data with world map data
merged_data <- merge(world_map, unicef_data1_total, by.x = "region", by.y = "country", all.x = TRUE)
head(merged_data)

# Create the plot
ggplot(data = merged_data, aes(x = long, y = lat, fill = merged_data$Children_developed)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), color = "gray") +
  coord_fixed(1.3) +
  theme_void() +
  scale_fill_gradient(name = "% of children development on track",
                      low = "red", high = "#006400") +
  labs(title = "Children development index by country") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


# 2. Bar chart

# create the bar chart
ggplot(country_counts, aes(x = score_group, y = count)) +
  geom_bar(stat = "identity", fill = "#1B9E77") +
  labs(x = "% range", y = "Number of Countries", title = "Distribution of countries by % of children_developed")



# 3. Scatter plot

# Create the scatter plot with a linear regression line
plot(Children_developed ~ Year, data = unicef_data1_total, main = "Scatterplot of % score vs. Year",
     xlab = "% children devlopment", ylab = "Year")
abline(lm(Children_developed ~ Year, data = unicef_data1_total), col = "red")


# 4. Time series chart

# Create the plot
ggplot(df_avg, aes(x = Year, y = avg_children_dev)) +
  geom_line() +
  labs(title = "Children Developed over Time",
       x = "Year",
       y = "Children Developed") +
  theme_bw()



