#' Author: Giovanni Ghedini
#' Data: Jan 23, 2023
#' Purpose: EDA for Diageo PLC a British multinational alcoholic beverages company


library(rpart)
library(rpart.plot)
library(caret)
library(zoo)
library(caret)
library(ggplot2)
library(dplyr)
library(tseries)
library(rugarch)
library(plotly)
library(rugarch)
library(stringr)
library(data.table)
library(tidyr)

# wd
setwd("~/Desktop/Dual Degree/R/hult_class/Personal_folder/A1_Assignment")

# load the data
my_df <- read.csv("/Users/giovannighedini/Desktop/Dual Degree/R/hult_class/Personal_folder/A1_Assignment/2024-01-23_complete_data_a1_EDA_case.csv")

# explore - summary
names(my_df)
head(my_df)
summary(my_df)

# RANDOM SAMPLE
#set.seed(1234)
#sample_my_df <- my_df %>%
  #sample_frac(0.5) # Random Sample with 50% of the observations


## DATA CLEANING
# Trim
#my_df <- my_df %>%
  #mutate_all(funs(str_trim(.)))
# Missing Values
any(is.na(my_df)) # Missing Values
# Count of MISSING VALUES
colSums(is.na(my_df)) # 8084 missing values in Zip Code 
# Count of BLANKS
colSums(my_df == "") # 8062 blanks in Address, City, and County. 3600 in Category Name, and 5 in Vendor Name

# Rows with NA and Blanks
# nulls_or_blanks <- sample_my_df[rowSums(is.na(sample_my_df) | sample_my_df == "") > 0, ]

# SAMPLE WITHOUT NA and BLANKS
# sample_my_df_clean <- sample_my_df[!(rowSums(is.na(sample_my_df) | sample_my_df == "") > 0), ]

# Trasform in date datatype
my_df$Date <- as.Date(my_df$Date, format = "%Y-%m-%d")
my_df$Store.Number <- as.numeric(my_df$Store.Number)
my_df$Zip.Code <- as.numeric(my_df$Zip.Code)
my_df$Pack <- as.numeric(my_df$Pack)
my_df$Bottle.Volume..ml. <- as.numeric(my_df$Bottle.Volume..ml.)
my_df$State.Bottle.Cost <- as.numeric(my_df$State.Bottle.Cost)
my_df$State.Bottle.Retail <- as.numeric(my_df$State.Bottle.Retail)
my_df$Bottles.Sold <- as.numeric(my_df$Bottles.Sold)
my_df$Sale..Dollars. <- as.numeric(my_df$Sale..Dollars.)
my_df$Month <- as.numeric(my_df$Month)

# RID of Scientific notation
options(scipen = 999)

# add a column that specify the year
my_df$year = format(my_df$Date, "%Y")
my_df$year <- as.numeric(my_df$year)

# ADDING COLUMNS - Margins And Cost/ml
my_df$margin <- my_df$State.Bottle.Retail - my_df$State.Bottle.Cost
my_df$cost_ml <- my_df$State.Bottle.Cost/my_df$Bottle.Volume..ml.

# Changing COL NAMES
# Use make.names() to generate valid names
new_column_names <- make.names(colnames(my_df))

# Rename columns in the data frame
colnames(my_df) <- new_column_names

# Check if there are negative numbers
colSums(my_df < 0)
negative_bottles <- filter(my_df, Sale..Dollars. < 0 | Bottles.Sold < 0)
## 4258 negative numbers of bottles sold. It doesn't make sense --> we'll eliminate these rows since it is just 0.03% of the data

### RE-DEFINE my_df
my_df <- filter(my_df, !(Sale..Dollars. < 0 | Bottles.Sold < 0))
colSums(my_df < 0) ## we still have 198 negative margins! IMPORTANT to see why!


###### OVERVIEW

# Comparisons across the years
df_yearly <- my_df %>%
  group_by(year) %>%
  summarize(Sales = sum(Sale..Dollars.),
            Costs = sum(State.Bottle.Cost),
            Margins = sum(margin),
            Bottles = sum(Bottles.Sold)
  )

# Bar Plot for Sales across the year
ggplot(df_yearly, aes(x = as.factor(year), y = Sales)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = scales::comma_format(scale = 1e-6)(Sales)), vjust = -0.5) +
  labs(title = "Sales Across Years",
       x = "Year",
       y = "Sales (Millions)") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6)) # Remove the background grid

# Bottle Solds across the years
ggplot(df_yearly, aes(x = as.factor(year), y = Bottles)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = scales::comma_format(scale = 1e-6)(Bottles)), vjust = -0.5) +
  labs(title = "Number of Bottles Sold",
       x = "Year",
       y = "Sales (Millions)") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6)) # Remove the background grid

# Total Costs across the years
ggplot(df_yearly, aes(x = as.factor(year), y = Costs)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = scales::comma_format(scale = 1e-6)(Costs)), vjust = -0.5) +
  labs(title = "Cost of Bottles Sold",
       x = "Year",
       y = "Sales (Millions)") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6)) # Remove the background grid

# Profit Margin across years
ggplot(df_yearly, aes(x = as.factor(year), y = Margins)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = scales::comma_format(scale = 1e-6)(Margins)), vjust = -0.5) +
  labs(title = "Profit Margins from Bottles Sold",
       x = "Year",
       y = "Sales (Millions)") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6)) # Remove the background grid


## SALES and COST DISTRIBUTION
summary(my_df$Sale..Dollars.) 
# 50% of the orders are between $41.4 and $162.8. Median = $85.5 (Max = $260832)

# Box Plot - Sales Distribution (every order)
boxplot(my_df$Sale..Dollars, outline = FALSE, main = "Box Plot - Sales Distribution")
# Calculate quartile values
q1 <- quantile(my_df$Sale..Dollars, 0.25)
q3 <- quantile(my_df$Sale..Dollars, 0.75)
# Add numerical labels for the 1st and 3rd quartiles
text(1, q1, paste("Q1: ", round(q1, 2)), pos = 1, offset = 0.5, col = "blue")
text(1, q3, paste("Q3: ", round(q3, 2)), pos = 3, offset = 0.5, col = "blue")

#Calculate tot cost per order
my_df$Tot.cost <- my_df$State.Bottle.Cost*my_df$Bottles.Sold

# Cost Distribution (every order)
summary(my_df$State.Bottle.Cost)
boxplot(my_df$State.Bottle.Cost, outline = FALSE, main = "Box Plot - Costs Distribution")
# Calculate quartile values
q1_cost <- quantile(my_df$State.Bottle.Cost, 0.25)
q3_cost <- quantile(my_df$State.Bottle.Cost, 0.75)
# Add numerical labels for the 1st and 3rd quartiles
text(1, q1_cost, paste("Q1: ", round(q1_cost, 2)), pos = 1, offset = 0.5, col = "blue")
text(1, q3_cost, paste("Q3: ", round(q3_cost, 2)), pos = 3, offset = 0.5, col = "blue")
# 50% of Costs per Bottle lay between $6 and $14.17. Median = $ 11.55. Max = $18436

high_costs <- filter(my_df, my_df$State.Bottle.Cost > quantile(my_df$State.Bottle.Cost, 0.95))
boxplot(high_costs$State.Bottle.Cost, outline = FALSE, main = "High Costs Distribution")
summary(high_costs$State.Bottle.Cost)
## 95% or our bottles cost less than $ 50.49


## SEASONALITY

# Group By month
df_monthly <- my_df %>%
  group_by(year, Month) %>%
  summarize(
    Sales = sum(Sale..Dollars.),
    Costs = sum(State.Bottle.Cost),
    Margins = sum(margin),
    Bottles = sum(Bottles.Sold)
  )

# Plot for monthly seasonality
ggplot(df_monthly, aes(x = as.factor(Month), y = Sales, group = year, color = year)) +
  geom_line() +
  labs(title = "Monthly Seasonality",
       x = "Month",
       y = "Sales (Millions)") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6)) +
  scale_x_discrete(labels = month.abb) +  # Use month abbreviations on the x-axis
  theme_minimal()

# filter dataset to remove year 2019 that has really differend trends from other yrs
grouped_df_without_2019 <- df_monthly %>% filter(year > 2019)

# Bar plot with median of sales for every month
ggplot(grouped_df_without_2019, aes(x = as.factor(Month), y = Sales)) +
  stat_summary(fun = sum, geom = "bar", fill = "skyblue", alpha = 0.7) +
  labs(title = "Tot Sales for Every Month (2020 - 2023)",
       x = "Month",
       y = "Tot Sales (millions)") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6)) +
  scale_x_discrete(labels = month.abb) + 
  theme_minimal()

# Filter dataset years 2019 - 2021 to see before and after covid
grouped_2020_2023 <- df_monthly %>% filter(year > 2019)
# Line Chart
ggplot(grouped_2019_2021, aes(x = as.factor(Month), y = Sales, group = year, color = year) +
  geom_line() +
  labs(title = "Monthly Seasonality (2019 - 2021)",
       x = "Month",
       y = "Sales (Millions)") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6)) +
  scale_x_discrete(labels = month.abb) +  # Use month abbreviations on the x-axis
  theme_minimal()
# change in the trend between FEB and MAR and between MAY and JUNE

## add column DAY OF THE WEEK
my_df$day_of_week <- weekdays(my_df$Date)

# GROUP BY DAY OF THE WEEK
my_df_day <- my_df %>%
  group_by(year, day_of_week) %>%
  summarize(
    Sales = sum(Sale..Dollars.),
    Costs = sum(State.Bottle.Cost),
    Tot_Margins = sum(margin),
    Avg_Margins = mean(margin),
    Tot_Bottles = sum(Bottles.Sold),
    Avg_Bottles = mean(Bottles.Sold)
  )
# Filter for year 2023
my_df_day_2023 <- my_df_day %>% filter(year == 2023)
my_df_day_2023$day_of_week <- factor(my_df_day_2023$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
# Plot PROFIT MARGINS per DAY OF WEEK
ggplot(my_df_day_2023, aes(x = day_of_week, y = Tot_Margins)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = scales::comma_format(scale = 1e-3)(Tot_Margins)), vjust = -0.5) +
  labs(title = "2023 Profit per Day of the Week (thousands)",
       x = "Day of the Week",
       y = "Profit Margin (thousands)") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
  theme_minimal() +
  theme(panel.grid = element_blank())

# Plot SALES per DAY OF WEEK
ggplot(my_df_day_2023, aes(x = day_of_week, y = Sales)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = scales::comma_format(scale = 1e-3)(Sales)), vjust = -0.5) +
  labs(title = "2023 Sales per Day of the Week (thousands)",
       x = "Day of the Week",
       y = "Sales (thousands)") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
  theme_minimal() +
  theme(panel.grid = element_blank())

# Plot Bottles per DAY OF WEEK
ggplot(my_df_day_2023, aes(x = day_of_week, y = Tot_Bottles)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = scales::comma_format(scale = 1e-3)(Tot_Bottles)), vjust = -0.5) +
  labs(title = "2023 Tot Bottles per Day of the Week (thousands)",
       x = "Day of the Week",
       y = "Bottles (thousands)") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
  theme_minimal() + 
  theme(panel.grid = element_blank())

# Plot Costs per DAY OF WEEK
ggplot(my_df_day_2023, aes(x = day_of_week, y = Costs)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = scales::comma_format(scale = 1e-3)(Costs)), vjust = -0.5) +
  labs(title = "2023 Costs per Day of the Week (thousands)",
       x = "Day of the Week",
       y = "$ Costs (thousands)") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
  theme_minimal() +
  theme(panel.grid = element_blank())



## CATEGORIES

# Group By Category Name to see the contribution margin for each category
my_df_categories <- my_df %>%
  group_by(Category.Name) %>%
  summarise(sum_margin = sum(margin),
            sum_bottle_sold = sum(Bottles.Sold),
            sum_sales_dollars = sum(Sale..Dollars.),
            avg_cost_ml = mean(cost_ml))
summary(my_df_categories)

# ELIMINATING THE EMPTY CATEGORY NAME
my_df_categories <- filter(my_df_categories, my_df_categories$Category.Name != "")

# Plot CATEGORY with highest COST_ML
df_categories_cost_ml <- my_df_categories %>%
  arrange(desc(avg_cost_ml)) %>%
  head(10)  # Select the top 10 categories

# Bar plot for highest avg_cost_ml
ggplot(df_categories_cost_ml, aes(x = reorder(Category.Name, -avg_cost_ml), y = avg_cost_ml)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = round(avg_cost_ml, 2)), vjust = -0.5) +
  labs(title = "Top 10 Categories by Highest Cost per Ml",
       x = "Category",
       y = "Average Cost per Milliliter") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank())


# Plot CATEGORY that brings PROFIT MARGIN
# Sort in descending order and show the top 10 categories
top_categories <- my_df_categories %>%
  arrange(desc(sum_margin)) %>%
  head(10)

# Bar plot with top 10 categories by profit margins
ggplot(top_categories, aes(x = reorder(Category.Name, -sum_margin), y = sum_margin)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = scales::comma_format(scale = 1e-6)(sum_margin)), vjust = -0.5) +
  labs(title = "Top 10 Categories by Profit Margins (2019 - 2023)",
       x = "Category",
       y = "$ Profit Margins (millions)") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid = element_blank())

## INSIGHTS: I have the category that brings the highest margins, as well as the most sold ones 

any(is.na(my_df$City))
# TOP Category per City
city_category_summary <- my_df %>%
  filter(nchar(trimws(City)) > 0 & year == 2023) %>%
  group_by(City, Category.Name) %>%
  summarize(Total_Bottles = sum(Bottles.Sold),
            Profit_Margin = sum(margin))

city_category_summary <- city_category_summary %>%
  filter(Category.Name != "")

# Top 10 cities VOLUME SOLD
top_10_cities <- city_category_summary %>%
  group_by(City) %>%
  summarize(Total_Bottles = sum(Total_Bottles)) %>%
  arrange(desc(Total_Bottles)) %>%
  slice(1:10)
# Top 10 cities PROFIT GENERATED
top_10_per_profit <- city_category_summary %>%
  group_by(City) %>%
  summarize(Total_Profit = sum(Profit_Margin)) %>%
  arrange(desc(Total_Profit)) %>%
  slice(1:10)

## Filter the data for the top 10 categories in each of the top 10 cities
# Volume sold
top_categories_per_city <- city_category_summary %>%
  semi_join(top_10_cities, by = "City") %>%
  arrange(City, desc(Total_Bottles)) %>%
  group_by(City) %>%
  slice(1:10)
# Profit generated
top_profit_categories_per_city <- city_category_summary %>%
  semi_join(top_10_per_profit, by = "City") %>%
  arrange(City, desc(Profit_Margin)) %>%
  group_by(City) %>%
  slice(1:10)

# Create the heatmap for the top 10 categories in each city
# Volume sold
ggplot(top_categories_per_city, aes(x = reorder(Category.Name, -Total_Bottles), y = City, fill = Total_Bottles)) +
  geom_tile() +
  labs(title = "2023 - Top 10 Categories - Sales Across Cities",
       x = "Category",
       y = "City",
       fill = "Total Bottles Sold") +
  scale_fill_viridis_c() +  # You can use a different color scale if preferred
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# Profit generated
ggplot(top_profit_categories_per_city, aes(x = reorder(Category.Name, -Profit_Margin), y = City, fill = Profit_Margin)) +
  geom_tile() +
  labs(title = "2023 - Top 10 Categories - Profit Across Cities",
       x = "Category",
       y = "City",
       fill = "Total Profit ") +
  scale_fill_viridis_c() +  # You can use a different color scale if preferred
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Bar Pot - TOP 10 CITIES PER PROFIT 2023
ggplot(top_10_per_profit, aes(x = reorder(City, -Total_Profit), y = Total_Profit)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = scales::comma_format(scale = 1e-6)(Total_Profit)), vjust = -0.5) +
  labs(title = "Top 10 Cities by Profit Margins (2023)",
       x = "Category",
       y = "$ Profit Margins (millions)") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.3),
        panel.grid = element_blank())

# TOP 10 LEAST profitable cities
bottom_10_profit_city <- my_df %>%
  group_by(City) %>%
  summarize(Profit = sum(margin),
            Volume_sold = sum(Bottles.Sold)) %>%
  arrange(Profit) %>%
  slice(1:10)
# Bar Pot - BOTTOM 10 CITIES PER PROFIT 2023
ggplot(bottom_10_profit_city, aes(x = reorder(City, -Profit), y = Profit)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = Profit), vjust = -0.5) +
  labs(title = "Bottom 10 Cities by Profit Margins (2023)",
       x = "City",
       y = "$ Profit Margins") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.3),
        panel.grid = element_blank())
# Bar Pot - Volume sold by these least profitable cities
ggplot(bottom_10_profit_city, aes(x = reorder(City, -Profit), y = Volume_sold)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = Volume_sold), vjust = -0.5) +
  labs(title = "Volume sold by least profitable cities (2023)",
       x = "City",
       y = "Bottles sold") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.3),
        panel.grid = element_blank())

  
## STORE

# Group By Store Number to see the contribution margin for each store and other insights
my_df_stores <- my_df %>%
  filter(year == 2023) %>% 
  group_by(Store.Number) %>%
  summarise(Store.Name = first(Store.Name),
            City = first(City),
            Address = first(Address),
            sum_margin = sum(margin),
            sum_bottle_sold = sum(Bottles.Sold),
            sum_sales_dollars = sum(Sale..Dollars.),
            avg_cost_ml = mean(cost_ml),
            sum_ml_volume = sum(Bottle.Volume..ml.),
            customers = n())
summary(my_df_stores)

my_df_stores <- my_df_stores %>%
  separate(Store.Name, into = c("Store", "City_2"), sep = "/")

# Top Profitable Stores
top_10_stores <- my_df_stores %>%
  arrange(desc(sum_margin)) %>%
  head(10)

# Bar Plot - Top 10 performing stores by margins
ggplot(top_10_stores, aes(x = reorder(Store, -sum_margin), y = sum_margin, fill = City)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_text(aes(label = scales::comma_format(scale = 1e-3)(round(sum_margin, 0))), vjust = -0.5) +
  labs(title = "Top 10 Performing Stores by Margins (2023)",
       x = "Store",
       y = "$ Margins (thousands)") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank())

# Bar Plot for least performing stores
# Top Profitable Stores
bottom_10_stores <- my_df_stores %>%
  arrange(sum_margin) %>%
  head(15)
# Bar Plot - Least 10 performing stores by margins
ggplot(bottom_10_stores, aes(x = reorder(Store, -sum_margin), y = sum_margin, fill = City)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_text(aes(label = (round(sum_margin, 0))), vjust = -0.5) +
  labs(title = "Bottom 15 Performing Stores by Margins (2023)",
       x = "Store",
       y = "$ Margins") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank())




# Diageo data
diageo_data <- my_df %>%
  filter(Vendor.Name == "DIAGEO AMERICAS")

# Most profitable category
diageo_most_profitable_category <- diageo_data %>%
  group_by(Category.Name) %>%
  summarise(total_profit = sum(margin)) %>%
  arrange(desc(total_profit)) %>%
  slice(1:10)

# Bar plot
ggplot(diageo_most_profitable_category, aes(x = reorder(Category.Name, -total_profit), y = total_profit)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = scales::comma_format(scale = 1e-3)(round(total_profit, 0))), vjust = -0.5) +
  labs(title = "Most Profitable Category",
       x = "Category",
       y = "$ Total Profit (thousands)") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank())

# Monthly sales 
diageo_monthly_sales <- diageo_data %>%
  group_by(Month) %>%
  summarise(avg_total_sales = mean(`Sale..Dollars.`))

# Line chart for average monthly total sales
ggplot(diageo_monthly_sales, aes(x = factor(Month), y = avg_total_sales, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Monthly Total Sales",
       x = "Month",
       y = "$ Average Total Sales") +
  scale_x_discrete(labels = month.name) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank())


# Profitability by day of the week
diageo_profitability_by_day <- diageo_data %>%
  filter(year == 2023) %>%
  group_by(day_of_week) %>%
  summarise(total_profit = sum(margin))

# Define the order of days
day_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Convert 'day_of_week' to a factor with specified levels
diageo_profitability_by_day$day_of_week <- factor(diageo_profitability_by_day$day_of_week, levels = day_order)

# Bar plot
ggplot(diageo_profitability_by_day, aes(x = day_of_week, y = total_profit)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = scales::comma_format(scale = 1e-3)(round(total_profit, 0))), vjust = -0.5) +
  labs(title = "Profitability by Day of the Week (2023)",
       x = "Day of the Week",
       y = "$ Total Profit (thousands)") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank())

# TOT SOLD per DAY of Week
diageo_bottles_sold_by_day <- diageo_data %>%
  filter(year == 2023) %>%
  group_by(day_of_week) %>%
  summarise(total_bottles_sold = sum(Bottles.Sold))

# Convert 'day_of_week' to a factor with specified levels
diageo_bottles_sold_by_day$day_of_week <- factor(diageo_bottles_sold_by_day$day_of_week, levels = day_order)

# Bar plot
ggplot(diageo_bottles_sold_by_day, aes(x = day_of_week, y = total_bottles_sold)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = scales::comma_format(scale = 1e-3)(round(total_bottles_sold, 0))), vjust = -0.5) +
  labs(title = "Total Bottles Sold by Day of the Week (2023)",
       x = "Day of the Week",
       y = "Total Bottles Sold (thousands)") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank())

# Cost_ml per category
diageo_cost_ml_per_category <- diageo_data %>%
  group_by(Category.Name) %>%
  summarise(avg_cost_ml = mean(cost_ml, na.rm = TRUE)) %>%
  arrange(desc(avg_cost_ml))

# Print the resulting dataframe
print(diageo_cost_ml_per_category)



# Filter for the year 2023
diageo_data_2023 <- diageo_data %>%
  filter(year == 2023)

# Top 10 cities by margin in 2023
top_cities_margin_2023 <- diageo_data_2023 %>%
  group_by(City) %>%
  summarise(total_margin = sum(margin)) %>%
  arrange(desc(total_margin)) %>%
  slice(1:10)

# Bar plot for top 10 cities by margin in 2023
ggplot(top_cities_margin_2023, aes(x = reorder(City, -total_margin), y = total_margin)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = scales::comma_format(scale = 1e-3)(round(total_margin, 0))), vjust = -0.5) +
  labs(title = "Top 10 Cities by Margin in 2023 (Diageo)",
       x = "City",
       y = "$ Margin (thousands)") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank())


# Bottom 10 cities by margin in 2023
bottom_cities_margin_2023 <- diageo_data_2023 %>%
  group_by(City) %>%
  summarise(total_margin = sum(margin)) %>%
  arrange(total_margin) %>%
  slice(1:10)
# Bar plot for bottom 10 cities by margin in 2023
ggplot(bottom_cities_margin_2023, aes(x = reorder(City, -total_margin), y = total_margin)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = round(total_margin, 0)), vjust = -0.5) +
  labs(title = "Bottom 10 Cities by Margin in 2023 (Diageo)",
       x = "City",
       y = "$ Margin") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank())
