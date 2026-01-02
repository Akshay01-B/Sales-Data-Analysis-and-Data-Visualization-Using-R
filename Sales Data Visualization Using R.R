# Install required libraries
# ggplot2 → Used for data visualization (charts, graphs, plots)
install.packages("ggplot2")

# dplyr → Used for data manipulation (filtering, grouping, summarizing data)
install.packages("dplyr")

# lubridate → Used for working with dates and time easily
install.packages("lubridate")

# tidyr -is used to clean and reshape data into a tidy format
install.packages("tidyr")

# scales - helps in formatting numbers, percentages, and axes in plots
install.packages("scales")

# Load Required Libraries
library(ggplot2) #for creating charts and graphs
library(dplyr)  #for data manipulation (filter, group_by, summarise)
library(lubridate) #for handling dates and time easily
library(tidyr) #for cleaning and reshape into a tidy format
library(scales) #for formatting numbers, percentages, and axes in plots

# Load the Dataset

# read.csv() is used to read a CSV file into R
# file.choose() opens a file browser so you can manually select the CSV file
# header = TRUE tells R that the first row contains column names 
data <- read.csv(file.choose(),header = T)  
View(data) #view data preview
head(data) #view first five rows
str(data)  #check structure


# Basic Data Cleaning
# Convert Order_Date column into POSIXct date-time format
# POSIXct - for convert text/date value in proper date-time format
data$Order_Date <- as.POSIXct(data$Order_Date)


# Data Visualization

# 1. Total Sales by Category (Bar Chart)

ggplot(data, aes(x= Category, y= Sales, fill = Category)) +
  stat_summary(fun = sum, geom = "bar") +
  labs(title = "Total Sales by Category",
       x= "Category",
       y= "Sales") +
theme_minimal()

# 2. Sales Trend Over Time (Line Chart)

# Step 1: Aggregate the data
daily_sales <- data %>%
  group_by(as.Date(Order_Date)) %>%
  summarise(Total_Sales = sum(Sales))

# Step 2: Create Line Chart
ggplot(daily_sales, aes(x = `as.Date(Order_Date)`, y = Total_Sales)) +
  geom_line(color = "green") +
  labs(title = "Daily Sales Trend",
       x = "Date",
       y = "Sales") +
  theme_minimal()


# 3. Sales Distribution (Box Plot)

ggplot(data,aes(x = Category, y = Sales, fill = Category)) +
  geom_boxplot() +
  labs(title = "Sales Distribution By Category") +
  theme_minimal()

# 4. Sales By Region (Bar Chart)

ggplot(data, aes(x = Region,y = Sales, fill = Region)) +
  stat_summary(fun = sum, geom = "bar") +
  labs(title = "Total Sales By Region") + 
  theme_minimal()

# 5. Customer Type Comparion (Bar Chart)

ggplot(data, aes(x = Customer_Type, y = Sales, fill = Customer_Type)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(title = "Average Sales By Customer Type") +
  theme_minimal()

# 6. Sales By Region & Category (Heatmap)

# Step 1: Aggreagate the data 
heatmap_data <- data %>%
  group_by(Region,Category) %>%
  summarise(Total_Sales = sum(Sales))

# step 2: Create Heatmap
ggplot(heatmap_data, aes(x = Category, y = Region, fill = Total_Sales)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Heatmap: Total Sales by Region and Category",
    x = "Category",
    y = "Region",
    fill = "Sales"
  ) +
  theme_minimal()


# 7. Funnel Chart - Sales Conversion Flow
 
# Step 1: Create Funnel Data
funnel_data <- data.frame(
  Stage = c("Orders Placed",
            "Orders with Quantity > 2",
            "Orders with Sales > 2000"),
  Count = c(
    nrow(data),
    nrow(filter(data, Quantity > 2)),
    nrow(filter(data, Sales > 2000))
  )
)
 
# Step 2: Funnel Chart (Using Bar Chart Logic)
ggplot(funnel_data, aes(x = reorder(Stage, -Count), y = Count, fill = Stage)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Sales Funnel Analysis",
    x = "Stage",
    y = "Number of Orders"
  ) +
  theme_minimal()


# 8. Forest Plot – Average Sales with Confidence Intervals
 
# Step 1: Prepare Summary Statistics
forest_data <- data %>%
  group_by(Category) %>%
  summarise(
    Mean_Sales = mean(Sales),
    SD = sd(Sales),
    N = n()
  ) %>%
  mutate(
    Lower = Mean_Sales - 1.96 * (SD / sqrt(N)),
    Upper = Mean_Sales + 1.96 * (SD / sqrt(N))
  )

# Step 2: Forest Plot
ggplot(forest_data, aes(x = Mean_Sales, y = Category)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2) +
  labs(
    title = "Forest Plot: Average Sales by Category",
    x = "Average Sales (with 95% CI)",
    y = "Category"
  ) +
  theme_minimal()


