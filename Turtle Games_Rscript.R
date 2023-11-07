

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

### Project background.

## Turtle Games has a business objective of improving overall sales performance 
# by utilising customer trends. Turtle Games has commissioned KW Data Analytics 
# to conduct an in-depth analysis to understand:
   # Q1. How customers accumulate loyalty points
   # Q2. How customer groups can be used to target specific market segments
   # Q3. How online customer reviews can be used in marketing campaigns
   # Q4. The impact of products on sales
   # Q5. The reliability of the data sets
   # Q6. Any relationship(s) between North America, Europe, and global sales
  
## This R script contains the analysis for the final three questions, as the 
# sales department have indicated that they prefer analysis to be performed 
# on its data using R, rather than Python. The accompanying Jupyter Notebook 
# contains the analysis of the first three questions. 


###############################################################################

### Q4. The impact of products on sales. 

# This part of the analysis will investigate the factors impacting sales. 
# These factors could include region, product ID number, platform, and genre. 
# Turtle Games will use the insights from this analysis to drive its future 
# sales strategies. For example, which products to focus on in which regions, 
# or which products or platforms should be discontinued. 


###############################################################################
###############################################################################


# 4.1. Data ingestion. 

## Install and import the necessary packages and libraries.
### Ignore the conflict messages for tidyverse. 
install.packages("stringr")
library(tidyverse)
library(ggplot2)
library(dplyr)
library (skimr)
library(DataExplorer)
library(scales)
library(plotly)


## Import the dataset.
## The dataset has also been included in the project folder. 
sales <- read.csv('turtle_sales.csv', header=T)


## Print the DataFrame. 
View(sales)


# View the structure of the DataFrame. 
### Output shows that there are 352 observations and nine columns. 
str(sales)


## View the DataFrame as a tibble.
as_tibble(sales)


## View the descriptive statistics. 
summary(sales)
skim(sales)
#### Output shows two NA values in the Year column. 


## Confirm two NA values in the Year column. 
sum(is.na(sales$Year))


## Create a HTML report with stats and charts. 
DataExplorer::create_report(sales)


##############################################################################
##############################################################################


# 4.2 Data wrangling. 

## 4.2.1 Delete unnecessary columns. 
### Ranking is not needed for this analysis. 
sales <- subset(sales, select = -c(Ranking)) 


## 4.2.2 Change Year and Product to factor.
### R is treating these as numerical values but they are categorical. 
### Create a new DataFrame.
sales_final <- mutate(sales, Product = as.factor(Product),
                      Year = as.factor(Year))

#### Look at descriptive statistics to check if corrected.
#### Confirmed that these are no longer categorical values. 
summary(sales_final)


## 4.2.3 Change NA values to correct year.
### Based on the year that the product was launched on other platforms. 
### Identify rows with NA values. 
na_rows <- apply(sales_final, 1, function(x) any(is.na(x)))
sales_final[na_rows, ]

#### Use row and column indexing to replace the NA values. 
sales_final[48, "Year"] <- "2010"
sales_final[316, "Year"] <- "2003"

#### Check the Year column for NA values. 
### Confirmed that there are no NA values in the Year column. 
sum(is.na(sales_final$Year))


## 4.2.3 Add a new column 'Other_Sales'.
### Calculate Other_Sales from the existing sales columns. 
sales_final <- mutate(sales_final, Other_Sales = Global_Sales -
                        (NA_Sales + EU_Sales))


## 4.2.4 Reorder the columns.
sales_final <- select(sales_final, Product, Year, Genre, Platform, Publisher, 
                      NA_Sales, EU_Sales, Other_Sales, Global_Sales)


## View the DataFrame.
View(sales_final)


###############################################################################
###############################################################################


# 4.3. Initial data exploration. 

## This section involved creating visualisations with qplot and ggplot to 
## see how the data is distributed, initial trends, and any outliers.


## 4.3.1 Identification of outliers.

### This section used boxplots to see check for any outliers in the data. 
### These were created using the different sales columns. Plotly was used to 
### create interactive boxplots so any outliers could be investigated further 
### if needed. 
### Several outliers by sales were identified from the boxplots. One in 
### particular is a game for the Nintendo Wii, which had almost £68 million
### in global sales for Turtle Games, over double that of the next highest
### selling game. Research into Wii games revealed many are rare and some
### are collectors items, and can therefore be very expensive to buy 
### (Source: The Tech Wire, 2022). While some of the outliers are large, 
### they were deemed necessary for the analysis and it was decided that none
### would be removed at this stage. 


### 4.3.1.1 Identification of outliers in Global sales.  
#### Use Plotly to plot an interactive boxplot.
plot_ly(sales_final, y = ~Global_Sales, type = "box",
        hoverinfo = "text",
        text = ~paste(" Product Number: ", Product, "<br>",
                     "Platform: ", Platform, "<br>",
                      "Global Sales: £", Global_Sales, "M"),
        boxpoints = "outliers",
        marker = list(color = "black")) %>%
  layout(title = "Distribution of Global Sales of Games",
         xaxis = list(title = "", showticklabels = FALSE),
         yaxis = list(title = "Global Sales (£M)"),
         margin = list(t = 80))


### 4.3.1.2 Identification of outliers in EU sales.
#### Use Plotly to plot a interactive boxplot. 
plot_ly(sales_final, y = ~EU_Sales, type = "box",
        hoverinfo = "text",
        text = ~paste(" Product Number: ", Product, "<br>",
                      "Platform: ", Platform, "<br>",
                      "EU Sales: £", EU_Sales, "M"),
        boxpoints = "outliers",
        marker = list(color = "black")) %>%
  layout(title = "Distribution of EU Sales of Games",
         xaxis = list(title = "", showticklabels = FALSE),
         yaxis = list(title = "EU Sales (£M)"),
         margin = list(t = 80))



### 4.3.1.3 Identification of outliers in NA sales.
#### Use Plotly to plot a interactive boxplot. 
plot_ly(sales_final, y = ~NA_Sales, type = "box",
        hoverinfo = "text",
        text = ~paste(" Product Number: ", Product, "<br>",
                      "Platform: ", Platform, "<br>",
                      "NA Sales: £", NA_Sales, "M"),
        boxpoints = "outliers",
        marker = list(color = "black")) %>%
  layout(title = "Distribution of NA Sales of Games",
         xaxis = list(title = "", showticklabels = FALSE),
         yaxis = list(title = "NA Sales (£M)"),
         margin = list(t = 80))


### 4.3.1.4 Identification of outliers in Other sales.
#### Use Plotly to plot a interactive boxplot. 
plot_ly(sales_final, y = ~Other_Sales, type = "box",
        hoverinfo = "text",
        text = ~paste(" Product Number: ", Product, "<br>",
                      "Platform: ", Platform, "<br>",
                      "Other Sales: £", Other_Sales, "M"),
        boxpoints = "outliers",
        marker = list(color = "black")) %>%
  layout(title = "Distribution of Other Sales of Games",
         xaxis = list(title = "", showticklabels = FALSE),
         yaxis = list(title = "Other Sales (£M)"),
         margin = list(t = 80))


##########################

## 4.3.2 Distribution of sales.

### This section looked at the distribution of sales using histograms. 
### All sales columns were found to be heavily skewed to the right, with 
### chart peaks concentrated to the left hand side. This will be 
### investigated further in the analysis. This is due to the presence of 
### extreme outliers seen in the boxplots above. 


### 4.3.2.1 Distribution of Global sales.
qplot(Global_Sales, data = sales_final, geom = "histogram", 
      binwidth = 2.0, fill = I("#A67D3D"), col = I("black"), 
      xlab = "Global Sales (£M)", ylab = "Frequency",
      main = "Distribution of Global Sales") +
  guides(fill = FALSE)



### 4.3.2.2 Distribution of EU sales.
qplot(EU_Sales, data = sales_final, geom = "histogram", 
      binwidth = 1.0, fill = I("#0072B2"), col = I("black"), 
      xlab = "EU Sales (£M)", ylab = "Frequency",
      main = "Distribution of EU Sales") +
  guides(fill = FALSE)



### 4.3.2.3 Distribution of NA sales.
qplot(NA_Sales, data = sales_final, geom = "histogram", 
      binwidth = 1.0, fill = I("#FF800E"), col = I("black"), 
      xlab = "NA Sales (£M)", ylab = "Frequency",
      main = "Distribution of NA Sales")



### 4.3.2.4 Distribution of Other sales.
qplot(Other_Sales, data = sales_final, geom = "histogram", 
      binwidth = 1.0, fill = I("#8c92ac"), col = I("black"), 
      xlab = "Other Sales (£M)", ylab = "Frequency",
      main = "Distribution of Other Sales")


##########################


## 4.3.3 Proportion of total sales by region.

### This section shows the total share of sales by region.
### North America has the highest share at 47.2%, followed by Europe at 30.8%.
### Other regions account for 22% of sales. 

### Sum each of the regional sales columns.
### Create a new DataFrame. 
sales_final_sum <- data.frame(
  Region = c("EU_Sales", "NA_Sales", "Other_Sales"),
  Sales = c(sum(sales_final$EU_Sales), 
            sum(sales_final$NA_Sales), 
            sum(sales_final$Other_Sales)))


### Calculate the percentage of sales for each region
sales_final_sum$Percent <- sales_final_sum$Sales / 
  sum(sales_final_sum$Sales)


### Use ggplot to create a piechart. 
### Use identity statistic to plot sales values.
### Coord_polar() function transforms Cartesian coordinates into 
### polar coordinates and arranges bars in a circle. 
### Add labels to the chart. 
### Use colourblind friendly colours. 
ggplot(sales_final_sum, aes(x="", y=Sales, fill=Region)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  ggtitle("Sales by Region") +
  geom_text(aes(label = percent(Percent)), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#0072B2", "#FF800E", "#8c92ac"))


##########################


## 4.3.4 Exploration of the Platform column.

### This section used visualisations to explore the Platform column to assess
### whether platforms impacted game sales. A boxplot was created to show the 
### impact of different platforms on Global sales, and showed that some 
### platforms do have a greater impact on sales e.g. Wii. Similar 
### boxplots could have been created by region to see the impact of different
### platforms on sales, but barcharts were created instead.

### With a barchart, it also looked at the number of games sold by Turtle 
### Games by platform. For all games sold by Turtle Games, the highest number 
### are for the Xbox X360 (47 games), followed by PlayStation (41), and PC (40). 

### Using a faceted barchart, the percentage of sales by platform were
### compared across regions. For example, Wii games accounted for 17% of 
### Global sales, followed by Xbox 360 (14%), PS3 and DS (11% each). 
### Trends were similar for EU Sales and NA Sales, with Wii, Xbox 360, and PS3 
### the top three platforms. For Other Sales, DS was the top selling platform 
### for games (16% of sales), with Xbox 360 accounting for just 6% of sales.



## 4.3.4.1 Distribution of Global sales by Platform. 
## Use qplot to plot a boxplot. 
qplot(Platform, Global_Sales, data = sales_final, geom = 'boxplot', 
      fill = I('#A67D3D')) + 
  ggtitle("Distribution of Sales by Platform") +
  xlab("Platform") +
  ylab("Global Sales (£M)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))


## 4.3.4.2 Visualisation of count of games by Platform.
### Use ggplot to create a barchart that shows the count of games by Platform 
### in descending order. 
sales_final %>% 
  count(Platform) %>%  
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(Platform, -n), y = n)) + 
  geom_bar(fill = "#A67D3D", color = "black", stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  scale_y_continuous(limits = c(0, 50)) + 
  xlab("Platform") +
  ylab("Frequency") +
  ggtitle("Most Commonly Used Platforms for Games")


## 4.3.4.3 Visualisation of sales of games by Platform by region.   
### Create a DataFrame that calculates and groups percentage sales of 
### different platforms by region.
sales_perc <- sales_final %>%
  group_by(Platform) %>%
  summarise(Global_Sales = sum(Global_Sales),
            EU_Sales = sum(EU_Sales),
            NA_Sales = sum(NA_Sales),
            Other_Sales = sum(Other_Sales)) %>%
  mutate(Global_Sales = Global_Sales / sum(Global_Sales) * 100,
         EU_Sales = EU_Sales / sum(EU_Sales) * 100,
         NA_Sales = NA_Sales / sum(NA_Sales) * 100,
         Other_Sales = Other_Sales / sum(Other_Sales) * 100) %>%
  arrange(desc(Global_Sales))

### View the DataFrame. 
head(sales_perc)

### Reshape the DataFrame to long format.
### The sales columns are stacked in the value column.
### The original column names are in the variable column. 
sales_perc_long <- sales_perc %>%
  gather(key = "variable", value = "value", -Platform)

### View the DataFrame. 
head(sales_perc_long)

### Reorder by descending Global_Sales.
### Have Global_Sales on the top. 
sales_perc_long$Platform <- factor(sales_perc_long$Platform,
                                   levels = sales_perc$Platform
                                   [order(sales_perc$Global_Sales, 
                                          decreasing = TRUE)])
sales_perc_long$variable <- factor(sales_perc_long$variable,
                                   levels = c("Global_Sales", 
                                              "EU_Sales", 
                                              "NA_Sales", 
                                              "Other_Sales"))


### Create faceted barchart comparing percentage sales of each platform by
### region. 
ggplot(sales_perc_long, aes(x=Platform, y=value, fill=variable)) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = paste0(round(value, 1), "%")), 
            position = position_stack(vjust = 1.4), size=3) +
  facet_grid(variable ~ ., scales="free_y") +
  labs(x="Platform", y="Sales (%)", fill="") +
  ggtitle("Percentage Sales by Platform by Region") +
  scale_y_continuous(limits = c(0, 30)) + 
  theme(legend.position="bottom") +
  guides(fill = guide_legend(reverse=FALSE)) + 
  scale_fill_manual(values = c("#A67D3D", "#0072B2", "#FF800E", "#8c92ac"))


##########################


## 4.3.5 Exploration of the Genre column. 

### This section used visualisations to explore the Genre column to assess
### whether genre impacted game sales. A boxplot was created to show the 
### impact of different genres on Global sales, and showed that while some 
### genres appear to have a greater impact on sales, the variation between 
### genres was not that significant. Similar boxplots could have been created 
### by region to see the impact of different genres on sales, but barcharts 
### were created instead.

### With a barchart, it also looked at the number of games sold by Turtle 
### Games by genre. For all games sold by Turtle Games, the highest number 
### are shooter (71 games), followed by action (69), and sports (50). 

### Using a faceted barchart, the percentage of sales by genre were
### compared across regions. For example, shooter games accounted for 17% of 
### Global sales, followed by platform and action (15% each). 
### Trends were similar for NA Sales, while shooter, action, and sports each
### accounted for 17% of EU sales.For Other Sales, role-playing games 
### accounted for 22% of sales. 


## 4.3.5.1 Distribution of Global sales by Genre. 

## Use qplot to plot a boxplot
qplot(Genre, Global_Sales, data = sales_final, geom = 'boxplot', 
      fill = I('#A67D3D')) + ggtitle("Global Sales by Genre") +
  xlab("Genre") +
  ylab("Global Sales (£M)")


# 4.3.5.2 Visualisation of count of games by Genre.
sales_final %>% 
  count(Genre) %>%  
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(Genre, -n), y = n)) + 
  geom_bar(fill = "#0072B2", color = "black", stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  xlab("Genre") +
  ylab("Frequency") +
  ggtitle("Most Common Genres for Games") +
  scale_y_continuous(limits = c(0, 80)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## 4.3.5.3 Visualisation of sales of games by Genre by region.   
### Create a DataFrame to group percentage sales of different genres 
## by region.
sales_perc2 <- sales_final %>%
  group_by(Genre) %>%
  summarise(Global_Sales = sum(Global_Sales),
            EU_Sales = sum(EU_Sales),
            NA_Sales = sum(NA_Sales),
            Other_Sales = sum(Other_Sales)) %>%
  mutate(Global_Sales = Global_Sales / sum(Global_Sales) * 100,
         EU_Sales = EU_Sales / sum(EU_Sales) * 100,
         NA_Sales = NA_Sales / sum(NA_Sales) * 100,
         Other_Sales = Other_Sales / sum(Other_Sales) * 100) %>%
  arrange(desc(Global_Sales))


### View the DataFrame. 
head(sales_perc2)


### Reshape the DataFrame to long format.
### The sales columns are stacked in the value column.
### The original column names are in the variable column. 
sales_perc_long2 <- sales_perc2 %>%
  gather(key = "variable", value = "value", -Genre)


### View the DataFrame. 
head(sales_perc_long2)


### Reorder by descending Global_Sales.
sales_perc_long2$Genre <- factor(sales_perc_long2$Genre,
                                   levels = sales_perc2$Genre
                                   [order(sales_perc2$Global_Sales, 
                                          decreasing = TRUE)])

### Have Global_Sales on the top. 
sales_perc_long2$variable <- factor(sales_perc_long2$variable,
                                   levels = c("Global_Sales", 
                                              "EU_Sales", 
                                              "NA_Sales", 
                                              "Other_Sales"))


### Create faceted barchart comparing percentage sales of each genre by
### region. 
ggplot(sales_perc_long2, aes(x=Genre, y=value, fill=variable)) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = paste0(round(value, 1), "%")), 
            position = position_stack(vjust = 1.3), size=3) +
  facet_grid(variable ~ ., scales="free_y") +
  labs(x="Genre", y="Sales (%)", fill="") +
  ggtitle("Percentage Sales by Genre by Region") +
  scale_y_continuous(limits = c(0, 30)) + 
  theme(legend.position="bottom") +
  guides(fill = guide_legend(reverse=FALSE)) + 
  scale_fill_manual(values = c("#A67D3D", "#0072B2", "#FF800E", "#8c92ac"))


##########################


## 4.3.6 Exploration of the Publisher column. 

### This section used visualisations to explore the Publisher column to assess
### whether publishers impacted game sales. A boxplot was created to show the 
### impact of different publishers on Global sales, and showed that some 
### publishers appear to have a more impact on sales e.g. Nintendo. Similar 
### boxplots could have been created by region to see the impact of different 
### publisher on sales, but barcharts were created instead.

### With a barchart, it also looked at the number of games sold by Turtle 
### Games by publisher. For all games sold by Turtle Games, the highest number 
### are published by Nintendo (88 games), followed by Electronic Arts 
### (78 games), and Activision (52 games). 

### Using a faceted barchart, the percentage of sales by publisher were
### compared across regions. For example, Nintendo games accounted for 48% of 
### Global sales, followed by Activision (11%). Trends were similar across 
### regions, with Nintendo accounting for 57% of Other sales. 


## 4.3.6.1 Distribution of Global sales by Publisher. 
## Use qplot to plot a boxplot
qplot(Publisher, Global_Sales, data = sales_final, geom = 'boxplot', 
      fill = I('#A67D3D')) + 
  ggtitle("Global Sales by Publisher") +
  xlab("Publisher") +
  ylab("Global Sales (£M)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))



# 4.3.6.2 Visualisation of count of games by Publisher.
sales_final %>% 
  count(Publisher) %>%  
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(Publisher, -n), y = n)) + 
  geom_bar(fill = "#FF800E", color = "black", stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 2.5) +
  xlab("Publisher") +
  ylab("Frequency") +
  ggtitle("Most Common Publishers for Games") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size =7))



## 4.3.6.3 Visualisation of sales of games by Publisher by region.   
### Create a DataFrame that calculates and groups percentage sales of 
### different publishers by region.
sales_perc3 <- sales_final %>%
  group_by(Publisher) %>%
  summarise(Global_Sales = sum(Global_Sales),
            EU_Sales = sum(EU_Sales),
            NA_Sales = sum(NA_Sales),
            Other_Sales = sum(Other_Sales)) %>%
  mutate(Global_Sales = Global_Sales / sum(Global_Sales) * 100,
         EU_Sales = EU_Sales / sum(EU_Sales) * 100,
         NA_Sales = NA_Sales / sum(NA_Sales) * 100,
         Other_Sales = Other_Sales / sum(Other_Sales) * 100) %>%
  arrange(desc(Global_Sales))


### View the DataFrame. 
head(sales_perc3)


### Reshape the DataFrame to long format.
### The sales columns are stacked in the value column.
### The original column names are in the variable column. 
sales_perc_long3 <- sales_perc3 %>%
  gather(key = "variable", value = "value", -Publisher)

### View the DataFrame. 
head(sales_perc_long3)


### Reorder by descending Global_Sales.
sales_perc_long3$Publisher <- factor(sales_perc_long3$Publisher,
                                 levels = sales_perc3$Publisher
                                 [order(sales_perc3$Global_Sales, 
                                        decreasing = TRUE)])

### Have Global_Sales on the top. 
sales_perc_long3$variable <- factor(sales_perc_long3$variable,
                                    levels = c("Global_Sales", 
                                               "EU_Sales", 
                                               "NA_Sales", 
                                               "Other_Sales"))


### Create faceted barchart comparing percentage sales of each publisher
### by region. 
ggplot(sales_perc_long3, aes(x=Publisher, y=value, fill=variable)) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = paste0(round(value, 1), "%")), 
            position = position_stack(vjust = 1.3), size=3) +
  facet_grid(variable ~ ., scales="free_y") +
  labs(x="Publisher", y="Sales (%)", fill="") +
  ggtitle("Percentage Sales by Publisher by Region") +
  scale_y_continuous(limits = c(0, 75)) + 
  theme(legend.position="bottom") +
  guides(fill = guide_legend(reverse=FALSE)) + 
  scale_fill_manual(values = c("#A67D3D", "#0072B2", "#FF800E", "#8c92ac")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size =6))

##########################


## 4.3.7 Exploration of the Year column. 

### This section used visualisations to explore the Year column to assess
### whether game launch year impacted game sales. A boxplot was created to 
### show the impact of different years on Global sales, and showed there was 
### some variation in the impact of different launch years. Games launched 
### in 2005 and 2006 had a large impact, as well as games from 1984 and 1989. 
### This is probably driven by a trend in retro gaming. Similar boxplots
### could have been created by region to see the impact of different years
### on sales, but barcharts were created instead.

### With a barchart, it also looked at the number of games sold by Turtle 
### Games by year. For all games sold by Turtle Games, the highest number 
### were launched in 2010 (33 games), 2011 (30), and 2014 (30).

### Using a faceted barchart, the percentage of sales by year were
### compared across regions. For example, games launched in 2006 accounted 
### for almost 9% of Global sales, followed by 2009 (8%) and 2020 (8%). Trends 
### were similar across regions, with a slightly higher proportion of NA sales
### for products launched in the 1980's compared to other regions. 



## 4.3.7.1 Distribution of Global sales by Year. 
## Use qplot to plot a boxplot
qplot(Year, Global_Sales, data = sales_final, geom = 'boxplot', 
      fill = I('#A67D3D')) + 
  ggtitle("Global Sales by Game Lauch Year") +
  xlab("Year") +
  ylab("Global Sales (£M)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))



# 4.3.7.2 Visualisation of count of games by Year.
sales_final %>% 
  count(Year) %>%  
  ggplot(aes(x = Year, y = n)) + 
  geom_bar(fill = "#A67D3D", color = "black", stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  xlab("Year") +
  ylab("Frequency") +
  ggtitle("Games by Launch Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## 4.3.7.3 Visualisation of sales of games by Year by region.   
### Create a DataFrame that calculates and groups percentage sales of 
### different years by region.
sales_perc4 <- sales_final %>%
  group_by(Year) %>%
  summarise(Global_Sales = sum(Global_Sales),
            EU_Sales = sum(EU_Sales),
            NA_Sales = sum(NA_Sales),
            Other_Sales = sum(Other_Sales)) %>%
  mutate(Global_Sales = Global_Sales / sum(Global_Sales) * 100,
         EU_Sales = EU_Sales / sum(EU_Sales) * 100,
         NA_Sales = NA_Sales / sum(NA_Sales) * 100,
         Other_Sales = Other_Sales / sum(Other_Sales) * 100) %>%
  arrange(desc(Global_Sales))


### View the DataFrame. 
head(sales_perc4)


### Reshape the DataFrame to long format.
### The sales columns are stacked in the value column.
### The original column names are in the variable column. 
sales_perc_long4 <- sales_perc4 %>%
  gather(key = "variable", value = "value", -Year)

### View the DataFrame. 
head(sales_perc_long4)


### Reorder by descending Global_Sales.
### Have Global_Sales on the top. 
sales_perc_long4$variable <- factor(sales_perc_long4$variable,
                                    levels = c("Global_Sales", 
                                               "EU_Sales", 
                                               "NA_Sales", 
                                               "Other_Sales"))


### Create faceted barchart comparing percentage sales of each year by
### region. 
ggplot(sales_perc_long4, aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = paste0(round(value, 1), "%")), 
            position = position_stack(vjust = 1.3), size=3) +
  facet_grid(variable ~ ., scales="free_y") +
  labs(x="Year", y="Sales (%)", fill="") +
  ggtitle("Percentage Sales by Year by Region") +
  scale_y_continuous(limits = c(0, 15)) + 
  theme(legend.position="bottom") +
  guides(fill = guide_legend(reverse=FALSE)) + 
  scale_fill_manual(values = c("#A67D3D", "#0072B2", "#FF800E", "#8c92ac")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size =6))


##########################


## 4.3.8 Investigation into the impact of Product ID on sales.

### This section looked at the impact of each product on sales. The sales team
### at Turtle Games want to understand what the top selling products are 
### globally and regionally in order to focus its global and regional
### marketing campaigns. It also wants to streamline its offerings by region, 
### so also asked to see which products generate 0.2% or less of sales by
### region. 

### Visualisation by boxplot would have been too difficult as there are 175 
### unique products. Bar charts were also used but needed to be filtered to 
### top 25 and lowest selling products in order to be seen clearly. 

### For top selling products, 107 and 515 were the top two globally, and 
### these also appeared in the top five products for NA, EU, and Other sales. 
### There were regional differences in the rest of the top products. The sales
### team can use this information to tailor its marketing initiatives to 
### customers is different regions.

### For the lowest selling products there were regional differences in the 
### number of products with 0.2% or less of sales. There were 42 products in 
### EU Sales, 30 in NA Sales, and 41 in Other Sales. The sales teams can use 
### this information to reduce certain product offerings to specific regions
### in order to streamline its offerings in order to focus on better selling 
### products.

##########################


### 4.3.8.1 Top 50 Products by Global Sales by region.

#### This section involves identifying the top 50 products by Global sales 
#### and seeing the split of sales for each of these products by region. 

# Aggregate each sales column by sum of sales for each unique Product. 
# Create new DataFrame. 
sales_agg <- aggregate(cbind(NA_Sales, EU_Sales, Other_Sales, Global_Sales) 
                       ~ Product, sales_final, sum)


# View the DataFrame. 
head(sales_agg)


# Filter to the top 50 products by Global sales. 
top_sales <- head(sales_agg[order(-sales_agg$Global_Sales),], 50)


# View the DataFrame. 
str(top_sales)


# Create new DataFrame.
# Create three vector columns: Sales, Region, and Product. 
# Each row represents a combination of a product and a region
# With the sales data for that combination in the Sales column. 
sales_by_region <- data.frame(
  Sales = c(top_sales$EU_Sales, top_sales$NA_Sales, top_sales$Other_Sales),
  Region = rep(c("EU", "NA", "Other"), each = 50),
  Product = rep(top_sales$Product, times = 3))

# View the DataFrame. 
head(sales_by_region)


# Calculate total sales for each product
sales_by_product <- sales_by_region %>%
  group_by(Product) %>%
  summarize(Total_Sales = sum(Sales)) %>%
  arrange(desc(Total_Sales))


# View the DataFrame. 
head(sales_by_product)


# Reorder the levels of the Product factor based on total sales. 
sales_by_region$Product <- factor(sales_by_region$Product, 
                                  levels = sales_by_product$Product)

# Create a ggplot object. 
p <- ggplot(sales_by_region, aes(x = Product, y = Sales, fill = Region)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Global Sales by Region for Top 50 Products", 
       x = "Product", 
       y = "Global Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10)) +
  guides(fill=guide_legend(title="Region", nrow = 1)) +
  scale_fill_manual(values=c("#0072B2", "#FF800E", "#8c92ac"))

# Create an interactive version of the plot using plotly. 
ggplotly(p)


### 4.3.8.2 Impact of Product ID on EU sales. 

#### Create a new DataFrame with the sum of EU sales by product. 
#### Sort by descending EU sales.
sum_sales_prodEU <- sales_final %>% 
  group_by(Product) %>% 
  summarise(EU_Sales = sum(EU_Sales)) %>% 
  arrange(desc(EU_Sales)) 


### View the DataFrame. 
View(sum_sales_prodEU)


### Use ggplot to plot a bar chart. 
### Order the sales in descending order.
### Output is too hard to read as there are too many products.
ggplot(data = sum_sales_prodEU, 
       aes(x = reorder(Product, -EU_Sales), y = EU_Sales)) + 
  geom_col(fill = "#0072B2") +
  labs(title = "Sum of EU Sales by Product (£M)",
       x = "Product",
       y = "EU Sales (£M)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### 4.3.8.2.1 See top 25 selling products by EU sales.

### Calculate the percentage of EU sales for each product.
sum_sales_prodEU <- sum_sales_prodEU %>% 
  mutate(Per_SalesEU = EU_Sales/sum(EU_Sales)*100)


### View the DataFrame. 
View(sum_sales_prodEU)


#### Use slice function on the first 25 rows.
#### Add % data labels to each bar using geom_text.
ggplot(data = sum_sales_prodEU %>% slice(1:25), 
       aes(x = reorder(Product, -EU_Sales), 
           y = EU_Sales)) + 
  geom_col(fill = "#0072B2") +
  geom_text(aes(label = paste0(round(Per_SalesEU, 1), "%"), 
                y = EU_Sales), vjust = -0.5, size = 2.5) +
  labs(title = "Top 25 Products by % EU Sales (£M)",
       x = "Product",
       y = "EU Sales (£M)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))


### 4.3.8.2.2 See lowest selling products by EU sales. 
#### Filter the DataFrame to products with sales of 0.2%.
#### Add % data labels to each bar using geom_text. 
sum_sales_prod_filteredEU <- sum_sales_prodEU %>% 
  filter(Per_SalesEU < 0.24887230)


### View the DataFrame. 
### There are 42 products with 0.2% or less of EU sales. 
head(sum_sales_prod_filteredEU)
str(sum_sales_prod_filteredEU)
View(sum_sales_prod_filteredEU)


### Use ggplot to plot a bar chart. 
#### Use the filtered data. 
ggplot(data = sum_sales_prod_filteredEU, 
       aes(x = reorder(Product, -EU_Sales), 
           y = EU_Sales)) + 
  geom_col(fill = "#0072B2") +
  geom_text(aes(label = paste0(round(Per_SalesEU, 1), "%"), 
                y = EU_Sales), vjust = -0.5, size = 2.5) +
  scale_y_continuous(limits = c(0, 23)) +
  labs(title = "Lowest Selling Products by % EU Sales",
       x = "Product",
       y = "EU Sales (£M)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))



### 4.3.8.3 Impact of Product ID on NA sales. 
#### Create a new DataFrame with the sum of NA sales by product. 
#### Sort by descending NA sales.
sum_sales_prodNA <- sales_final %>% 
  group_by(Product) %>% 
  summarise(NA_Sales = sum(NA_Sales)) %>% 
  arrange(desc(NA_Sales)) 


### View the DataFrame. 
View(sum_sales_prodNA)


### Use ggplot to plot a bar chart. 
### Order the sales in descending order.
### Output is too hard to read as there are too many products.
ggplot(data = sum_sales_prodNA, 
       aes(x = reorder(Product, -NA_Sales), y = NA_Sales)) + 
  geom_col(fill = "#FF800E") +
  labs(title = "Sum of NA Sales by Product (£M)",
       x = "Product",
       y = "NA Sales (£M)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### 4.3.8.3.1 See the top 25 products by % NA sales.
###~ Calculate the percentage of NA sales for each product.
sum_sales_prodNA <- sum_sales_prodNA %>% 
  mutate(Per_SalesNA = NA_Sales/sum(NA_Sales)*100)


### View the DataFrame. 
View(sum_sales_prodNA)


#### Use slice function on the first 25 rows.
#### Add % data labels to each bar using geom_text. 
ggplot(data = sum_sales_prodNA %>% slice(1:25), 
       aes(x = reorder(Product, -NA_Sales), 
           y = NA_Sales)) + 
  geom_col(fill = "#FF800E") +
  geom_text(aes(label = paste0(round(Per_SalesNA, 1), "%"), 
                y = NA_Sales), vjust = -0.5, size = 2.5) +
  labs(title = "Top 25 Products by % NA Sales (£M)",
       x = "Product",
       y = "NA Sales (£M)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))


### 4.3.8.3.2 See lowest selling products by % NA sales.
#### Filter the DataFrame to products with sales of 0.2%. 
#### Add % data labels to each bar using geom_text. 
sum_sales_prod_filteredNA <- sum_sales_prodNA %>% 
  filter(Per_SalesNA < 0.246155236)


### View the DataFrame. 
### There are 30 products with 0.2% or less of NA sales. 
head(sum_sales_prod_filteredNA)
str(sum_sales_prod_filteredNA)
View(sum_sales_prod_filteredNA)


### Use ggplot to plot a bar chart. 
#### Use the filtered data. 
ggplot(data = sum_sales_prod_filteredNA, 
       aes(x = reorder(Product, -NA_Sales), 
           y = NA_Sales)) + 
  geom_col(fill = "#FF800E") +
  geom_text(aes(label = paste0(round(Per_SalesNA, 1), "%"), 
                y = NA_Sales), vjust = -0.5, size = 2.5) +
  scale_y_continuous(limits = c(0, 35)) +
  labs(title = "Lowest Selling Products by % NA Sales",
       x = "Product",
       y = "NA Sales (£M)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))



### 4.3.8.4 Impact of Product on Other sales. 
#### Create a new DataFrame with the sum of Other sales by product. 
#### Sort by descending Other sales.
sum_sales_prodOT <- sales_final %>% 
  group_by(Product) %>% 
  summarise(Other_Sales = sum(Other_Sales)) %>% 
  arrange(desc(Other_Sales)) 


### View the DataFrame. 
View(sum_sales_prodOT)


### Use ggplot to plot a bar chart. 
### Order the sales in descending order.
### Output is too hard to read as there are too many products.
ggplot(data = sum_sales_prodOT, 
       aes(x = reorder(Product, -Other_Sales), y = Other_Sales)) + 
  geom_col(fill = "#8c92ac") +
  labs(title = "Sum of Other Sales by Product (£M)",
       x = "Product",
       y = "Other Sales (£M)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### 4.3.8.4.1 See the top 25 products by % Other sales.
### Calculate the percentage of Other sales for each product. 
sum_sales_prodOT <- sum_sales_prodOT %>% 
  mutate(Per_SalesOT = Other_Sales/sum(Other_Sales)*100)


### View the DataFrame. 
View(sum_sales_prodOT)


##### Use slice function on the first 25 rows.
##### Add % data labels to each bar using geom_text. 
ggplot(data = sum_sales_prodOT %>% slice(1:25), 
       aes(x = reorder(Product, -Other_Sales), 
           y = Other_Sales)) + 
  geom_col(fill = "#8c92ac") +
  geom_text(aes(label = paste0(round(Per_SalesOT, 1), "%"), 
                y = Other_Sales), vjust = -0.5, size = 2.5) +
  labs(title = "Top 25 Products by % Other Sales (£M)",
       x = "Product",
       y = "Other Sales (£M)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))


### 4.3.8.4.2 See lowest selling products by % Other sales.
#### Filter the DataFrame to products with sales of 0.2%. 
#### Add % data labels to each bar using geom_text. 
sum_sales_prod_filteredOT <- sum_sales_prodOT %>% 
  filter(Per_SalesOT < 2.490449e-01)


### View the DataFrame. 
### There are 41 products with 0.2% or less of Other sales. 
head(sum_sales_prod_filteredOT)
str(sum_sales_prod_filteredOT)
View(sum_sales_prod_filteredOT)


ggplot(data = sum_sales_prod_filteredOT, 
       aes(x = reorder(Product, -Other_Sales),
           y = Other_Sales))  +
  geom_col(fill = "#8c92ac") +
  geom_text(aes(label = paste0(round(Per_SalesOT, 1), "%"), 
                y = Other_Sales), vjust = -0.5, size = 2.5) +
  scale_y_continuous(limits = c(0, 10.5)) +
  labs(title = "Lowest Selling Products by % Other Sales", 
       x = "Product",
       y = "Other Sales (£M)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))


##########################


## 4.3.9 Relationship between regional sales and Global sales.

### This section looked at the relationship between EU sales and NA sales
### with Global sales. The resulting scatterplots show that NA sales appear
### to have a marginally stronger linear relationship with Global sales, 
### which will be investigated further in the analysis.
### Platform was also added to the scatterplots as a third variable. These 
### were not easy to assess as there were too many data points. The faceted 
### barcharts created earlier provide a better breakdown of platform sales by
### region. 


### 4.3.9.1 Relationship between EU sales and Global sales.
#### Use qplot to create a scatterplot.
qplot(EU_Sales, Global_Sales, color = I("#0072B2"), data = sales_final) +
  ggtitle("Global Sales vs. EU Sales") +
  xlab("EU Sales (£M)") +
  ylab("Global Sales (£M)")



### 4.3.9.2 Relationship between EU sales and Global sales by platform.
#### Use qplot to plot a scatterplot.
#### Add Platform as a third variables using 'color'. 
qplot(EU_Sales, Global_Sales, color = Platform, data = sales_final) +
  ggtitle("Global Sales vs. EU Sales by Platform") +
  xlab("EU Sales (£M)") +
  ylab("Global Sales (£M)")



### 4.3.9.3 Relationship between NA sales and Global sales. 
#### Use qplot to plot a scatterplot.
#### Change the colour to orange and add titles and axis labels. 
qplot(NA_Sales, Global_Sales, color = I("#FF800E"), data = sales_final) +
  ggtitle("Global Sales vs. NA Sales") +
  xlab("NA Sales (£M)") +
  ylab("Global Sales (£M)")



### 4.3.9.4 Relationship between NA sales and Global sales by platform.
#### Use qplot to plot a scatterplot.
#### Add Platform as a third variables using 'color'. 
qplot(NA_Sales, Global_Sales, color = Platform, data = sales_final) +
  ggtitle("Global Sales vs. NA Sales by Platform") +
  xlab("NA Sales (£M)") +
  ylab("Global Sales (£M)")



### 4.3.9.5 Relationship between Other sales and Global sales.
#### Use qplot to plot a scatterplot.
#### Change the colour to grey and add titles and axis labels.
qplot(Other_Sales, Global_Sales, color = I("#8c92ac"), data = sales_final) +
  ggtitle("Global Sales vs. Other Sales") +
  xlab("Other Sales (£M)") +
  ylab("Global Sales (£M)")



### 4.3.9.6 Relationship between Other sales and Global sales by platform.
#### Use qplot to plot a scatterplot.
#### Add Platform as a third variables using 'color'. 
a = qplot(Other_Sales, Global_Sales, color = Platform, data = sales_final) +
  ggtitle("Global Sales vs. Other Sales by Platform") +
  xlab("Other Sales (£M)") +
  ylab("Global Sales (£M)")

###############################################################################
###############################################################################

# Q4. Observations and insights
## Initial exploration of the data with visualisations assessed how the data 
## was distributed and identified trends as well as any outliers. 
## Outliers were seen for the data using boxplots; however these were deemed 
## suitable for inclusion in the analysis and were not removed. Histograms  
## were used to see the distribution of the sales columns and all were found 
## to be skewed heavily to the right. This will be investigated further in 
## the analysis. 
## Initial trends saw that North America had the highest proportion of 
##total sales (47%), with Europe at 31%. 
## Several columns, such as Platform, Genre, Publisher, and Year were 
## investigated for their impact on sales using boxplots. There was more
## variation in the boxes between platforms, publishers, and years than 
## for genre, indicating that genre did not appear to impact sales as much
## as the other variables. 
## Faceted barcharts were used to compare the percentage of global sales
## to regional sales by platform, genre,  publisher, and year. For example
## looking at platform, Wii games accounted for 17% of Global sales, 
## followed by Xbox 360 (14%), PS3 and DS (11% each). Trends were similar
## for EU Sales and NA Sales, but for Other Sales, DS was the top selling 
## platform. This will help the sales team have a better understanding of
## product sales trends in relation to these variables.
## The impact of each product on sales was also investigated. The top 50 
## products by global were found, as well the top products by region. Regional 
## differences in the top selling products were seen, which should help the 
## sales team with its sales strategies. Products with less than 2% of 
## sales were also identified for each region which will help the company plan
## any activities to streamline product offerings by region.
## The relationship between EU sales and NA sales with Global sales was 
## also evaluated using scatterplots. NA sales appear to have a slightly 
## stronger linear relationship with Global sales, which will be investigated 
## later in the analysis.

###############################################################################
###############################################################################


# Q5. Determine the reliability of the data sets. 

### This section of the analysis will assess the data for normality prior to
### conducting linear regression analysis. As linear regression assumes that 
### the residuals are normally distributed, regression coefficients may be 
### biased or inefficient, and the standard errors and confidence intervals 
### may be incorrect if the residuals are not normally distributed. 

# Install and import packages and libraries.
install.packages("corrplot")
library(corrplot)
library(moments)

##  5.1 Determine the normality of Global Sales. 

### 5.1.1 Create Q-Q Plot
#### Add a reference line of expected values for normal distribution. 
qqnorm(sales_final$Global_Sales, col = "#A67D3D", 
       main = "QQ Plot of Global Sales")
qqline(sales_final$Global_Sales, col = "black")
xlab("Theoretical Quantiles")
ylab("Sample Quantiles")

# Resulting plot shows it deviates from normality on the right side. 


### 5.1.2 Perform Shapiro-Wilk test. 
shapiro.test(sales_final$Global_Sales)
# p-value < 2.2e-16 (is not > 0.05)
# Therefore reject null hypothesis. 
# Data is not deemed normal. 


### 5.1.3 Determine Skewness. 
skewness(sales_final$Global_Sales)
# 4.045582
# 0 is perfect symmetrical distribution.
# This result indicates a significant right skew.
# This is in line with the Q-Q Plot. 


### 5.1.4 Determine Kurtosis. 
kurtosis(sales_final$Global_Sales)
# 32.63966
# Normal distribution has a kurtosis of 0.
# This output indicated that the data is highly leptokurtic. 
# This is in line with the Q-Q Plot.  


##  5.2 Determine the normality of EU Sales 

### 5.2.1 Create Q-Q Plot.
### Add a reference line of expected values for normal distribution.
qqnorm(sales_final$EU_Sales, col = "#0072B2", 
       main = "QQ Plot of EU Sales")
qqline(sales_final$EU_Sales, col = "black")
xlab("Theoretical Quantiles")
ylab("Sample Quantiles")


### 5.2.2 Perform Shapiro-Wilk test. 
shapiro.test(sales_final$EU_Sales)
# p-value < 2.2e-16 (is not > 0.05)
# Therefore reject null hypothesis. 
# Data is not deemed normal.


### 5.2.3 Determine Skewness. 
skewness(sales_final$EU_Sales)
# 4.818688
# This result indicates a significant right skew.
# This is in line with the Q-Q Plot. 


### 5.2.4 Determine Kurtosis. 
kurtosis(sales_final$EU_Sales)
# 44.68924
# This output indicated that the data is highly leptokurtic. 
# This is in line with the Q-Q Plot.


## 5.3 Determine the normality of NA Sales 

### 5.3.1 Create Q-Q Plot. 
### Add a reference line of expected values for normal distribution.
qqnorm(sales_final$NA_Sales, col = "#FF800E", 
       main = "QQ Plot of NA Sales")
qqline(sales_final$NA_Sales, col = "black")
xlab("Theoretical Quantiles")
ylab("Sample Quantiles")


### 5.3.2 Perform Shapiro-Wilk test. 
shapiro.test(sales_final$NA_Sales)
# p-value < 2.2e-16 (is not > 0.05)
# Therefore reject null hypothesis. 
# Data is not deemed normal.


### 5.3.3 Determine Skewness.
skewness(sales_final$NA_Sales)
# 4.30921
# This result indicates a significant right skew.
# This is in line with the Q-Q Plot,


### 5.3.4 Determine Kurtosis.
kurtosis(sales_final$NA_Sales)
# 31.36852
# This output indicated that the data is highly leptokurtic. 
# This is in line with the Q-Q Plot.


## 5.4 Determine the normality of Other Sales 

### 5.4.1 Create Q-Q Plot. 
### Add a reference line of expected values for normal distribution.
qqnorm(sales_final$Other_Sales, col = "#8c92ac", 
       main = "QQ Plot of Other Sales")
qqline(sales_final$NA_Sales, col = "black")
xlab("Theoretical Quantiles")
ylab("Sample Quantiles")


### 5.4.2 Perform Shapiro-Wilk test
shapiro.test(sales_final$Other_Sales)
# p-value < 2.2e-16 (is not > 0.05)
# Therefore reject null hypothesis. 
# Data is not deemed normal.


### 5.4.3 Determine Skewness. 
skewness(sales_final$Other_Sales)
# 2.46905
# This result indicates a right skew.
# This is in line with the Q-Q Plot,


### 5.4.4 Determine Kurtosis. 
kurtosis(sales_final$Other_Sales)
# 10.4401
# This output indicated that the data is leptokurtic. 
# This is in line with the Q-Q Plot.


##  5.5 Determine correlation between the sales columns.  

### 5.5.1.Correlation between Global sales and EU sales.  
cor(sales_final$Global_Sales, sales_final$EU_Sales)
# Output = 0.8775575
# Shows a strong positive correlation. 


### 5.5.2 Correlation between Global sales and NA sales.
cor(sales_final$Global_Sales, sales_final$NA_Sales)
# Output = 0.9349455
# Shows a very strong positive correlation.


### 5.5.3 Correlation between Global sales and Other sales.
cor(sales_final$Global_Sales, sales_final$Other_Sales)
# Output = 0.8206484
# Shows a strong positive correlation.


### Visualise the correlation between the columns. 
### Create new DataFrame with just numerical values. 
### Plot the correlation. 
sales_num <- select(sales_final, -Platform, -Product, -Year, 
                    -Genre, -Publisher, -Other_Sales)

## Chagne DataFrame to matrix. 
sales_matrix <- cor(sales_num[, sapply(sales_num, is.numeric)])

## Create a correlation plot. 
corrplot(sales_matrix, method = "color", type = "upper", tl.col = "black")
## Output confirms high correlation between global sales and the regional
## sales columns.

###############################################################################

# Q5. Observations and insights. 
## Results from the Shapiro-Wilk tests indicate that the distribution 
## of all the sales columns is significantly non-normal (p-value < 2.2e-16). 
## Moreover, the skewness and kurtosis values also indicate that the 
## distributions are heavily skewed and have high kurtosis, respectively. 
## The skewness can be seen in the Q-Q plots and in histograms that were 
## created earlier to show the distribution of the different sales columns 
## (see section ## 4.3.2). 
## It would be generally recommended to normalise this data before performing 
## multiple linear regression, but not simple linear regression as it only 
## involves one predictor variable. However, as the different sales columns
## have similar scales, and there is strong positive linear correlation 
## between the dependent variable (Global Sales) and the independent variables 
## (EU Sales, Global Sales, and Other Sales), it was decided that data 
## normalisation will not be carried out for this analysis. 

###############################################################################
###############################################################################

# Q6. Assess relationship between Global sales and regional sales. 

## The section will use linear regression models to understand relationships 
## between regional sales (EU Sales and NA Sales) and Global sales, in order
## to support predicting global sales. Other Sales will not be assessed at 
## this time.
## The models will be compared for accuracy using metrics such as R-squared,
## Mean Squared Error (MSE), Root Mean Squared Error (RMSE), and Mean Absolute 
## Error (MAE) values. MSE, RMSE, and MAE are the most common metric for 
## evaluating the performance of regression models (Source: Machine Learning 
## Mastery (2021), Medium (2021)). 

# Install and import necessary packages and libraries.
install.packages('psych')
install.packages("Metrics")
library(psych)
library(Metrics)
library(lmtest)
library(car)

# 6.1 Simple linear regression.

## This section used simple linear regression to see the relationship between 
## EU sales and global sales, and the relationship between NA sales and 
## global sales. The resulting models could support Turtle Games in predicting 
## global sales based on regional sales. 

## 6.1.1. Impact of EU Sales on Global Sales (Model 1). 

## Plot EU Sales vs Global Sales using a scatterplot. 
plot(sales_final$EU_Sales, sales_final$Global_Sales, col = "#0072B2", 
     xlab = "EU Sales", ylab = "Global Sales", 
     main = "Global Sales vs EU Sales")


## Create a linear model using the lm function.
### Global_Sales is the response variable.
### EU_Sales is the predictor variable
model1 <- lm(Global_Sales ~ EU_Sales, data = sales_final)


## View Model 1. 
model1
## The slope coefficient for EU_Sales is estimated to be 2.71399. 
## Indicates that for every unit increase in EU Sales, Global Sales
## will increase by 2.71399 units. 


## See summary of Model 1 parameters. 
summary(model1)
## The coefficient of EU_Sales is statistically significant (p-value < 2e-16) 
## indicating EU sales is a statistically significant predictor of global 
## sales. 
## R-squared = 0.77 (could be improved). 
## Standard error is relatively small (0.07926) compared to the magniture of 
## the coefficient (2.71399).
## Residual standard error is 3.008 - relatively low compared to the magnitude 
## of the response variable.
## Overall model could be a good fit. 


## Plot the residuals of Model 1. 
plot(model1$residuals)
## Output shows no pattern; suggests the model is a good fit. 


## See the estimated intercept and slope coefficients of Model 1. 
coefficients(model1)


## Add a fitted regression line to a scatterplot. 
abline(coefficients(model1))

## Check the accuracy of the model.
## The R squared value for this model is average (77%). The MSE, RMSE, and MAE
## for the model were 8.99, 2.99 and 1.73 respectively, which are generally 
## acceptable in the context of predicting sales data, but could be improved. 
## In addition, the model did not show heteroscedasticity. 


## Get the residuals from Model 1.
resid1 <- residuals(model1)


## Calculate the predicted values
predict1 <- predict(model1)


## Calculate R-squared.
rsq <- summary(model1)$r.squared
cat("R-squared:", rsq, "\n")
# R-squared: 0.77


# Calculate MSE.
n <- length(resid1)
mse <- (1/n) * sum(resid1^2)
cat("MSE:", mse, "\n")
# MSE = 8.99


# Calculate RMSE.
rmse <- sqrt(mean(resid1^2))
cat("RMSE:", rmse, "\n")
# RMSE = 2.99


# Calculate MAE.
mae <- mae(predict1, sales_final$Global_Sales)
cat("MAE:", mae, "\n")
# MAE: 1.73


## Check for heteroscedasticity. 

# Perform the Breusch-Pagan test.
bp_test <- bptest(model1)


# View the p-value of the test. 
bp_test$p.value
## p = 0.1847696 (p > 0.05)
# Fail to reject the null hypothesis.
# There is no heteroscedasticity


################################


## 6.1.2 Impact of NA Sales on Global Sales (Model 2). 

### Plot NA Sales vs Global Sales using a scatterplot. 
plot(sales_final$NA_Sales, sales_final$Global_Sales, col = "#FF800E", 
     xlab = "NA Sales", ylab = "Global Sales", 
     main = "Global Sales vs NA Sales")


### Create a linear model using the lm function.
## Global_Sales is the response variable.
## NA_Sales is the predictor variable
model2 <- lm(Global_Sales ~ NA_Sales, data = sales_final)


### View Model 2.
model2
## The slope coefficient for NA_Sales is estimated to be 1.71797. 
## Indicates that for every unit increase in NA Sales, Global Sales
## will increase by 1.71797 units.


### See summary of Model 2 parameters. 
summary(model2)
## The coefficient of NA sales is statistically significant (p-value < 2e-16) 
## indicating NA sales is a statistically significant predictor of global 
## sales.
## R-squared = 0.87 (good).  
## Standard error is relatively small (0.03485) compared to the magnitude of 
## the coefficient (1.71797).
## Residual standard error is 2.226 - relatively low compared to the magnitude 
## of the response variable.
## Overall model could be a good fit. 


### Plot the residuals of Model 2.
plot(model2$residuals)
## Output shows no pattern; suggests the model is a good fit. 


### See the estimated intercept and slope coefficients of Model 2.
coefficients(model2)


### Add a fitted regression line to a scatterplot.
abline(coefficients(model2))


### Check the accuracy of Model 2.
## The R squared value for this model is good (87%). The MSE, RMSE, and MAE
## for the model were 4.93, 2.23 and 1.45 respectively, which are generally 
## acceptable in the context of predicting sales data, and are better than 
## the values for Model 1. However, the model did show heteroscedasticity.

## Get the residuals from Model 2.
resid2 <- residuals(model2)


## Calculate the predicted values
predict2 <- predict(model2)


## Calculate R-squared.
rsq <- summary(model2)$r.squared
cat("R-squared:", rsq, "\n")
# R-squared: 0.87


# Calculate MSE.
n <- length(resid2)
mse <- (1/n) * sum(resid2^2)
cat("MSE:", mse, "\n")
# MSE = 4.93


# Calculate RMSE.
rmse <- sqrt(mean(resid2^2))
cat("RMSE:", rmse, "\n")
# RMSE = 2.23


# Calculate MAE.
mae <- mae(predict2, sales_final$Global_Sales)
cat("MAE:", mae, "\n")
# MAE: 1.45


## Check for homoscedasticity.

# Perform the Breusch-Pagan test
bp_test <- bptest(model2)


# View the p-value of the test. 
bp_test$p.value
## p = 2.009067e-326 (p < 0.05)
# Reject the null hypothesis.
# There is heteroscedasticity

#############################################################################

# 6.2 Multiple linear regression models.

## In this part of the analysis, four different multiple linear regression 
## models were created and compared. The different models involved various 
## permutations, including removing outliers or grouping the sales by sum of 
## unique Product sales.Turtle Sales were concerned that the outliers in the 
## sales data may impact the accuracy of models so asked for models to be
## generated without outliers. 
## The model with the best fit will be used to predict global sales by EU and 
## NA sales. 

################################

## 6.2.1 Multiple linear regression (Model 3) - original DataFrame. 

## Select only numeric columns from the original DataFrame.
sales_num <- select(sales_final, -Platform, -Product, -Year, 
                    -Genre, -Publisher, -Other_Sales)


## View the DataFrame. 
head(sales_num)


## Create the multiple linear regression model.
### Global_Sales is the response variable.
### NA_Sales and EU_Sales are the predictor variables. 
model3 = lm(Global_Sales~NA_Sales + EU_Sales, data=sales_num)


## View the parameters of Model 3.
summary(model3)
## The coefficients of NA sales and EU sales are statistically significant 
## (both p-values < 2e-16) indicating they are statistically significant 
## predictors of global sales.
## R-squared = 0.97 (very good).
## Adjusted R-squared = 0.97 (very good). 
## Residual standard error is 1.112 - relatively low. 
## F-statistic is 5398 with a p-value of < 2.2e-16, indicating that the 
## model is statistically significant. 
## Overall model could be a very good fit. 


## Add predicted values to the DataFrame. 
sales_num$predicted_GlobalSales <- predict(model3)


## View the DataFrame.
View(sales_num)


## Create a scatterplot of predicted vs. observed values.
ggplot(sales_num, aes(x = predicted_GlobalSales, y = Global_Sales)) +
  geom_point(color = "#A67D3D") +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  labs(x = "Predicted Global Sales", y = "Observed Global Sales",
       title = "Observed vs. Predicted Global Sales") +
  theme_minimal()


## Check accuracy of Model 3.
## The R squared and adjusted R squared values for this model are very good 
## (97%), and have increased compared to the simple linear regression models. 
## The MSE, RMSE, and MAE for the model were 1.23, 1.11 and 0.69 respectively, 
## which are acceptable in the context of predicting sales data and have 
## improved from Models 1 and 2. However the model did demonstrate 
## heteroscedasticity.

# Get predicted Global sales values from Model 3.
predicted3 <- predict(model3)


# Calculate residuals. 
residuals3 <- sales_num$Global_Sales - predicted3


# Calculate R-squared.
rsq <- summary(model3)$r.squared
cat("R-squared:", rsq, "\n")
# R-squared: 0.97


# Calculate Adjusted R-squared.
adj_rsq <- summary(model3)$adj.r.squared
cat("Adjusted R-squared:", adj_rsq, "\n")
# Adjusted R-squared: 0.97 


# Calculate MSE.
n <- length(residuals3)
mse <- (1/n) * sum(residuals3^2)
cat("MSE:", mse, "\n")
# MSE = 1.23


# Calculate RMSE.
rmse <- sqrt(mean(residuals3^2))
cat("RMSE:", rmse, "\n")
# RMSE = 1.11


# Calculate MAE.
mae <- mae(predicted3, sales_num$Global_Sales)
cat("MAE:", mae, "\n")
# MAE: 0.69 


## Check for multicollinearity using VIF. 

# Calculate VIF for the predictor variables.
vif(model3)
# NA_Sales 1.991094
# EU_Sales 1.991094
# Output is below the threshold of 5, indicating there is no multicollinearity
# between these independent variables. 


## Check for homoscedasticity.

# Plot the residuals against predicted values. 
ggplot(sales_num, aes(x = predicted3, y = residuals3)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Predicted Global Sales", y = "Residuals",
       title = "Residuals vs. Predicted Global Sales") +
  theme_minimal()


# Perform the Breusch-Pagan test
bp_test <- bptest(model3)


# View the p-value of the test. 
bp_test$p.value
## p = 1.912697e-06 (p < 0.05)
# Reject the null hypothesis.
# There is heteroscedasticity


################################

# 6.2.2 Multiple linear regression (Model 4) - outliers removed. 

# Calculate IQR.
Q1 <- quantile(sales_num$Global_Sales, 0.25)
Q3 <- quantile(sales_num$Global_Sales, 0.75)
IQR <- Q3 - Q1

# Calculate upper and lower bounds
upper <- Q3 + 1.5*IQR
lower <- Q1 - 1.5*IQR

# Identify outliers
outliers <- sales_num$Global_Sales > upper | sales_num$Global_Sales < lower

# Remove outliers
sales_clean <- sales_num[!outliers, ]

# Generate Model 4. 
model4 = lm(Global_Sales~NA_Sales + EU_Sales, data=sales_clean)


# View the parameters of Model 4.
summary(model4)
## The coefficients of NA sales and EU sales are statistically significant 
## (both p-values < 2e-16) indicating they are statistically significant 
## predictors of global sales.
## R-squared = 0.92 (very good).
## R-squared adjusted = 0.92 (very good).
## Residual standard error is 0.909 - relatively low. 
## F-statistic is 2010 with a p-value of < 2.2e-16, indicating that the 
## model is statistically significant. 
## Overall model could be a very good fit. 


# Add predicted values to the DataFrame. 
sales_clean$predicted_GlobalSales <- predict(model4)


# View the DataFrame.
View(sales_clean)


# Create a scatterplot of predicted vs. observed values.
ggplot(sales_clean, aes(x = predicted_GlobalSales, y = Global_Sales)) +
  geom_point(color = "#0072B2") +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  labs(x = "Predicted Global Sales", y = "Observed Global Sales",
       title = "Observed vs. Predicted Global Sales (no outliers)") +
  theme_minimal()


## Check accuracy of Model 4.
## The R squared and adjusted R squared values for this model are very good 
## (92%), but are less then Model 3. The MSE, RMSE, and MAE for the model were 
## 0.82, 0.90 and 0.58 respectively, an improvement on Model 3 values. In 
### addition, the model did not show heteroscedasticity.


# Get predicted Global sales values from Model 4.
predicted4 <- predict(model4)


# Calculate residuals. 
residuals4 <- sales_clean$Global_Sales - predicted4


# Calculate R-squared.
rsq <- summary(model4)$r.squared
cat("R-squared:", rsq, "\n")
# R-squared: 0.92


# Calculate Adjusted R-squared.
adj_rsq <- summary(model4)$adj.r.squared
cat("Adjusted R-squared:", adj_rsq, "\n")
# Adjusted R-squared: 0.92


# Calculate MSE.
n <- length(residuals4)
mse <- (1/n) * sum(residuals4^2)
cat("MSE:", mse, "\n")
# MSE  = 0.82


# Calculate RMSE
rmse <- sqrt(mean(residuals4^2))
cat("RMSE:", rmse, "\n")
# RMSE: 0.90 


# Calculate MAE.
mae <- mae(predicted4, sales_clean$Global_Sales)
cat("MAE:", mae, "\n")
# MAE: 0.58


# Check for multicollinearity using VIF. 

# Calculate VIF for the predictor variables.
vif(model4)
# NA_Sales 1.605393
# EU_Sales 1.605393
# Output is below the threshold of 5, indicating there is no multicollinearity
# between these independent variables. 


# Check for homoscedasticity. 

# Perform the Breusch-Pagan test
bp_test <- bptest(model4)

# View the p-value of the test. 
bp_test$p.value
## p = 0.057 (p > 0.05)
# Fail to reject the null hypothesis.
# There is no heteroscedasticity. 

################################

# 6.2.3 Multiple linear regression Model 5 - Grouped Data 

# Select only numeric columns from the original DataFrame.
sales_num2 <- select(sales_final, -Platform, -Year, 
                    -Genre, -Publisher)


# View the DataFrame. 
head(sales_num2)



# Group the sales data by sum of unique product ID numbers.
sales_group <- sales_num2 %>%
  group_by(Product) %>%
  summarise(Global_Sales = sum(Global_Sales),
            NA_Sales = sum(NA_Sales),
            EU_Sales = sum(EU_Sales))


# View the DataFrame.
head(sales_group)
str(sales_group)


# Create Model 5.
model5 = lm(Global_Sales~NA_Sales + EU_Sales, data=sales_group)


# View the parameters of Model 5.
summary(model5)
## The coefficients of NA sales and EU sales are statistically significant 
## (both p-values < 2e-16) indicating they are statistically significant 
## predictors of global sales.
## R-squared = 0.97 (very good).
## R-squared adjusted = 0.97 (very good).  
## Residual standard error is 1.49 - relatively low. 
## F-statistic is 2504 with a p-value of < 2.2e-16, indicating that the 
## model is statistically significant. 
## Overall model could be a very good fit. 

# Add predicted values to the DataFrame. 
sales_group$predicted_GlobalSales <- predict(model5)


# View the DataFrame.
View(sales_group)


# Create a scatterplot of predicted vs. observed values.
ggplot(sales_group, aes(x = predicted_GlobalSales, y = Global_Sales)) +
  geom_point(color = "#FF800E") +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  labs(x = "Predicted Global Sales", y = "Observed Global Sales",
       title = "Observed vs. Predicted Global Sales (Model 5)") +
  theme_minimal()


## Check accuracy of Model 5. 
## The R squared and adjusted R squared values for this model are very good 
## (97%). While the MSE, RMSE, and MAE for the model were slightly higher than 
## for Model 4 (2.18, 1.48 and 1.10 respectively), they are still acceptable
## in the context of predicting sales data. In addition, the model did not 
## show heteroscedasticity.

# Get predicted Global sales values from Model 5.
predicted5 <- predict(model5)


# Calculate residuals. 
residuals5 <- sales_group$Global_Sales - predicted5


# Calculate R-squared.
rsq <- summary(model5)$r.squared
cat("R-squared:", rsq, "\n")
# R-squared: 0.9667969


# Calculate Adjusted R-squared.
adj_rsq <- summary(model5)$adj.r.squared
cat("Adjusted R-squared:", adj_rsq, "\n")
# Adjusted R-squared: 0.9664108  


# Calculate MSE.
n <- length(residuals5)
mse <- (1/n) * sum(residuals5^2)
cat("MSE:", mse, "\n")
# MSE = 2.18


# Calculate RMSE
rmse <- sqrt(mean(residuals5^2))
cat("RMSE:", rmse, "\n")
# RMSE = 1.48


# Calculate MAE
mae <- mae(predicted5, sales_group$Global_Sales)
cat("MAE:", mae, "\n")
# MAE: 1.10


## Check for multicollinearity using VIF. 

# Calculate VIF for the predictor variables.
vif(model5)
# NA_Sales 1.627488
# EU_Sales 1.627488
# Output is below the threshold of 5, indicating there is no multicollinearity
# between these independent variables. 


# Check for homoscedasticity. 

# Plot the predicted values and the residuals to check for cone shape. 
ggplot(sales_group, aes(x = predicted5, y = residuals5)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Predicted Global Sales", y = "Residuals",
       title = "Residuals vs. Predicted Global Sales") +
  theme_minimal()

# Perform the Breusch-Pagan test
bp_test <- bptest(model5)

# View the p-value of the test. 
bp_test$p.value
## p = 0.1717833 (p > 0.05).
# Fail to reject the null hypothesis.
# There is no heteroscedasticity.

################################

# 6.2.3 Multiple linear regression Model 6 - Grouped data and outliers removed.

# Group the data as before. 
sales_group <- sales_num2 %>% 
  group_by(Product) %>% 
  summarize(Global_Sales = sum(Global_Sales),
            NA_Sales = sum(NA_Sales),
            EU_Sales = sum(EU_Sales))


# Calculate the IQR. 
q1 <- quantile(sales_group$Global_Sales, 0.25)
q3 <- quantile(sales_group$Global_Sales, 0.75)
iqr <- q3 - q1


# Calculate upper and lower bounds
upper <- q3 + 1.5*iqr
lower <- q1 - 1.5*iqr


# Identify the outliers
outliers <- sales_group$Global_Sales  > 
  upper | sales_group$Global_Sales < lower


# Remove outliers
sales_groupclean <- sales_group[!outliers, ]


# Create Model 6.
model6 = lm(Global_Sales~NA_Sales + EU_Sales, data=sales_groupclean)


# View the parameters of Model 6.
summary(model6)
## The coefficients of NA sales and EU sales are statistically significant 
## (both p-values < 2e-16) indicating they are statistically significant 
## predictors of global sales.
## R-squared = 0.91 (very good). 
## Adjusted R-squared = 0.91 (very good). 
## Residual standard error is 1.42 - relatively low. 
## F-statistic is 850.4 with a p-value of < 2.2e-16, indicating that the 
## model is statistically significant. 
## Overall model could be a very good fit. 


# Add predicted values to the DataFrame. 
sales_groupclean$predicted_GlobalSales <- predict(model6)


# View the DataFrame.
View(sales_groupclean)


# Create a scatterplot of predicted vs. observed values.
ggplot(sales_groupclean, aes(x = predicted_GlobalSales, y = Global_Sales)) +
  geom_point(color = "#8c92ac") +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  labs(x = "Predicted Global Sales", y = "Observed Global Sales",
       title = "Observed vs. Predicted Global Sales (without outliers") +
  theme_minimal()


## Check accuracy of Model 6. 
## The R squared and adjusted R squared values for this model are very good 
## (91%), but lower than Model 5. The MSE, RMSE, and MAEfor the model were 
## slightly lower than for Model 5 (1.97, 1.40 and 1.04 respectively). 
## The model also did not show heteroscedasticity.


# Get predicted Global sales values from Model 6.
predicted6 <- predict(model6)


# Calculate residuals. 
residuals6 <- sales_groupclean$Global_Sales - predicted6


# Calculate R-squared.
rsq <- summary(model6)$r.squared
cat("R-squared:", rsq, "\n")
# R-squared: 0.91  


# Calculate Adjusted R-squared.
adj_rsq <- summary(model6)$adj.r.squared
cat("Adjusted R-squared:", adj_rsq, "\n")
# Adjusted R-squared: 0.91 


# Calculate MSE.
n <- length(residuals6)
mse <- (1/n) * sum(residuals6^2)
cat("MSE:", mse, "\n")
# MSE = 1.97


# Calculate RMSE
rmse <- sqrt(mean(residuals6^2))
cat("RMSE:", rmse, "\n")
# RMSE: 1.40


# Calculate MAE
mae <- mae(predicted6, sales_groupclean$Global_Sales)
cat("MAE:", mae, "\n")
# MAE: 1.04 


# Check for homoscedasticity. 

# Plot the predicted values and the residuals to check for cone shape. 
ggplot(sales_groupclean, aes(x = predicted6, y = residuals6)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Predicted Global Sales", y = "Residuals",
       title = "Residuals vs. Predicted Global Sales") +
  theme_minimal()

# Perform the Breusch-Pagan test
bp_test <- bptest(model6)

# View the p-value of the test. 
bp_test$p.value
## p = 0.07099732 (p > 0.05).
# Fail to reject the null hypothesis.
# There is no heteroscedasticity.

############################################################################

# 6.3 Predictions based on given values.

## Model 5, which involved grouping data by unique product number, is the 
## best model for predicting global sales using EU sales and NA sales. 
## is based on its high R squared value (97%) and no heteroscedasticity. While
## its MSE, RMSE, and MAE values were slightly higher than for Model 6, this 
## acceptable for this business context, and the R-squared value is more
## important. 

## The following section compared observed values to predicted values using
## Model 5. Accuracy metrics (MSE, RMSE, and MAE) show low levels of error 
## overall and confirm that the model is a good fit. 

subset(sales_group, NA_Sales == 34.02 & EU_Sales == 23.80)
# Observed Global Sales for these values is 67.8
# Predicted value is 68.1

# Calculate MSE.
mse <- (67.8 - 68.1)^2
print(mse) 
# MSE = 0.09

# Calculate RMSE
rmse <- sqrt(mse)
print(rmse) 
# RMSE = 0.3

# Calculate MAE
mae <- abs(67.8 - 68.1)
print(mae) 
# MAE = 0.3

################################

subset(sales_group, NA_Sales == 11.50 & EU_Sales == 7.54)
# Observed Global Sales for these values is 23.8
# Predicted is 23.1

# Calculate MSE.
mse <- (23.8 - 23.1)^2
print(mse) 
# MSE = 0.49

# Calculate RMSE
rmse <- sqrt(mse)
print(rmse) 
# RMSE = 0.7

# Calculate MAE
mae <- abs(23.8 - 23.1)
print(mae) 
# MAE = 0.7

################################

subset(sales_group, NA_Sales == 5.60 & EU_Sales == 1.30)
# Observed Global Sales for these values is 7.04
# Predicted is 8.93

# Calculate MSE.
mse <- (7.04 - 8.93)^2
print(mse) 
# MSE = 3.57

# Calculate RMSE
rmse <- sqrt(mse)
print(rmse) 
# RMSE = 1.89

# Calculate MAE
mae <- abs(7.04 - 8.93)
print(mae) 
# MAE = 1.89


################################

subset(sales_group, NA_Sales == 2.18 & EU_Sales == 8.40)
# Observed Global Sales for these values is 13.3
# Predicted is 13.6

# Calculate MSE.
mse <- (13.3 - 13.6)^2
print(mse) 
# MSE = 0.09

# Calculate RMSE
rmse <- sqrt(mse)
print(rmse) 
# RMSE = 0.3

# Calculate MAE
mae <- abs(13.3 - 13.6)
print(mae) 
# MAE = 0.3

################################

subset(sales_group, NA_Sales == 1.37 & EU_Sales == 2.28)
# Observed Global Sales for these values is 5.47
# Predicted is 5.33 

# Calculate MSE.
mse <- (5.47 - 5.33)^2
print(mse) 
# MSE = 0.02

# Calculate RMSE
rmse <- sqrt(mse)
print(rmse) 
# RMSE = 0.14

# Calculate MAE
mae <- abs(5.47 - 5.33)
print(mae) 
# MAE = 0.14


############################################################################
############################################################################

# Q6. Observations and insights. 
## Outputs from the two simple linear regression models (Model 1 and Model 2) 
## were good. The resulting parameters showed that both models were a good fit
## and there was a strong positive linear relationship between both EU sales 
## and NA sales and global sales. The R-squared value for the EU sales model 
## was 77%, while for the NA model it was 87%. While 87% is a good value, 
## 77% is average, and should be improved. Therefore multiple linear 
## regression was performed with both EU and NA sales to see if a better and 
## more accurate model could be generated that could better predict global 
## sales.

## Four different multiple linear regression models were created and compared. 
## The models involved various permutations, including removing outliers or 
## grouping the sales by sum of unique Product sales. Parameters from all 
## four models showed that EU sales and NA sales were statistically 
## significant predictors of global sales.Model 3 (original DataFrame) had 
## very high R-squared and adjusted R squared values (97%) but demonstrated 
## heteroscedasticity. Model 4 (outliers removed) had high R-squared and 
## adjusted R squared values (92%), but these were lower than Model 3. Model 5 
## (data grouped by product) had very high R-squared and adjusted R squared 
## values (97%). The model also did not demonstrate heteroscedasticity. While 
## its MSE, RMSE, and MAE values were the highest of all models, they were 
## still acceptable in the context of forecasting sales data. Model 6 (grouped 
## data and outliers removed) had high R-squared and adjusted (91%), but 
## were lower than Model 5. The model did not show heteroscedasticity.

## Based on these findings, Model 5 was selected as the most accurate model 
## for predicting global sales based on EU and NA sales. Several predicted
## were compared to observed Global sales values. Accuracy metrics (MSE, RMSE, 
## and MAE) show low levels of error overall and confirm that the model is
## a good fit. 

## The sales department may find it useful to predict global sales from 
## regional sales for several reasons, including resource allocation and 
## strategic planning, creating better and more accurate sales forecasts,
## and to evaluate the effectiveness of sales strategies and tactics. 

############################################################################
############################################################################

## Sources.

## The Tech Wire (2022). Why Are Wii Games So Expensive? (Top 10 Reasons).
## Retrieved from https://www.thetechwire.com/why-are-wii-games-so-expensive. 

## Machine Learning Mastery (2021). Regression Metrics for Machine Learning.
## Retrieved from https://machinelearningmastery.com/regression-metrics-for-
## machine-learning. 

## Medium (2021). Evaluation Metrics for Regresson Models. Retrieved from 
## https://medium.com/analytics-vidhya/evaluation-metrics-for-regression-
## models-c91c65d73af

############################################################################
############################################################################
