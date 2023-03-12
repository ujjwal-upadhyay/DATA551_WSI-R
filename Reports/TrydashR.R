
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)

dataset <- read.csv('Sustainability_cleaned.csv')

marks <- list()
for (i in 2000:2018) {
  marks[[as.character(i)]] <- as.character(i)
}

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app <- Dash$new()

app$layout(
  dbcContainer(
    list(
      htmlH1('World Sustainability Dashboard',
             style = list(textAlign = 'center', color = 'blue')
      ),
      dbcTabs(list(
        dbcTab(label = 'Environmental', children = list(
          dccRangeSlider(
            id = 'slider-env',
            min = 2000,
            max = 2018,
            step = 1,
            marks = marks,
            value = list(2015, 2018)),
          htmlBr(),
          dccDropdown(
            id = 'dropdown-env',
            options = lapply(unique(dataset$Country), function(country) {
              list(label = country, value = country)
            }),
            value = 'India'
          ),
          htmlBr(),
          dccGraph(id='env-1')
        )
        ))
      ))
  )
)

app$callback(
  output('env-1', 'figure'),
  list(input('slider-env', 'value'), input('dropdown-env', 'value')),
  function(slider_value, dropdown_value) {
    #years_range <- slider_value[1]:slider_value[2]
    
    # Filter the data based on the dropdown values
    filtered_dataset <- subset(dataset, dataset$Country == dropdown_value & dataset$Year >= slider_value[1] & dataset$Year <= slider_value[2])
    
    # Filter the data based on the slider values
    #filtered_year <- subset(filtered_country, Year %in% years_range)
    
    # Create chart
    chart <- ggplot(filtered_dataset) + 
      aes(x = Internet,
          y = Electricity_access,
          color = Income_classification) + 
      geom_point(shape = "circle", size = 5, alpha = 0.6) +
      ggthemes::scale_color_tableau() 
    
    ggplotly(chart) %>% layout(dragmode = 'select')
  }
)

app$run_server(debug = T)

dataset <- read.csv('Sustainability_cleaned.csv')
head(dataset)
years_range <- 2000:2018

filtered_dataset <- subset(dataset, dataset$Country == "India" & dataset$Year >= years_range[1] & dataset$Year <= years_range[-length(years_range)])
head(filtered_dataset,10)

chart <- ggplot(filtered_dataset) + 
  aes(x = Internet,
      y = Electricity_access,
      color = Income_classification) + 
  geom_point(shape = "circle", size = 5, alpha = 0.6) +
  ggthemes::scale_color_tableau() 

ggplotly(chart) %>% layout(dragmode = 'select')


## eco-1
library(lubridate)
library(tidyverse)
dataset <- read.csv('Sustainability_cleaned.csv')
# Melt data frame
melted <- dataset %>% 
  pivot_longer(cols = c("Imports", "Exports"), names_to = "impexp", values_to = "GDP__per_capita") %>% 
  mutate(Year = year(parse_date_time(Year, orders = "%Y")))

# Create chart
chart <- melted %>% 
  ggplot(aes(x = impexp, y = GDP__per_capita, fill = impexp)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(vars(Year), ncol = 5 , strip.position = "bottom") +
  #xlab(NULL) +
  labs(title = "Import and Export as % of GDP per Capita", y = "Trade(%) of GDP per capita", fill = "Trade type", x = NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold", margin = margin(b = 20)),
        strip.text = element_text(hjust = 0.5, size = 16, face = "bold"))

# Add tooltip
gg <- ggplotly(chart, tooltip = c("GDP__per_capita", "impexp"))
gg


## summary-1
library(ggplot2)
dataset <- read.csv('Sustainability_cleaned.csv')
filtered_data <- subset(dataset, dataset$Year == 2010 & dataset$Income_classification == "High income")
head(filtered_data)

chart <- ggplot(filtered_data, aes(x=GDP_per_capita, y=Inflation, size=Population, color=Country)) +
  geom_point(alpha=0.5) +
  labs(x="GDP per capita", y="Inflation", color=NULL, size=NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        legend.position = "none")

gg <- ggplotly(chart, tooltip = c("Country", "Inflation"))
gg


## summary-2
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

dataset <- read.csv('Sustainability_cleaned.csv')
filtered_data <- subset(dataset, dataset$Year == 2010 & dataset$Income_classification == c("High income","Low income"))
head(filtered_data)

counts <- filtered_data %>%
  count(Income_classification)

# Create a pie chart
pie_chart <- ggplot(counts, aes(x="", y=n, fill=Income_classification)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  scale_fill_viridis(discrete=TRUE) +
  labs(title="Income Classification")

# Add labels to the chart
pie_chart 