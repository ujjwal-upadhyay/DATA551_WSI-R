library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)

dataset <- read.csv('data/processed/Sustainability_cleaned.csv')

marks <- list()
for (i in 2000:2018) {
  marks[[as.character(i)]] <- as.character(i)
}

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(htmlDiv(list(
  htmlH1(
    'World Sustainability Dashboard',
    style = list(textAlign = 'center', color = 'blue')
  ),
  dccTabs(
    id = "tabs",
    value = 'tab-1-env',
    children = list(
      #Enviroment Tab-1
      dccTab(
        label = 'Environmental',
        value = 'tab-1-env',
        children = list(dbcRow(list(
          dbcCol(
            list(
              htmlLabel('Please select the year range for the graphs'),
              dccRangeSlider(
                id = 'slider-env',
                min = 2000,
                max = 2018,
                step = 1,
                vertical = TRUE,
                marks = marks,
                value = list(2015, 2018)
              ),
              htmlLabel('Please select the country from the dropdown for the graphs'),
              dccDropdown(
                id = 'dropdown-env',
                options = lapply(unique(dataset$Country), function(country) {
                  list(label = country, value = country)
                }),
                value = 'Finland'
              )
            ),
            width = 3,
            md = 3
          ),
          dbcCol(list(
            dccGraph(id = 'env-1'),
            dccGraph(id = 'env-2')
          ), width = 10, md = 7)
        )))
      ),
      #Social Tab-2
      dccTab(
        label = 'Social',
        value = 'tab-2-soc',
        children = list(dbcRow(list(
          dbcCol(
            list(
              htmlLabel('Please select the year range for the graphs'),
              dccRangeSlider(
                id = 'slider-soc',
                min = 2000,
                max = 2018,
                step = 1,
                vertical = TRUE,
                marks = marks,
                value = list(2015, 2018)
              ),
              htmlLabel('Please select the country from the dropdown for the graphs'),
              dccDropdown(
                id = 'dropdown-soc',
                options = lapply(unique(dataset$Country), function(country) {
                  list(label = country, value = country)
                }),
                value = 'Finland'
              )
            ),
            width = 3,
            md = 3
          ),
          dbcCol(list(
            dccGraph(id = 'soc-1'),
            dccGraph(id = 'soc-2')
          ), width = 10, md = 7)
        )))
      ),
      #Economical Tab-3
      dccTab(
        label = 'Economical',
        value = 'tab-3-eco',
        children = list(dbcRow(list(
          dbcCol(
            list(
              htmlLabel('Please select the year range for the graphs'),
              dccRangeSlider(
                id = 'slider-eco',
                min = 2000,
                max = 2018,
                step = 1,
                vertical = TRUE,
                marks = marks,
                value = list(2015, 2018)
              ),
              htmlLabel('Please select the country from the dropdown for the graphs'),
              dccDropdown(
                id = 'dropdown-eco',
                options = lapply(unique(dataset$Country), function(country) {
                  list(label = country, value = country)
                }),
                value = 'Finland'
              )
            ),
            width = 3,
            md = 3
          ),
          dbcCol(list(dccGraph(id = 'eco-1')), width = 10, md = 7)
        )))
      ),
      #Summary Tab-4
      dccTab(
        label = 'Summary',
        value = 'tab-4-summ',
        children = list(dbcRow(list(
          dbcCol(
            list(
              htmlLabel('Please select the year from the dropdown for the graphs'),
              dccDropdown(
                id = 'year-summ',
                options = lapply(unique(dataset$Year), function(year) {
                  list(label = year, value = year)
                }),
                value = 2018
              ),
              htmlLabel('Please select an Income Group'),
              dccDropdown(
                id = 'income-summ',
                options = lapply(unique(dataset$Income_classification), function(class) {
                  list(label = class, value = class)
                }),
                value = list(
                  'High income',
                  'Low income',
                  'Upper-middle income',
                  'Lower-middle income'
                ),
                multi = TRUE
              )
            ),
            width = 3,
            md = 3
          ),
          dbcCol(list(
            dccGraph(id = 'summ-1')
          ))
        )))
      )
    )
  )
)))

#Environment - Chart 1
app$callback(output('env-1', 'figure'),
             list(input('slider-env', 'value'), input('dropdown-env', 'value')),
             function(slider_value, dropdown_value) {
               # Filter the data based on the dropdown values
               filtered_dataset <-
                 subset(
                   dataset,
                   dataset$Country == dropdown_value &
                     dataset$Year >= slider_value[1] &
                     dataset$Year <= slider_value[2]
                 )
               
               # Create chart
               chart <- ggplot(filtered_dataset) +
                 aes(x = Internet,
                     y = Electricity_access,
                     color = Income_classification) +
                 ggtitle('Access to Utilities, as a percentage of Population') +
                 xlab('Access to Internet (% of population)') +
                 ylab('Access to electricity (% of population)') +
                 geom_point(shape = "circle",
                            alpha = 0.6,
                            size = 5)
               ggthemes::scale_color_tableau()
               
               ggplotly(chart) %>% layout(dragmode = 'select')
             })

#Environment - Chart 2
app$callback(output('env-2', 'figure'),
             list(input('slider-env', 'value'), input('dropdown-env', 'value')),
             function(slider_value, dropdown_value) {
               # Filter the data based on the dropdown values
               filtered_dataset <-
                 subset(
                   dataset,
                   dataset$Country == dropdown_value &
                     dataset$Year >= slider_value[1] &
                     dataset$Year <= slider_value[2]
                 )
               
               # Create chart
               chart <- ggp <- ggplot(filtered_dataset)  +
                 geom_bar(
                   aes(x = Year, y = Co2_prod_tonnes),
                   stat = "identity",
                   fill = "cyan",
                   colour = "#006000"
                 ) +
                 labs(x = "Year", y = "Annual CO2 production (in tonnes)") +
                 ggthemes::scale_color_tableau()
               
               ggplotly(chart) %>% layout(dragmode = 'select')
             })

#Social - Chart 1
app$callback(output('soc-1', 'figure'),
             list(input('slider-soc', 'value'), input('dropdown-soc', 'value')),
             function(slider_value, dropdown_value) {
               # Filter the data based on the dropdown values
               filtered_dataset <-
                 subset(
                   dataset,
                   dataset$Country == dropdown_value &
                     dataset$Year >= slider_value[1] &
                     dataset$Year <= slider_value[2]
                 )
               
               df_renamed_long <-
                 tidyr::pivot_longer(
                   filtered_dataset,
                   cols = c("Primary", "Secondary"),
                   names_to = "School_Enrolment",
                   values_to = "value"
                 )
               
               # Create chart
               chart <-
                 ggplot(df_renamed_long,
                        aes(x = Year, y = value, color = School_Enrolment)) +
                 geom_line() +
                 geom_point() +
                 scale_y_continuous(name = "Enrolment (%)", expand = expansion(mult = c(0, 0.05))) +
                 labs(title = "Literacy level")
               ggthemes::scale_color_tableau()
               
               ggplotly(chart) %>% layout(dragmode = 'select')
             })

#Social - Chart 2
app$callback(output('soc-2', 'figure'),
             list(input('slider-soc', 'value'), input('dropdown-soc', 'value')),
             function(slider_value, dropdown_value) {
               # Filter the data based on the dropdown values
               filtered_dataset <-
                 subset(
                   dataset,
                   dataset$Country == dropdown_value &
                     dataset$Year >= slider_value[1] &
                     dataset$Year <= slider_value[2]
                 )
               
               colnames(filtered_dataset)[11] <- 'Male'
               colnames(filtered_dataset)[12] <- 'Female'
               
               df_renamed_long <-
                 tidyr::pivot_longer(
                   filtered_dataset,
                   cols = c("Female", "Male"),
                   names_to = "Gender",
                   values_to = "value"
                 )
               
               # Create chart
               chart <-
                 ggplot(df_renamed_long, aes(x = Year, y = value, color = Gender)) +
                 geom_line() +
                 geom_point() +
                 scale_y_continuous(name = "Unemployment Rate (%)", expand = expansion(mult = c(0, 0.05))) +
                 labs(title = "Gender Gap in Unemployment")
               ggthemes::scale_color_tableau()
               
               ggplotly(chart) %>% layout(dragmode = 'select')
             })

#Economical - Chart 1
app$callback(output('eco-1', 'figure'),
             list(input('slider-eco', 'value'), input('dropdown-eco', 'value')),
             function(slider_value, dropdown_value) {
               # Filter the data based on the dropdown values
               filtered_dataset <-
                 subset(
                   dataset,
                   dataset$Country == dropdown_value &
                     dataset$Year >= slider_value[1] &
                     dataset$Year <= slider_value[2]
                 )
               
               # Melt data frame
               melted <- filtered_dataset %>%
                 pivot_longer(
                   cols = c("Imports", "Exports"),
                   names_to = "impexp",
                   values_to = "GDP__per_capita"
                 ) %>%
                 mutate(Year = year(parse_date_time(Year, orders = "%Y")))
               
               # Create chart
               chart <- melted %>%
                 ggplot(aes(x = impexp, y = GDP__per_capita, fill = impexp)) +
                 geom_bar(stat = "identity", position = "dodge") +
                 facet_wrap(vars(Year), ncol = 5 , strip.position = "bottom") +
                 labs(
                   title = "Import and Export as % of GDP per Capita",
                   y = "Trade(%) of GDP per capita",
                   fill = "Trade type",
                   x = NULL
                 ) +
                 theme(
                   plot.title = element_text(
                     hjust = 0.5,
                     size = 20,
                     face = "bold",
                     margin = margin(b = 20)
                   ),
                   strip.text = element_text(
                     hjust = 0.5,
                     size = 16,
                     face = "bold"
                   )
                 )
               ggthemes::scale_color_tableau()
               
               ggplotly(chart) %>% layout(dragmode = 'select')
             })

#Summary - Chart 1
app$callback(output('summ-1', 'figure'),
             list(input('year-summ', 'value'),
                  input('income-summ', 'value')),
             function(year_value, income_value) {
               # Filter the data based on the dropdown values
               filtered_dataset <-
                 subset(
                   dataset,
                   dataset$Year == year_value &
                     dataset$Income_classification %in% income_value
                 )
               
               chart <-
                 ggplot(
                   filtered_dataset,
                   aes(
                     x = GDP_per_capita,
                     y = Inflation,
                     size = Population,
                     color = Country
                   )
                 ) +
                 geom_point(alpha = 0.5) +
                 labs(
                   x = "GDP per capita",
                   y = "Inflation",
                   color = NULL,
                   size = NULL
                 ) +
                 theme_bw() +
                 theme(
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   panel.border = element_blank(),
                   legend.position = "none"
                 )
               ggthemes::scale_color_tableau()
               
               ggplotly(chart) %>% layout(dragmode = 'select')
             })

app$run_server(debug = T)