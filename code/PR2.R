#PR2

#SEGONA VISUALITZACIÓ
setwd('/Users/alexb/OneDrive/Escritorio/UOC/VD_Visualització_de_dades/PR2/')

library(ggplot2)
library(reshape2)
library(gridExtra)
library(dplyr)
library(shiny)
library(plotly)

data <- read.csv(file = "GlobalTemperatures.csv", header = TRUE)

# Preprocessar les dades per la primera figura (plotly)
data1 <- data[, c(1, 2, 4, 6)]
data1 <- data1[complete.cases(data1), ]
data1$dt <- as.Date(data1$dt)
data1$Year <- format(data1$dt, "%Y")
data1$Month <- format(data1$dt, "%m")
data1 <- data1[, -1]

# Preprocessar les dades per la segona figura  (ggplot2)
data2 <- data[, c(1, 8)]
data2 <- data2[complete.cases(data2), ]
data2$dt <- as.Date(data2$dt)
data2$year <- format(data2$dt, "%Y")
average_temp_by_year <- aggregate(LandAndOceanAverageTemperature ~ year, data = data2, FUN = mean)
average_temp_by_year$year <- as.numeric(as.character(average_temp_by_year$year))



# Definir UI 
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML('body { background-color: #1a1a1a; color: white; }'),
      HTML('.selectize-control { border: none; }')
    )
  ),
  titlePanel(h1("", style = "font-size: 17px;")),
  sidebarLayout(
    sidebarPanel(
      selectInput("month", "Select Month", choices = month.name, selected = "January"),
      style = "width: 150px; background-color: #1a1a1a; padding: 10px; border: 2px solid #212121; margin-left: 250px;"  # Adjust width and background color
    ),
    mainPanel(
      plotlyOutput("temperaturePlot"),
      plotlyOutput("temperatureGGPlot")
    )
  )
)

server <- function(input, output) {
  # Funció reactiva per filtrar les dades segons el mes
  filtered_data <- reactive({
    month_selected <- match(input$month, month.name)
    data_filtered <- data1[data1$Month == sprintf("%02d", month_selected), ]
    return(data_filtered)
  })
  
  # 1a figura
  output$temperaturePlot <- renderPlotly({
    plot_ly() %>%
      add_trace(data = filtered_data(), x = ~Year, y = ~LandMaxTemperature, type = 'scatter', mode = 'lines', name = 'Max Temperature',
                line = list(color = '#ff4040'),
                legendgroup = 'Max') %>%
      add_trace(data = filtered_data(), x = ~Year, y = ~LandAverageTemperature, type = 'scatter', mode = 'lines', name = 'Average Temperature',
                fill = 'tonexty',
                line = list(color = '#40a040'),
                fillcolor = toRGB('rgba(255, 92, 92, 0.3)'),
                legendgroup = 'Average') %>%
      add_trace(data = filtered_data(), x = ~Year, y = ~LandMinTemperature, type = 'scatter', mode = 'lines', name = 'Min Temperature',
                fill = 'tonexty',
                line = list(color = '#4040ff'),
                fillcolor = toRGB('rgba(92, 128, 247, 0.3)'),
                legendgroup = 'Min') %>%
      layout(title = 'Temperature Trends Over the Years',
             xaxis = list(title = 'Year',
                          tickmode = 'array',
                          tickvals = seq(min(filtered_data()$Year), max(filtered_data()$Year), by = 10),
                          ticktext = seq(min(filtered_data()$Year), max(filtered_data()$Year), by = 10)),
             yaxis = list(title = 'Temperature'),
             paper_bgcolor = '#1a1a1a',
             plot_bgcolor = '#1a1a1a',
             font = list(color = 'white'))
  })
  
  #2a figura
  output$temperatureGGPlot <- renderPlotly({
    gg <- ggplot(average_temp_by_year, aes(x = year, y = LandAndOceanAverageTemperature, group = 1)) +
      geom_smooth(alpha = 0.2, colour = "white") +
      geom_point(aes(color = LandAndOceanAverageTemperature), size = 2) +
      scale_color_gradient(limits = c(14.75, 17), low = "#7cbff2", high = "#ff1e00") +
      labs(title = "Land And Ocean Average Temperature Over the Years") +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        plot.background = element_rect(fill = "#1a1a1a"),
        panel.background = element_rect(fill = "#1a1a1a"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "#d9d9d9")
      ) +
      scale_x_continuous(breaks = seq(1850, 2015, 5))
    
    ggplotly(gg, colors = c("#7cbff2", "#ff1e00"), limits = list(color = c(14.75, 17)))
  })
}

shinyApp(ui, server)



# Pre processat de la tercera visualitació

#-------------------------------------------------------------------------------

df <- read.csv(file = "GlobalLandTemperaturesByCountry.csv", header = T)


df <- df[,-3]
df$dt <- as.Date(df$dt)
df$year <- format(df$dt, "%Y")

average_temp_by_year <- aggregate(AverageTemperature  ~ year + Country, data = df, FUN = mean)
average_temp_by_year$year <- as.numeric(as.character(average_temp_by_year$year))

head(average_temp_by_year)

# Filter for the year 2013
data_2013 <- subset(average_temp_by_year, year == 2013)

# Calculate the average temperature for all years for each country
average_all_years <- aggregate(AverageTemperature ~ Country, data = average_temp_by_year, FUN = mean)

# Merge the data for 2013 and the overall average temperature for each country
merged_data <- merge(data_2013, average_all_years, by = "Country", suffixes = c("_2013", "_avg"))

# Calculate the variance
merged_data$variance <- (merged_data$AverageTemperature_2013 - merged_data$AverageTemperature_avg)^2
merged_data$difference <- (merged_data$AverageTemperature_2013 - merged_data$AverageTemperature_avg)

# Print the result or use it as needed
write.csv(merged_data, file = "TemperatureDifference2013FromAverageByCountry.csv")







