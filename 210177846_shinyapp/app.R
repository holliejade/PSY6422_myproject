

library(shiny)
library(plotly)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)
library(knitr)

df <- read.csv(here("data", "global_mortality.csv"))
df_tb <- as_tibble(df)

# Changing the names of particular columns that I want to use, so it makes it easier when displaying the data as an interactive plot 
names(df_tb)[1] <- "Region"
names(df_tb)[3] <-  "Year"
names(df_tb)[8] <- "Dementia"
names(df_tb)[26] <- "PD"


# Selecting the appropriate rows and columns for data visualisation
mortality <- df_tb[c(5752:5805, 6049:6075), c("Region", "Year", "Dementia", "PD")]

# Define UI for application that draws a histogram


ui <-  fluidPage(
  titlePanel("Global Mortality"),
  
  # Creating the drop-down bar to allow for choosing between two different plots
  sidebarPanel(
    inputPanel(
      selectInput("per_death", label = "Percentage of deaths, per year, caused by:",
                  choices = c("Dementia", "Parkinson's Disease"), selected = "Dementia"),
    )),
  mainPanel(fluidRow(plotlyOutput("mortalityPlot", width = 800, height = 400))
  )
)

server <-  function(input, output, session){
  # Creating the output plots so they are shown when the appropriate option is selected  
  
  output$mortalityPlot <- renderPlotly({
    
    # This function allows me to create a plot that only appears when the "Dementia" option is chosen 
    if(input$per_death == "Dementia") { 
      
      # Attributing the "Dementia" plot to the first variable named "p1", making sure the x and y axis are displayed correctly, and the colours represent the different countries
      p1 <-  ggplot(mortality, aes(fill = Region, y = Dementia, x = Year)) + 
        
        # Creating a grouped bar plot to allow for data for the UK and USA to appear side-by-side for each year 
        geom_bar(position="dodge", stat="identity") +
        
        # Formatting the x axis to allow room for each year to be displayed (each year will appear perpendicular to the x axis)
        theme(axis.text.x = element_text(angle = 90, size = 10)) +    
        
        # Editing the x axis to show each year consecutively
        scale_x_continuous(breaks = seq(from = 1990, to = 2016, by = 1), limits = c(1989, 2017)) +
        
        # Creating the x and y axis labels 
        labs(x = "Year", y = "Deaths caused by Dementia (%)") +
        
        # making sure to use colours that are distinguishable to colour-blind individuals
        scale_fill_manual(values=c("darkorange2", "cornflowerblue", "limegreen")) 
      
      print(ggplotly(p1))
      
      
      
      # creating a plot that only appears if "Parkinson's disease" is chosen, and not "Dementia" 
      
    } else if (input$per_death == "Parkinson's Disease") { 
      
      # Attributing the "Parkinson's Disease" plot to the second variable named "p2", making sure the x and y axis are displayed correctly (corresponding to the previously modified column names)
      p2 <- ggplot(mortality, aes(fill = Region, y = PD, x = Year)) + 
        
        geom_bar(position="dodge", stat ="identity") + 
        
        theme(axis.text.x = element_text(angle = 90, size = 10)) +
        
        scale_x_continuous(breaks = seq(from = 1990, to = 2016, by = 1), limits = c(1989, 2017)) +
        
        labs(x = "Year", y = "Deaths caused by Parkinson's Disease (%)") +
        
        # Ensuring that the same colours are used for the "Parkinson's disease" plot, and making sure they're in the correct order so that the UK is orange and the US is blue 
        scale_fill_manual(values=c("darkorange2", "cornflowerblue", "limegreen")) 
      
      print(ggplotly(p2))
      
      
      
    }
    
    
    
  }
  )
}



shinyApp(ui = ui, server = server)