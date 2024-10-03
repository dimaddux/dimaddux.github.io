library(usdata)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(tidyr)
library(shiny)
library(plotly)
library(DT)
library(rsconnect)
library(maps)
library(shinycssloaders)

# read crime data set
ogdata <- read.csv('crime_data_w_population_and_crime_rate.csv')

# read unemployment and poverty data sets
unem <- read.csv('UnemploymentDataSet.csv')
pov <- read.csv('PovertyEstimates.csv')

# Maps
states <- map_data("state")
counties <- map_data("county")

# Data Loading
load("MyData.RData")

# Shiny App
ui = fluidPage(
  titlePanel("United States Crime Rates (per 100,000)"),
  fluidRow(
    column(4,
           selectInput(inputId = "var1", label = "Select State:", choices = c("All", unique(crime_map$region))))
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Maps",
               fluidRow(
                 column(4,
                        HTML("<br>"),
                        selectInput(inputId = "var2", label = "Select Type of Crime:", choices = c("Violent Crime" = "Violent_Crime", "Property Crime" = "Property_Crime"))), 
                 column(4, 
                        HTML("<br>"), 
                        selectInput(inputId = "scale", label = "Select Color Scale:", choices = c("Viridis", "Gradient")))),
               plotlyOutput(outputId = "map") %>% withSpinner(color="#0dc5c1")),
      tabPanel("Data",
               fluidRow(
                 column(4,
                        HTML("<br>"),
                        uiOutput("test")
                 )),
               DTOutput(outputId = "mytable")),
      tabPanel("Economic Indicators",
               fluidRow(
                 column(4,
                        HTML("<br>"),
                        selectInput(inputId = "crime", label = "Select Type of Crime:", choices = c("Violent Crime" = "Violent_Crime", "Property Crime" = "Property_Crime"))),
                 column(4,
                        HTML("<br>"),
                        selectInput(inputId = "econ", label = "Select Economic Indicator:", choices = c("Median Household Income" = "Median_Household_Income", "Unemployment Rate" = "Unemployment_Rate")))),
               plotlyOutput(outputId = "myscatter")
      )
    )
  )
)

server = function(input, output){
  dat <- reactive({
    if(input$var1 == "All"){
      return(crime_map)
    } else{
      subset(crime_map, region == input$var1)
    }})
  dat2 <- reactive({
    if(input$var1 == "All"){
      return(states1)
    } else{
      subset(states1, region == input$var1)
    }
  })
  dat3 <- reactive({
    if(input$var1 == "All"){
      return(ogstates)
    } else{
      subset(og_clean, State == input$var1)
    }
  }
  )
  dat4 <- reactive({
    if(input$var1 == "All"){
      return(econstates)
    } else{
      subset(econ_clean, State == input$var1)
    }
  })
  
  
  output$map <- renderPlotly({
    w = switch(input$var1,
               "All" = 1400, "North Carolina" = 1000, "Wyoming" = 700, "New York" = 700,"Arkansas" = 600, "Iowa" = 700,"Washington" = 800, "Nebraska" = 900, "Kansas" = 800, "Virginia" = 900, "North Dakota" = 800, "South Dakota" = 800, "Texas" = 700, "Massachusetts" = 750, "Maryland" = 800, "Connecticut" = 700, "Colorado" = 700,"Tennessee" = 1000,"South Carolina" = 700, "Montana" = 900,"Pennsylvania" = 900, "Oklahoma" = 1000, "Kentucky" = 1000,"Oregon" = 700,"New Jersey" = 400, "Alabama" = 500, "Arizona" = 500, "Delaware" = 400, "Maine" = 400, "Illinois" = 400, "Nevada" = 500, "Indiana" = 500, "Idaho" = 500, "Mississippi" = 500, "New Hampshire" = 400, "Rhode Island" = 500, "Utah" = 400, "Vermont" = 500, 600)
    h = switch(input$var1,
               "All" = 700,
               "North Carolina" = 400,
               "Tennessee" = 300,
               500)
    if (input$var1 == "All") {
      p <- ggplot(data = dat(), aes(x = long, y = lat, group = group, text = paste("State:", region))) + 
        geom_polygon(data = dat2(), color = "black", fill = NA, linewidth = 0.5, aes(group = group)) +
        geom_polygon(data = dat(), color = "brown", linewidth = 0.2, alpha = 0.75, aes(fill = .data[[input$var2]])) + 
        scale_fill_continuous(type = str_to_lower(input$scale), trans = "sqrt") + theme_bw() +
        labs(fill = input$var2)
      ggplotly(p, width = w, height = h, tooltip = "text")
    }
    else{
      p <- ggplot(data = dat(), aes(x = long, y = lat, group = group, text = paste("County:", subregion, "<br>", "Crime Rate:", get(input$var2)))) + 
        geom_polygon(data = dat(), color = "brown", linewidth = 0.2, alpha = 0.75, aes(fill = .data[[input$var2]])) + 
        scale_fill_continuous(type = str_to_lower(input$scale), trans = "sqrt") + theme_bw() +
        labs(fill = input$var2)
      ggplotly(p, width = w, height = h, tooltip = "text") 
    } 
  }
  )
  output$test <- renderUI({
    if(input$var1 == "All") {
      selectInput("stateSub", label = "Select State:", 
                  choices = c("All States", unique(ogstates$State)), multiple = TRUE, selected = "All States")
    }
    
    else {
      sub <- subset(og_clean, State == input$var1)
      selectInput("countySub", label = "Select Counties:", 
                  choices = c("All Counties", unique(sub$County)), multiple = TRUE, selected = "All Counties")
    }
  })
  
  output$mytable <- renderDT({
    if(input$var1 == "All" & "All States" %in% input$stateSub) {
      return(ogstates)
    }
    else if(input$var1 == "All") {
      subset(ogstates, State %in% input$stateSub)
    }
    else if("All Counties" %in% input$countySub) {
      subset(og_clean, State %in% input$var1)
    }
    else {
      subset(og_clean, County %in% input$countySub & State %in% input$var1)
    }
  }
  )
  output$myscatter <- renderPlotly({
    fit <- lm(get(input$crime) ~ get(input$econ), data = dat4())
    plot_ly(data = dat4()) %>%
      add_trace(type = "scatter", mode = "markers", x = as.formula(paste0("~",input$econ)),
                y = as.formula(paste0("~",input$crime)), text = ~str_c("<b>", County)) %>%
      add_lines(x = as.formula(paste0("~",input$econ)), y = fitted(fit)) %>%
      layout(xaxis = list(title = 'Economic Indicator'),
             yaxis = list(title = 'Crime Rate'))
  }
  )
}

shinyApp(ui, server)
