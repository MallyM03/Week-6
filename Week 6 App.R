##Week 6##

if(!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  shiny,
  ggplot2,
  dplyr
)

dat1<-read.csv("emdat_app")
dat1<-emdat_app


ui<-fluidPage(
  h1("Disaster Statsitics Trends"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "country",
        label= "Select a Country",
        choices = unique(dat1$country),
        selected = "Belgium"
      ),
      
      selectInput(
        inputId = "variable",
        label= "Variable-choose 1 of 3",
        choices = c("Deaths","Injuries","Homelessness"),
        selected = "Deaths"
      ),
      
      sliderInput(
        inputId = "year_range",
        label = "Select a Range",
        min = min(dat1$Year),
        max = max(dat1$Year),
        value = c(min(dat1$Year), max(dat1$Year)),
        step = 1,
        sep= ""
      )
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    variable <- switch(input$variable,
                       "Deaths" = "deaths",
                       "Injuries" = "injuries",
                       "Homelessness" = "homelessness")
    
    dat1 %>%
      filter(country == input$country, 
             Year >= input$year_range[1], 
             Year <= input$year_range[2]) %>%
      ggplot(aes(Year, .data[[variable]])) +
      geom_line() +
      labs(y = input$variable)
  })
  
}


shinyApp(ui, server)

