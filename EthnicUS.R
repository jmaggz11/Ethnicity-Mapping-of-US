
rsconnect::setAccountInfo(name="justin-mcguire", token="55D57412D5F5EEC3655ECE5F564D5892", secret="IOFSGrWpvYLR0ItPAQuvVm+EaVM4BL9MuNswd8W8")

percent_map <- function(var, color, legend.title, min = 0, max = 100) {
  
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("black", color))(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
                             include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]
  
  # plot choropleth map
  map("county", fill = TRUE, col = fills, 
      resolution = 0, lty = 0, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
      lty = 1, lwd = 1, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0))
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min, " % or less"),
                   paste0(min + inc, " %"),
                   paste0(min + 2 * inc, " %"),
                   paste0(min + 3 * inc, " %"),
                   paste0(max, " % or more"))
  
  legend("bottomleft", 
         legend = legend.text, 
         fill = shades[c(1, 25, 50, 75, 100)], 
         title = legend.title)
}

library(shiny)
counties <- readRDS("/Users/justinmcguire/Downloads/counties.rds")
library(maps)
library(mapproj)

server<-shinyServer(
  function(input, output) {
    
    output$map <- renderPlot({
      data <- switch(input$var, 
                     "Percent Caucasian" = counties$white,
                     "Percent African American" = counties$black,
                     "Percent Hispanic" = counties$hispanic,
                     "Percent Asian" = counties$asian)
      
      color <- switch(input$var, 
                      "Percent Caucasian" = "deeppink1",
                      "Percent African American" = "chartreuse1",
                      "Percent Hispanic" = "darkorchid1",
                      "Percent Asian" = "cyan1")
      
      legend <- switch(input$var, 
                       "Percent Caucasian" = "Percent Caucasian",
                       "Percent African American" = "Percent African American",
                       "Percent Hispanic" = "Percent Hispanic",
                       "Percent Asian" = "Percent Asian")
      
      percent_map(var = data, color = color, legend.title = legend, max = input$range[2], min = input$range[1])
    })
  }
)

ui<-shinyUI(fluidPage(
  titlePanel("Mapping the US ethnicity"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("This map of the United States indicates the population of certain ethnicities in certain areas"),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Percent Caucasian", "Percent African American",
                              "Percent Hispanic", "Percent Asian"),
                  selected = "Percent Caucasioan"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))
    ),
    
    mainPanel(plotOutput("map"))
  )
))

shinyApp(ui,server)