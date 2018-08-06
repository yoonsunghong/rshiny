# ===================================================================
# Title: gradevis.R
# Description:
#   This script creates interactive Shiny apps with 3 different tabs
# Author: Yoon Sung Hong
# Date: 11-14-2017
# ===================================================================

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggvis)
library(ggplot2)
library(dplyr)
source('../code/functions.R')
dat <- read.csv('../data/cleandata/cleanscores.csv')

# convert some variables as factors, for barcharts
dat$Grade <- as.factor(dat$Grade)

#creating prop table
gdist <- as.data.frame(table(dat$Grade))
gdist <- cbind(gdist,prop.table(table(dat$Grade)))
gdist <- gdist[,c(1,2,4)]
colnames(gdist) <- c('Grade', 'Freq', 'Prop')
gdist <- gdist[c(3,1,2,6,4,5,9,7,8,10,11),]
gdist$Prop <- round(gdist$Prop, 2)

# Variable names for barcharts
categorical <- colnames(dat) 
categorical <- categorical[1:22]


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Grade Visualizer"),
  
  # Sidebar with different widgets depending on the selected tab
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tabselected==1",
                       h3("Grades Distribution"),
                       tableOutput('tbl')
      ),
      conditionalPanel(condition = "input.tabselected==2",
                       selectInput("var2", "X-axis variable", categorical, 
                                   selected = "HW1"),
                       sliderInput("width", "Bin Width", 
                                   min = 1, max = 10, value = 10)),
      conditionalPanel(condition = "input.tabselected==3",
                       selectInput("var3", "X-axis variable", categorical, 
                                   selected = "Test1"),
                       selectInput("var4", "Y-axis variable", categorical, 
                                   selected = "Overall"),
                       sliderInput("opacity", "Opacity", 
                                   min = 0, max = 1, value = 0.5),
                       radioButtons("line", "Show line", 
                                    c("none" = "none", "lm" = "lm", 
                                      "loess" = "loess")))
    )
    ,
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Barchart", value = 1, 
                           ggvisOutput("barchart")),
                  tabPanel("Histogram", value = 2, 
                           ggvisOutput("histogram"),
                           textOutput("sumstats"),
                           verbatimTextOutput("summary")),
                  tabPanel("Scatterplot", value = 3, 
                           ggvisOutput("scatterplot"),
                           textOutput("Correlation"),
                           verbatimTextOutput("corr")),
                  id = "tabselected")
    )
  )
)


# Define server logic
server <- function(input, output) {
  # Prop table of grades
  output$tbl <- renderTable({gdist})  
  # Barchart (for 1st tab)
  vis_barchart <- reactive({
    gdist %>%
      ggvis(x = ~Grade, y = ~Freq) %>% 
      scale_ordinal('x', 
                    domain = c('A+', 'A', 'A-', 'B+', 'B', 'B-', 
                               'C+', 'C', 'C-', 'D', 'F' ))
  })
  
  vis_barchart %>% bind_shiny("barchart")
  
  
  # Histogram (for 2nd tab)
  vis_histogram <- reactive({
    var2 <- prop("x", as.symbol(input$var2))
    dat %>% 
      ggvis(x = var2, fill := "#abafb5") %>% 
      layer_histograms(stroke := 'white',
                       width = input$width)
  })
  vis_histogram %>% bind_shiny("histogram")
  
  # Text for summary stats
  output$sumstats <- renderText({
    "Summary Statistics"
  })
  
  # Summary Stats (for 2nd tab)
  output$summary <- renderPrint({
    var2 <- prop("x", as.symbol(input$var2))
    value = as.vector(dat[,as.character(var2)])
    stat = summary_stats(value)
    print_stats(stat)
  })
  
  # scatter (for 3rd tab)
  vis_scatterplot <- reactive({

    var3 <- prop("x", as.symbol(input$var3))
    var4 <- prop("y", as.symbol(input$var4))
    var5 <- input$line
    
    if (var5 == "loess") {
      dat %>% 
        ggvis(x = var3, y = var4, opacity := input$opacity) %>% 
        layer_points() %>%
        layer_smooths()
      
    } else if (var5 == "lm") {
      dat %>% 
        ggvis(x = var3, y = var4, opacity := input$opacity) %>% 
        layer_points() %>%
        layer_model_predictions(model = "lm")
      
    } else {
      dat %>% 
        ggvis(x = var3, y = var4, opacity := input$opacity) %>% 
        layer_points() 
    }
    
    
  })
  vis_scatterplot %>% bind_shiny("scatterplot")
  
  #Text for correlation
  output$Correlation <- renderText({
    "Correlation:"
  })
  
  # Correlation (for 3rd tab)
  output$corr <- renderPrint({
    var3 <- prop("x", as.symbol(input$var3))
    var4 <- prop("x", as.symbol(input$var4))
    c2 <- which(names(dat) == input$var3)
    c3 <- which(names(dat) == input$var4)
    cat(cor(dat[,c2],dat[,c3]))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)



