library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Example dataset
df <- read_rds('shinyAMUsurvey/AMUdata.rds')

df7<-df%>%filter(str_detect(Question, 'Q7'))
#df <- read_rds('shinyAMUsurvey.rds')

# df$cyl <- factor(df$cyl)
# df$gear <- factor(df$gear)

ui <- navbarPage(
  "Data Explorer",
  
  tabPanel("What Matters",
           sidebarLayout(
             sidebarPanel(
               selectInput("x", "X-axis:", choices = names(df7), selected = 'Question'),
               selectInput("y", "Y-axis:", choices = names(df7), selected = 'Answer'),
               selectInput("color", "Color:", choices = c("None", names(df7)), selected = "None"),
               selectInput("fill", "Fill:", choices = c("None", names(df7)), selected = "None"),
               selectInput("facet_x", "Facet X:", choices = c("None", names(df7)), selected = "None"),
               selectInput("facet_y", "Facet Y:", choices = c("None", names(df7)), selected = "None"),
               radioButtons("facet_type", "Facet Type:",
                            choices = c("Wrap" = "wrap", "Grid" = "grid"),
                            selected = "wrap"),
               selectInput("geom_type", "Geom Type:",
                           choices = c("Point" = "point",
                                       "Bar" = "bar",
                                       "Boxplot" = "boxplot",
                                       "Line" = "line"),
                           selected = "point")
             ),
             mainPanel(
               plotOutput("plot"),
               hr()#,
               #DTOutput("table")
             )
           )
  ), 
  tabPanel('Data', 
           sidebarLayout(
             sidebarPanel(
               selectInput("x", "X-axis:", choices = names(df)),
               selectInput("y", "Y-axis:", choices = names(df)),
               selectInput("color", "Color:", choices = c("None", names(df)), selected = "None"),
               selectInput("fill", "Fill:", choices = c("None", names(df)), selected = "None"),
               selectInput("facet_x", "Facet X:", choices = c("None", names(df)), selected = "None"),
               selectInput("facet_y", "Facet Y:", choices = c("None", names(df)), selected = "None"),
               radioButtons("facet_type", "Facet Type:",
                            choices = c("Wrap" = "wrap", "Grid" = "grid"),
                            selected = "wrap"),
               selectInput("geom_type", "Geom Type:",
                           choices = c("Point" = "point",
                                       "Bar" = "bar",
                                       "Boxplot" = "boxplot",
                                       "Line" = "line"),
                           selected = "point")
             ),
             mainPanel(
               #plotOutput("plot"),
               hr(),
               DTOutput("table")
             )
           )
           )
  
)

server <- function(input, output, session) {
  
  # Reactive data for both plot and table
  selected_data <- reactive({
    df %>% 
      select(all_of(unique(c(input$x, input$y, 
                             if (input$color != "None") input$color else NULL,
                             if (input$fill != "None") input$fill else NULL,
                             if (input$facet_x != "None") input$facet_x else NULL,
                             if (input$facet_y != "None") input$facet_y else NULL))))
  })
  
  output$plot <- renderPlot({
    plot_data <- df
    
    p <- ggplot(plot_data, aes_string(x = input$x, y = input$y)) 
    
    if (input$color != "None") {
      p <- p + aes_string(color = input$color)
    }
    if (input$fill != "None") {
      p <- p + aes_string(fill = input$fill)
    }
    
    if (input$geom_type == "point") {
      p <- p + geom_point(size = 3, alpha = 0.7)
    } else if (input$geom_type == "bar") {
      p <- p + geom_bar(position = "dodge", stat = identity)
    } else if (input$geom_type == "boxplot") {
      p <- p + geom_boxplot()
    } else if (input$geom_type == "line") {
      p <- p + geom_line()
    }
    
    facet_x <- ifelse(input$facet_x == "None", ".", input$facet_x)
    facet_y <- ifelse(input$facet_y == "None", ".", input$facet_y)
    
    if (input$facet_type == "wrap") {
      p <- p + facet_wrap(as.formula(paste("~", facet_x)))
    } else {
      p <- p + facet_grid(as.formula(paste(facet_y, "~", facet_x)))
    }
    
    p + theme_minimal()
  })
  
  output$table <- renderDT({
    datatable(selected_data(), options = list(pageLength = 10))
  })
}

shinyApp(ui, server)
