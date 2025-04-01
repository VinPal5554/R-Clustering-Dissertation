library(shiny)

# UI (User Interface)
ui <- fluidPage(
  titlePanel("Cybersecurity Clustering Analysis"),
  
  
  
  sidebarLayout(
    sidebarPanel(
      # Input to select clustering method
      selectInput("method", 
                  label = "Choose Clustering Method", 
                  choices = c("Hierarchical", "K-Means", "DBSCAN")),
      
      # Dynamic UI for additional inputs based on clustering method
      uiOutput("dynamic_ui"),
      
      actionButton("apply", "Apply Clustering")
    ),
    mainPanel(
      plotOutput("clusterPlot"),
      verbatimTextOutput("clusterSummary")
    )
  )
)