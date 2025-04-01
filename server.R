library(shiny)
library(ggplot2)
library(cluster)
library(dbscan)

server <- function(input, output, session) {
  
  # Reactive UI for additional clustering parameters
  output$dynamic_ui <- renderUI({
    if (input$method == "Hierarchical") {
      tagList(
        selectInput("linkage", "Choose Linkage Method", 
                    choices = c("complete", "average", "single"))
      )
    } else if (input$method == "K-Means") {
      tagList(
        numericInput("k", "Number of Clusters", value = 2, min = 2, max = 10)
      )
    } else if (input$method == "DBSCAN") {
      tagList(
        numericInput("eps", "Epsilon (Neighborhood Size)", value = 0.5, step = 0.1),
        numericInput("minPts", "Min Points", value = 5, min = 1)
      )
    }
  })
  
  # Reactive dataset processing
  data <- reactive({
    req(input$apply)  # Ensure clustering is only triggered on button click
    
    # Load dataset
    rawMalwareData <- read.csv(file = "FYPDataset2Portion.csv", sep = ",")
    malwareData <- rawMalwareData[1:373, ]  # Limit dataset to 373 rows
    
    # Shuffle dataset
    randomNum <- as.integer(runif(1, 1, 500))
    set.seed(randomNum)
    malwareData <- malwareData[sample(nrow(malwareData)), ]
    
    # Separate labels and features
    malwareDataNoLabel <- malwareData[-1]  # Remove label column
    correctClusters <- malwareData[, 1]  # Extract labels
    
    # Convert labels into numeric values (1 = Malicious, 2 = Benign)
    correctClusters <- ifelse(correctClusters == "malicious", 1, 2)
    
    # Scale the feature data
    malwareDataNoLabel <- scale(malwareDataNoLabel)
    
    return(list(features = malwareDataNoLabel, labels = correctClusters))
  })
  
  # Apply clustering based on selected method
  clusters <- eventReactive(input$apply, {
    dataProcessed <- data()
    df_no_label <- dataProcessed$features  # Get scaled features
    correct_clusters <- dataProcessed$labels  # Get correct labels
    
    # Perform Clustering
    if (input$method == "K-Means") {
      result <- kmeans(df_no_label, centers = input$k)
      cluster_assignment <- result$cluster
    } else if (input$method == "Hierarchical") {
      dist_matrix <- dist(df_no_label)
      hc <- hclust(dist_matrix, method = input$linkage)
      cluster_assignment <- cutree(hc, k = 2)  # Defaulting to 2 clusters
    } else if (input$method == "DBSCAN") {
      result <- dbscan(df_no_label, eps = input$eps, minPts = input$minPts)
      cluster_assignment <- result$cluster
    }
    
    list(correct = correct_clusters, assigned = cluster_assignment, df_no_label = df_no_label)
  })
  
  # Display clustering summary
  output$clusterSummary <- renderPrint({
    req(clusters())
    correct <- clusters()$correct
    assigned <- clusters()$assigned
    
    cat("Clustering Summary:\n")
    cat("Correct Labels:\n")
    print(table(correct))
    cat("\nAssigned Clusters:\n")
    print(table(assigned))
  })
  
  # Plot Clustering Results
  output$clusterPlot <- renderPlot({
    req(clusters())
    df_no_label <- clusters()$df_no_label
    assigned <- clusters()$assigned
    
    df_plot <- as.data.frame(df_no_label)
    df_plot$Cluster <- factor(assigned)
    
    ggplot(df_plot, aes(x = df_plot[,1], y = df_plot[,2], color = Cluster)) +
      geom_point(size = 3) +
      labs(title = paste(input$method, "Clustering Visualization"), x = "Feature 1", y = "Feature 2") +
      theme_minimal()
  })
}