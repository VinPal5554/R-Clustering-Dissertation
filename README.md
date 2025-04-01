# R-Clustering-Dissertation
This was my final year project at university where I explored how clustering techniques could be used to detect and categorise malware strains. By using a dummy malware dataset I aimed to explore the various techniques in order to predict new malware and have real-world application. The project during university was implemented as a command-line interface however post university I have come back to see and created a UI version via an interactive Shiny web application. 

# Features
- Preprocesses dataset (sanitising via normalising, converting labels into numeric format)
- Variety of clustering methods (K-Means, Hierarchical, DBSCAN, Robust)
- Adjustable parameters via dynamic UI (number of clusters, k value, epilon, etc)
- Data Visualisation (dendrograms, scatterplots, barcharts)

# Installation steps
**Ensure you have R and the appropriate packages:**
```
install.packages(c("shiny", "ggplot2", "cluster", "dbscan"))
```
 **Clone the repository:**
```
git clone https://github.com/VinPal5554/R-Clustering-Dissertation.git
```
  **Run the Shiny App (or CLI)**
```
library(shiny)
runApp("app.R")
```
 # Afterthoughts
 This was a fun project that brought back memories of my time at university. It is definitely rough around the edges (and still is) but it was something that I knew was going to be challenging and I gave it my all.
 
