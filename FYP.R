# This script investigates how different clustering algorithms can be used to group malware
# The algorithms that I have implemented are: Hierarchical, K-Means, DBSCAN, and a robust/consensus clustering prototype
# In order to assume the accuracy of clusters, a weighted kappa metric will be used

library(cluster)
library(fpc)
library(dbscan)
library(factoextra)
library(mise)
library(dendextend)

numOfClusters = ""
startAgain = 0

# Weighted kappa to measure agreement of algorithm clusters

WeightedKappa <- function(clusterArrangement1, clusterArrangement2) {

  clusterArrangementLength = length(clusterArrangement1)
  a1 = 0
  a2 = 0
  a3 = 0
  a4 = 0
  
  for (i in 1:clusterArrangementLength) {
  
    for (j in 1:clusterArrangementLength) {
    
      if (clusterArrangement1[i] == clusterArrangement1[j]) {
        
        match1 = 1  
        
      } else {
        
         match1 = 0 
         
      }
      
      if (clusterArrangement2[i] == clusterArrangement2[j]) {
        
         match2 = 1  
         
      } else {
        
         match2=0 
         
      }
      
      if (match1==1 & match2==1) {
        
        a1=a1+1
      }
      
      if (match1==1 & match2==0) {
        
        a2=a2+1 
      }
      
      if (match1==0 & match2==1) {
        
        a3=a3+1
      }
      
      if (match1==0 & match2==0) {
        
        a4=a4+1 
      }
      
    }
  }
  
  n1 = a1 + a3
  n2 = a2 + a4
  f1 = a1 + a2
  f2 = a3 + a4
  
  weightedKappa = (2 * (a1 * a4 - a2 * a3) ) / (n1 * f2 + n2 * f1)
  
}

# Logic to translate Weighted Kappa to English

weightedKappaStrength <- function(myKappa) {
  
  kappaStrength = ""
  
  if((myKappa <= 0.0) && (myKappa >= -1.0)) {
    
    kappaStrength = "VERY POOR"
    
  } else if ((myKappa <= 0.2) && (myKappa > 0.0)) {
    
    kappaStrength = "POOR"
    
  } else if ((myKappa <= 0.4) && (myKappa > 0.2)) {
    
    kappaStrength = "FAIR"
    
  } else if ((myKappa <= 0.6) && (myKappa > 0.4)) {
    
    kappaStrength = "MODERATE"
    
  } else if ((myKappa <= 0.8) && (myKappa > 0.6)) {
    
    kappaStrength = "GOOD"
    
  } else if ((myKappa <= 1.0) && (myKappa > 0.8)) {
    
    kappaStrength = "VERY GOOD"
    
  }
  
}


# Default algorithm used for robust clustering 

hClustering <- function() {
  
  euclideanDistances = dist(malwareDataNoLabel, method = "euclidean")
  myDendrogram = hclust(euclideanDistances, method = "ward.D")
  hClusters = cutree(myDendrogram, k = numOfClusters)
  
}

# Hierarchical clustering algorithm

hClusteringDetailed <- function() {
 
  distanceMetric = dist(malwareDataNoLabel, method = "euclidean")
  notChosenLinkage = TRUE

  while(notChosenLinkage) {
    
    cat("Please choose a linkage method (1 - single, 2 - complete, 3 - average, 4 - ward)\n")
    my.linkage = readline()
    
    if(my.linkage == "1") {
    
      myDendrogram = hclust(distanceMetric, method = "single")
      my.linkage = "single"
      notChosenLinkage = FALSE
    
    } else if (my.linkage == "2") {
    
      myDendrogram = hclust(distanceMetric, method = "complete")
      my.linkage = "complete"
      notChosenLinkage = FALSE
    
    } else if (my.linkage == "3") {
    
      myDendrogram = hclust(distanceMetric, method = "average")
      my.linkage = "average"
      notChosenLinkage = FALSE
    
    } else if (my.linkage == "4") {
    
      myDendrogram = hclust(distanceMetric, method = "ward.D")
      my.linkage = "ward"
      notChosenLinkage = FALSE
    
    } else {
    
      cat("\n")
      cat("Invalid linkage method\n")
      cat("\n")
      
    }
  }
  
  startTime = Sys.time() 
  
  hClusters = cutree(myDendrogram, k = numOfClusters)
  hClusters = as.numeric(hClusters)
  
  weightedKappaa = WeightedKappa(hClusters, correctClusters)
 
  cat("Algorithm: Hierarchical Clustering\n")
  cat(paste("Number of clusters: ", numOfClusters))
  cat("\n")
  cat(paste("Linkage: ", my.linkage))
  cat("\n")
  cat(paste("Weighted Kappa: ", weightedKappaa))
  cat("\n")
  weightedStrength = weightedKappaStrength(weightedKappaa)
  cat(paste("Weighted Kappa Strength: ", weightedStrength))
  cat("\n\n")
  
  endTime = Sys.time()
  timeDifference = endTime - startTime
  
  print(timeDifference)
  
  
  notChosenGraphChoice = TRUE
  
  while(notChosenGraphChoice) {
  
    cat("Would you like to view graphical representations of the data? (Yes or No)\n")
    my.graphChoice = readline()
  
    if(my.graphChoice == "Yes") {
  
      colourDendrogram = color_branches(myDendrogram, k = 2)
      plot(colourDendrogram, main="Dendrogram")
      clusplot(malwareDataNoLabel, hClusters, color=TRUE, shade=TRUE, labels=2, lines = 0)
      notChosenGraphChoice = FALSE
    
    } else if(my.graphChoice == "No") {
      
      notChosenGraphChoice = FALSE
    
    } else {
    
      cat("\n")
      cat("Invalid command\n")
      cat("\n")
    
    }
  
  }
  choice = my.graphChoice
}

# Default algorithm used for robust clustering 

kClustering <- function() {
  
  fit = kmeans(malwareDataNoLabel, numOfClusters)
  kClusters = fit$cluster
  
}

# K-Means clustering algorithm

kClusteringDetailed <- function() {
  
  notChosenClusterNumber = TRUE
  
  while(notChosenClusterNumber) {
    
    cat("Please choose a value for the number of clusters\n")
    my.clusterNumber = readline()
    
    myClusterNumber = as.integer(my.clusterNumber)
    
    for(i in 1:99) {
      
      if(i == myClusterNumber) {
        
        notChosenClusterNumber = FALSE
        
      }
    }
  
  }
  
  startTime = Sys.time()
  
  fit = kmeans(malwareDataNoLabel, myClusterNumber)
  kClusters = fit$cluster
  weightedkappaKDetailed = WeightedKappa(kClusters, correctClusters)
  
  cat("Algorithm: K-Means Clustering\n")
  cat(paste("Number of correct clusters: ", numOfClusters))
  cat("\n")
  cat(paste("Number of chosen clusters: ", myClusterNumber))
  cat("\n")
  cat(paste("Weighted Kappa: ", weightedkappaKDetailed))
  cat("\n")
  weightedStrength = weightedKappaStrength(weightedkappaKDetailed)
  cat(paste("Weighted Kappa Strength: ", weightedStrength))
  cat("\n\n")
  
  endTime = Sys.time()
  
  timeDifference = endTime - startTime
  
  print(timeDifference)
  
  notChosenGraphChoice = TRUE
  
  while(notChosenGraphChoice) {
  
    cat("Would you like to view graphical representations of the data? (Yes or No)")
    my.graphChoice = readline()
  
    if(my.graphChoice == "Yes") {
    
      plot(fviz_nbclust(malwareDataNoLabel, kmeans, method = "wss"))
      clusplot(malwareDataNoLabel, kClusters, color=TRUE, shade=TRUE, labels=2, lines = 0)
      notChosenGraphChoice = FALSE
    
    } else if(my.graphChoice == "No") {
      
      notChosenGraphChoice = FALSE

    } else {
    
      cat("\n")
      cat("Invalid command\n")
      cat("\n")
    
    }
  
  }
  
  choice = my.graphChoice
  
}

# Default algorithm used for robust clustering 

DBSCANclustering <- function() {
  
  set.seed(123456789)
  db = fpc::dbscan(malwareDataNoLabel, eps = 5, MinPts = 4)
    
}

# DBSCAN clustering algorithm

DBSCANclusteringDetailed <- function() {
  
  set.seed(123456789)
  cat("PLEASE NOTE: An eps value too small will identify sparse clusters as noise\n")
  cat("PLEASE NOTE: An eps value too big may cause denser clusters to be merged together\n\n")
  epsNotValid = TRUE
  minPtsNotValid = TRUE
  optimalEps = 5
  optimalMinsPts = 4
  
  while(epsNotValid || minPtsNotValid) {
    
      cat(paste("Please choose a value for eps, optimal calculated eps is ~", optimalEps))
      my.eps = readline()
      myEps = as.integer(my.eps)
      cat(paste("Please choose a value for minPts, recommended minPts is ", optimalMinsPts))
      my.minPts = readline()
      myMinPts = as.integer(my.minPts)
    
      # Parameters need to be within the limit, in this case set from 1 to 99
      
      for(i in 1:99) {
        
        if(i == myEps) {
          epsNotValid = FALSE
          
        }
        if(i == myMinPts) {
          minPtsNotValid = FALSE
        }
        
      }
      
      # If only 1 parameter is valid, combination is automatically invalid again
      
      if((epsNotValid == FALSE && minPtsNotValid == TRUE) || (epsNotValid == TRUE && minPtsNotValid == FALSE)) {
        
        epsNotValid = TRUE
        minPtsNotValid = TRUE
        cat("Invalid parameter combination, please try again\n\n")
        
      } else if((epsNotValid == TRUE && minPtsNotValid == TRUE) || (epsNotValid == TRUE && minPtsNotValid == TRUE)) {
        
        cat("Invalid parameter combination, please try again\n\n")
        
      }
    }
    
  startTime = Sys.time()
  
  my.eps = as.numeric(my.eps)
  my.minPts = as.numeric(my.minPts)
  db = fpc::dbscan(malwareDataNoLabel, eps = my.eps, MinPts = my.minPts)
  
  # Filter noise prediction and convert it to an actual class
  
   for(i in 1:length(db[["cluster"]])) {
    
    if(db[["cluster"]][i] == 0) {
      
      # Noise will be randomly assigned a valid cluster - this is better than a noise prediction
      
      numOfClustersInt = as.integer(numOfClusters)
      numOfClustersArray = seq(from = 1, to = numOfClustersInt, by = 1)
      randomNum = as.integer(runif(1, 1, numOfClustersInt))
      db[["cluster"]][i] = numOfClustersArray[randomNum]
    }
   }
  
  weightedKappaDBSCAN = WeightedKappa(db[["cluster"]], correctClusters)
  cat("Algorithm: DBSCAN\n")
  cat(paste("Number of clusters: ", numOfClusters))
  cat("\n")
  cat(paste("Radius size: ", my.eps))
  cat("\n")
  cat(paste("Minimum points: ", my.minPts))
  cat("\n")
  cat(paste("Weighted Kappa: ", weightedKappaDBSCAN))
  cat("\n")
  weightedStrength = weightedKappaStrength(weightedKappaDBSCAN)
  cat(paste("Weighted Kappa Strength: ", weightedStrength))
  cat("\n\n")
  
  endTime = Sys.time()
  
  timeDifference = endTime - startTime
  
  print(timeDifference)
  
  
  notChosenGraphChoice = TRUE
  
  while(notChosenGraphChoice) {
  
    cat("Would you like to view graphical representations of the data? (Yes or No)")
    my.graphChoice = readline()
  
    if(my.graphChoice == "Yes") {
    
      kNNdistplot(malwareDataNoLabel, k = 2)
      abline(h = my.eps, lty = 2)
      clusplot(malwareDataNoLabel, db[["cluster"]], color=TRUE, shade=TRUE, labels=2, lines = 0)
      notChosenGraphChoice = FALSE
    
    } else if(my.graphChoice == "No") {
      
      notChosenGraphChoice = FALSE
      
      
    } else {
      
      cat("\n")
      cat("Invalid command\n")
      cat("\n")
    }
  }
  choice = my.graphChoice
}

while(startAgain == 0) {

    cat("Investigating cybersecurity potential of clustering - By Vinay Pal\n\n")

    rawMalwareData = read.csv(file = "FYPDataset2Portion.csv", sep = ",")
    malwareData = rawMalwareData[1:373, ]
  
    randomNum = as.integer(runif(1, 1, 500))
  
    set.seed(randomNum)
    rows = sample(nrow(malwareData))
    malwareData = malwareData[rows, ]
  
  
    malwareDataNoLabel = malwareData[-1]
    correctClusters = malwareData[ , 1]
    numOfClusters = "2"
    numOfClusters = as.numeric(numOfClusters)
  
 
    # Change cluster identifications into something comparable
  
    for(i in 1:length(correctClusters)) {
    
      if(correctClusters[i] == "malicious") {
      
        correctClusters[i] = 1
      
      } else {
      
        correctClusters[i] = 2
      }
    
    }
  
    correctClusters = as.numeric(correctClusters)
    malwareDataNoLabel = scale(malwareDataNoLabel)
  

    cat("What algorithm would you like to use? (1 - Hierarchical, 2 - K-means, 3 - DBSCAN, 4 - All + aggregate clustering)\n")
    my.algorithmChoice = readline()

    if(my.algorithmChoice == "1") {
  
      hierarchicalClusteringAlgorithm = hClusteringDetailed()
  
    } else if(my.algorithmChoice == "2") {
  
      kMeansClusteringAlgorithm = kClusteringDetailed()
  
    } else if (my.algorithmChoice == "3") {
  
      DBSCANClusteringAlgorithm = DBSCANclusteringDetailed()

  
    } else if (my.algorithmChoice == "4") {
      
      startTime = Sys.time()
  
      robustVector = seq(from=1, to= length(correctClusters), by=1)
  
      kMeans = kClustering()
      hierarchical = hClustering()
      dbscan = DBSCANclustering()
    
      # Filter noise prediction and convert it to an actual class
    
      for(i in 1:length(dbscan[["cluster"]])) {
      
        if(dbscan[["cluster"]][i] == 0) {
        
          # Noise will be randomly assigned a valid cluster - this is better than a noise prediction
        
          numOfClustersInt = as.integer(numOfClusters)
        
          numOfClustersArray = seq(from = 1, to = numOfClustersInt, by = 1)
        
          randomNum = as.integer(runif(1, 1, numOfClustersInt))
        
          dbscan[["cluster"]][i] = numOfClustersArray[randomNum]
        
        }
      
      }
    
    
      # The majority prediction gets accepted - agreement must be the consensus
  
      for(i in 1:length(correctClusters)) {
    
        if(kMeans[i] == hierarchical[i] && kMeans[i] == dbscan[["cluster"]][i]) {
      
          robustVector[i] = kMeans[i]
      
        } else if(kMeans[i] == hierarchical[i] && kMeans[i] != dbscan[["cluster"]][i]){
      
          robustVector[i] = kMeans[i]
      
        } else if(kMeans[i] == dbscan[["cluster"]][i] && kMeans[i] != hierarchical[i]){
      
          robustVector[i] = kMeans[i]
      
        } else if(hierarchical[i] == dbscan[["cluster"]][i] && hierarchical[i] != kMeans[i]){
      
          robustVector[i] = hierarchical[i]
      
        } else if(hierarchical[i] != dbscan[["cluster"]][i] && hierarchical[i] != kMeans[i] && dbscan[["cluster"]][i] != kMeans[i]){
      
          # randomly select from either of the 3 predictions
          # tempVector contains 3 conflicting predictions
      
          tempVector = as.integer(c(kMeans[i], hierarchical[i], dbscan[["cluster"]][i]))
        
          randomNum = as.integer(runif(1, 1, 3))
      
          if(randomNum == as.integer(1)){
        
              robustVector[i] = kMeans[i]
        
          } else if (randomNum == as.integer(2)) {
        
              robustVector[i] = hierarchical[i]
        
          } else if (randomNum == as.integer(3)) {
        
              robustVector[i] = dbscan[["cluster"]][i]
        
          }
      
      
        }
    
    
      }
  
      weightedKappa1 = WeightedKappa(hierarchical, correctClusters)
      kappaStrength1 = weightedKappaStrength(weightedKappa1)
                            
      weightedKappa2 = WeightedKappa(kMeans, correctClusters)
      kappaStrength2 = weightedKappaStrength(weightedKappa2)
    
      weightedKappa3 = WeightedKappa(dbscan[["cluster"]], correctClusters)
      kappaStrength3 = weightedKappaStrength(weightedKappa3)
    
      weightedKappa4 = WeightedKappa(robustVector, correctClusters)
      kappaStrength4 = weightedKappaStrength(weightedKappa4)
    
      cat("Algorithm: Hierarchical Clustering")
      cat("\n")
      cat(paste("Weighted Kappa: ", weightedKappa1))
      cat("\n")
      cat(paste("Agreement strength: ", kappaStrength1))
      cat("\n\n")
    
      cat("Algorithm: K-Means Clustering")
      cat("\n")
      cat(paste("Weighted Kappa: ", weightedKappa2))
      cat("\n")
      cat(paste("Agreement strength: ", kappaStrength2))
      cat("\n\n")
    
      cat("Algorithm: DBSCAN")
      cat("\n")
      cat(paste("Weighted Kappa: ", weightedKappa3))
      cat("\n")
      cat(paste("Agreement strength: ", kappaStrength3))
      cat("\n\n")
    
      cat("Algorithm: Robust Clustering")
      cat("\n")
      cat(paste("Weighted Kappa: ", weightedKappa4))
      cat("\n")
      cat(paste("Agreement strength: ", kappaStrength4))
      cat("\n\n")
      
      endTime = Sys.time()
      
      timeDifference = endTime - startTime
      
      print(timeDifference)
    
      cat("Would you like to view further analytics for robust clustering? (Yes or No)")
    
      my.graphChoice = readline()
    
      if(my.graphChoice == "Yes") {
      
        robustComparison = c(weightedKappa1, weightedKappa2, weightedKappa3, weightedKappa4)
        barPlotNames = c("Hierarchical", "K-Means", "DBSCAN", "Robust")
        barplot(robustComparison, names.arg = barPlotNames, xlab = "Clustering Type", ylab = "Weighted Kappa", col = "green")
        clusplot(malwareDataNoLabel, robustVector, color=TRUE, shade=TRUE, labels=2, lines = 0)
      
      }
    
      choice = my.graphChoice
  
  
    } else {
  
      cat("\n")
      cat("Please choose a valid algorithm\n")
      cat("\n")
      next
    
    }

    notRestarted = TRUE
  
    while(notRestarted) {
  
      cat("Would you like to start again? (Yes or No)\n")

      my.startChoice = readline()

      if(my.startChoice == "Yes") {
  
        startAgain = 0
        notRestarted = FALSE
  
      } else if(my.startChoice == "No"){
  
        startAgain = 1
        notRestarted = FALSE
      
      } 
      
    }

    # Cleans console and graphs when user starts again or quits

    if(my.algorithmChoice == "1") {
  
      if(hierarchicalClusteringAlgorithm == "Yes") {
    
        mise(vars = FALSE, figs = FALSE)
        dev.off(dev.list()["RStudioGD"])
    
      } else {
    
        mise(vars = FALSE, figs = FALSE)
    
      }
  
    } else if(my.algorithmChoice == "2") {
  
      if(kMeansClusteringAlgorithm == "Yes") {
    
        mise(vars = FALSE, figs = FALSE)
        dev.off(dev.list()["RStudioGD"])
    
      } else {
    
        mise(vars = FALSE, figs = FALSE)
    
      }
  
    } else if(my.algorithmChoice == "3") {
  
      if(DBSCANClusteringAlgorithm == "Yes") {
    
        mise(vars = FALSE, figs = FALSE)
        dev.off(dev.list()["RStudioGD"])
    
      } else {
    
        mise(vars = FALSE, figs = FALSE)
    
      }
  
    } else if(my.algorithmChoice == "4") {
  
      if(choice == "Yes") {
    
        mise(vars = FALSE, figs = FALSE)
        dev.off(dev.list()["RStudioGD"])
    
      } else {
    
        mise(vars = FALSE, figs = FALSE)
    
      }
  
    }

  }







