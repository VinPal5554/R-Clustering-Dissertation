#install.packages("testthat")
library(testthat)

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

WeightedKappa <- function(cluster1, cluster2)
{
  
  n = length(cluster1)
  a1 = 0
  a2 = 0
  a3 = 0
  a4 = 0
  
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      if (cluster1[i] == cluster1[j])
      {   match1=1  }
      else
      {   match1=0 }
      if (cluster2[i] == cluster2[j])
      {   match2=1  }
      else
      {   match2=0 }
      
      if (match1==1 & match2==1) a1=a1+1
      if (match1==1 & match2==0) a2=a2+1
      if (match1==0 & match2==1) a3=a3+1
      if (match1==0 & match2==0) a4=a4+1
    }
  }
  
  n1 = a1 + a3
  n2 = a2 + a4
  f1 = a1 + a2
  f2 = a3 + a4
  
  wkappa = (2 * (a1 * a4 - a2 * a3) ) / (n1 * f2 + n2 * f1)
  
}


test_that("Checking if kappa strength produces correct evaluation",
          
          {
            expect_equal(weightedKappaStrength(0.5), "MODERATE")
            expect_equal(weightedKappaStrength(0.1), "POOR")
            
          }
          
          )

test_that("Checking if weighted kappa produces value of 1",
          
          {
            
            cluster1 = c(1, 2, 3)
            cluster2 = c(1, 2, 3)
            
            expect_equal(WeightedKappa(cluster1, cluster2), 1)
          
          }
          
          )











