#Mahalanobis Rule
MahalanobisRule <- function(data, percentile){
  
  #Remove Missing Values
  mydata <- data[complete.cases(data), ]
  
  #Mahalanobis distance
  mdist <- mahalanobis(mydata, colMeans(mydata), cov(mydata))
  print(mdist)
  #Calculation of critical value
  critical_value = qchisq(percentile, ncol(mydata))
  print(critical_value)
  #Detect Outliers
  outlier_index <- which(mdist>critical_value)
  outliersList <- mydata[outlier_index, ]
  
  print("Mahalanobis Rule")
  
  if (length(outliersList)==0){
    
      print(paste0("No outliers!"))
    
  } else{
    
      print(paste0("There are: ", nrow(outliersList)," Outliers"))   
      print(paste0("The list of Outliers is :"))
      print(outliersList)
  }
  
}

#create dataframe
data <- data.frame(Height = c(164, 167, 168, 168, 169, 169, 169, 170, 172, 173, 175, 176, 178),
                   Weight = c(55,   57,  58, 56,   57,  61,  61,  61,  64, 62,   56,  66,  70))


#First Parameter  : data
#Second Parameter : percentile
MahalanobisRule(data, 0.85)





