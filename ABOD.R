

#install package HighDimOut

#ABOD
abod <- function(data, n){
  
  library("HighDimOut")
  
  #Remove Missing Values
  mydata <- data[complete.cases(data), ]
  
  #Calculation Angle Variance
  angle.var <- Func.ABOD(mydata, basic = TRUE)
  
  #Add angle.var to mydata
  mydata$angle.var <- angle.var

  #Present top N outliers
  top_outliers <- na.omit(mydata[order(mydata$angle.var, decreasing = FALSE) [1:n], ])

  print("ABOD : Angle-Based Outlier Detection")
  
  if (nrow(top_outliers)==0){
    
    print(paste0("No outliers!"))
    
  } else{
    
    print(paste0("There are: ", nrow(top_outliers)," Outliers"))   
    print(paste0("The list of Outliers is :"))
    print(top_outliers)
  }
  
}
#create dataframe
data <- data.frame(x = c(0.5, 0.5, 1, 1, 2, 0, 0.75),
                   y = c(1, 0.5, 1, 0.5, 2, 1.5, 0.75))
#First Parameter  : data
#Second Parameter  : most top outliers
abod(data, 4)





