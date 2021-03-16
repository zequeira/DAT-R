extreme_outliers <- function(dataDF, outputFile) {
  
  # environment <- unique(data$envCond)[4]
  # dataDF <- data[data$envCond == environment, ]
    
  dataF_Wide <- dcast(dataDF, user ~ fileShort, value.var="rating", fun.aggregate = mean)
  dataF_WideIDs <- dcast(dataDF, user ~ fileShort, value.var="id", fun.aggregate = mean)
  
  # Create a variable/vector/collection of the column names you want to remove outliers on.
  varColumns <- names(dataF_Wide)
  varColumns <- varColumns[!varColumns %in% c('user')]
  varColumns <- sort(varColumns)
  
  # Create a variable to store the row id's to be removed
  Outliers <- c()
  Outliers_Users <- c()
  
  # Loop through the list of columns you specified
  for(i in varColumns){
    
    # Get the Min/Max values
    max <- quantile(dataF_Wide[,i],0.75, na.rm=TRUE) + (IQR(dataF_Wide[,i], na.rm=TRUE) * 3.0 )
    min <- quantile(dataF_Wide[,i],0.25, na.rm=TRUE) - (IQR(dataF_Wide[,i], na.rm=TRUE) * 3.0 )
    
    # Get the id's using which
    idx <- which(dataF_Wide[,i] < min | dataF_Wide[,i] > max)
    ids <- dataF_WideIDs[idx,i]
    outliers_users <- dataF_WideIDs[idx,'user']
    
    # Output the number of outliers in each variable
    # print(paste(i, length(idx), sep=' -> '))
    # print(i)
    # print(idx)
    
    # Append the outliers list
    Outliers <- c(Outliers, ids)
    Outliers_Users <- c(Outliers_Users, outliers_users)
  }
  print(sprintf("Extreme Outliers: %s: %d", unique(dataDF$envCond), length(Outliers)))
  
  write(sprintf("Extreme Outliers: %s: %d", unique(dataDF$envCond), length(Outliers)),
        outputFile, append=TRUE)
  
  Outliers_UsersDF <- data.frame(Outliers_Users)
  EOU <- count(Outliers_UsersDF, vars = Outliers_Users)
  
  print(sprintf("Extreme Outliers USERS at %s: ", unique(dataDF$envCond)))
  print(EOU)
  # for(z in EOU){print(z)}
  
  write(sprintf("Extreme Outliers USERS at %s: ", unique(dataDF$envCond)),outputFile, append=TRUE)
  write(capture.output(EOU), outputFile, append=TRUE)
  
  # Sort, I think it's always good to do this
  Outliers <- sort(Outliers)
  # Remove the outliers
  dataDF <- subset(dataDF, !id %in% Outliers)
  
  return(dataDF)
}