betsy = function(yfile){
  
  
  #Check the data size.If it is too large stop function then ,else continue to import data.
  if(file.info(yfile)$size >= 1700000000){stop("Please check the size of your csv data file. BETSY does not allow uploading of files over 1.5 GB.")
  }
  
  #' @to read csv data to convert clarion file format
  
  reading=read.csv(yfile, header = TRUE, sep = ",", quote = "\"",
                   dec = ".", fill = TRUE) 
  
  #Extract the infinite value +/- from the entered data and change it to 0.
  #Extract the NaN value from the entered data and change it to 0.
  reading[reading == Inf | reading == -Inf ] <- 0
  reading[reading == NA] <- 0
  
  
  data<- data.table::as.data.table(reading, keep.rowname = "id")
  
  
  
  # @if the number of rows of the data is too large, the level of the metadata is determined by controlling only the first 1000 rows for the number of rows that control the data more than 1000 while determining the level type to prevent slowness of the query.
  
  
  if(nrow(reading) >= 1000){
    
    # define vector to collect metadatalevel
    
    my_vector <- vector()
    for(i in 1:ncol(reading))
    {
      
      a=reading[[100,i]]
      test <- all.equal(a, as.numeric(a), check.attributes = FALSE)
      if(test == TRUE){ my_vector[[i+1]] <- 1 }
      else { my_vector[[i+1]] <-2 } 
      
      # label vector elements to enter the minimum element required by the level of the metadata
      
      
      my_vector[[1]]<-2
      my_vector_f = gsub(2, "feature", my_vector)
      my_vector_fs = gsub(1, "sample", my_vector_f)
    }
  }
  
  #' @return TRUE/FALSE on whether a number is numeric or not.
  
  else{
    my_vector <- vector()
    for(i in 1:ncol(reading))
    {
      a=reading[[i]]
      test <- all.equal(a, as.integer(a), check.attributes = FALSE)
      if(test == TRUE){ my_vector[[i+1]] <- 1 }
      else { my_vector[[i+1]] <-2 } 
      
      my_vector[[1]]<-2
      my_vector_f = gsub(2, "feature", my_vector)
      my_vector_fs = gsub(1, "sample", my_vector_f)
    }
  }
  
  # define metadata
  
  metadata <- data.table::data.table(names(data), level = my_vector_fs )
  
  # define the key element to cover the headers in the user's dataset as required by the metadata
  
  names(metadata)[1] <- "key"
  
  #define clarion format to convert data into clarion format
  
  clarion <- Clarion$new(data = data, metadata = metadata)
  
  # Return the elements of the metadata to the user to ensure that the elements of the metadata are correctly defined.
  
  
  return(metadata)
  
  
  
}
