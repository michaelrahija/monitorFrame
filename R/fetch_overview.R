#fetch_overview
#This function takes 1 parameter that's the directory of file
#it will return the overview table

fetch_overview <- function (path = NA){

  
#import table from notebook
pipe <- readNamedRegionFromFile(path, name= 'table', header=TRUE)
  


##--ID VERSION OF NOTEBOOK (version 1 w/o revised contract date)
test <- grepl("Revised",colnames(pipe))
  
  if(sum(test) == 1){
    version = 2 
  } else if (sum(test) == 0){
    version = 1
  } else {
    stop("Error in colnames. Can't ID version.")  
  }
  

  ##--GET VERSION 1 TABLE
  if(version == 1){
    
    #id relevant columns
    status = grep("status", colnames(pipe),
                  ignore.case = TRUE)
  
    contract  = grep("due", colnames(pipe),
                    ignore.case = TRUE)
  
    actual = grep("actual", colnames(pipe),
                  ignore.case = TRUE)
  
    deliverable = grep("deliverable", colnames(pipe),
                       ignore.case = TRUE)
  
    pipe.t <- pipe[,c(status, contract, actual, deliverable)]
    
    #clean table up
    colnames(pipe.t) <- c("Status", "Contract.due.date", "Actual.date", "Task/Deliverable")
    pipe.t$Contract.due.date <-gsub("\\ 00:00:00","\\",pipe.t$Contract.due.date)
    pipe.t$Actual.date <-gsub("\\ 00:00:00","\\",pipe.t$Actual.date)
  
    #replace na's
    pipe.t$Status[is.na(pipe.t$Status)] <- " "
    pipe.t$Actual.date <- as.character(pipe.t$Actual.date)
    pipe.t$Actual.date[is.na(pipe.t$Actual.date)] <- " "
    pipe.t$Contract.due.date <- as.character(pipe.t$Contract.due.date)
    pipe.t$Contract.due.date[is.na(pipe.t$Contract.due.date)] <- " "

  }  
  
  
  ##--GET VERSION 2 TABLE
  if (version ==2) {
    
  #--Find revised contract column
  id <- grep("revised", colnames(pipe),
              ignore.case = TRUE)
    
  
  test.revised <- nrow(pipe) == sum(is.na(pipe[,id]))
  
    #--If revised column isn't blank, use "Revised Due Date". 
    if(!test.revised){
     
      contract  = grep("revised", colnames(pipe),
                      ignore.case = TRUE)
      cols <- c("Status", "Revised.due.date", "Actual.date", "Task/Deliverable")
    
    #--If revised column has info, use it.
    } else if (test.revised){
      
      contract  = grep("due", colnames(pipe),
                        ignore.case = TRUE)
      cols <- c("Status", "Contract.due.date", "Actual.date", "Task/Deliverable")
    } else {
      stop("Error: Can't determine if Revised column contains info.")
    }
  
  
  #--Get other columns and consolidate in 1 frame
  status = grep("status", colnames(pipe),
                ignore.case = TRUE)
  
  actual = grep("actual", colnames(pipe),
                ignore.case = TRUE)
  
  deliverable = grep("deliverable", colnames(pipe),
                     ignore.case = TRUE)
  
  pipe.t <- pipe[,c(status, contract, actual, deliverable)]
  
  colnames(pipe.t) <- cols
  
  #--Clean up date columns
  dates <- grep("date", colnames(pipe.t),
                ignore.case = TRUE)
  
    for(i in dates){
      pipe.t[,i] <- gsub("\\ 00:00:00","\\",pipe.t[,i])
      pipe.t[,i] <- as.character(pipe.t[,i])
      pipe.t[,i][is.na(pipe.t[,i])] <- " "
    
    }

  #--Clean up status
  pipe.t$Status[is.na(pipe.t$Status)] <- " "

    
  }

#get rid of blank rows for deliverables
pipe.t <- pipe.t[!(is.na(pipe.t[,4])),]

  if(sum(is.na(pipe.t[1,])) ==  ncol(pipe.t)){
    pipe.t <- print("Workplan not entered")
    
  }
  
#return table
pipe.t




}