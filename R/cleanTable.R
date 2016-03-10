#Clean table
cleanTable <- function(df){
  
  #Remove empty columns
  df <- Filter(function(x)!all(is.na(x)), df)
  
  #replace NAs w/ " " 
  for (i in 1:length(df)){
    df[,i][is.na(df[,i])] <- " "
        
  }

df  
    
}
