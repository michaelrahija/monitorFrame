#fetch_countries
#This function takes 1 parameter that's the directory of file
#it will return the overview table


fetch_countries <- function (path = NA){

  
  #countries
  countries = readNamedRegionFromFile(path, name= 'countries', header=TRUE)
  colnames(countries) <- c("country", "contact","email","status","comment")
  countries.t = subset(countries, country != "NA")
  
  
  #Code to implement table selection
  if (is.na(countries[1,1])){
    print("Countries not choosen yet!")}

  
  #filter columns for when there's information
  if (!is.na(countries[1,1])){
  id.na <- sapply(countries.t[1,],is.na)
  index <- length(countries.t)  - sum(id.na) 
  
  
  
  source('R/cleanTable.R')
  countries <- cleanTable(countries.t[,c(1,4)])
  
  }

}