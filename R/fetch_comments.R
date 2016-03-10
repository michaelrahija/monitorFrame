#fetch_overview
#This function takes 1 parameter that's the directory of file
#it will return the overview table

library(XLConnect)

fetch_comments <- function (path = NA){

  
  comments = readNamedRegionFromFile(path, name= 'comments', header=TRUE)
  comments.t = subset(comments, Major.comments != "NA")
  comments.t = comments.t[1,1]
  
  if(is.na(comments.t)){
    comments.t = "NO COMMENTS"
    comments.t
  }
  
  if(!is.na(comments.t)){
    comments.t
  }


}