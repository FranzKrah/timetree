##                    author.search                      ##
##      This code is part of the timetree package        ##
## copyright F.-S. Krah 2015 (last update: 2015-04-15)   ##
author.search <- function(author){
  url <- paste("http://www.timetree.org/search/author/",
               author, sep="")
  parse <- readHTMLTable(url)
  if (length(parse)==0) { obj <- "No molecular data available for this query"}
  else {obj <- data.frame(parse[[1]])}
  return(obj)
}
