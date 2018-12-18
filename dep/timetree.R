##                    timetree                           ##
##      This code is part of the timetree package        ##
## copyright F.-S. Krah 2015 (last update: 2015-04-15)   ##
timetree <- function(taxa){
  url <- paste("http://www.timetree.org/search/pairwise/", 
    taxa[1], "/", taxa[2], sep ="")
  parse <- readHTMLTable(url)
  u <- htmlParse(url)
  u <- unlist(xpathApply(u, '//p', xmlValue))
  if(length(parse)>0){
  names(parse) <- c("div", "ref")
  parse$div[,1] <- as.character(parse$div[,1])
  parse$div[,2] <- as.character(parse$div[,2])
  parse$div <- rbind(parse$div, names(parse$div))
  names(parse$div) <- c("estimate", "a")
  }
  if(length(parse)==0 ){parse <- gsub("\r\n\t\t\t\t\t|\r\n\t\t\t", "", u[[1]])}
  return(parse)
}



