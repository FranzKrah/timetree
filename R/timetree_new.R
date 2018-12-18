#' Retrieve records from the timetree website
#' @param taxon1 character string specifying the first taxon name 
#' @param taxon2 character string specifying the second taxon name 
#' 
#' @import RSelenium httr RCurl
#'
#' @author Franz-Sebastian Krah
#'
#' @examples
#' \dontrun{
#' timetree(taxon1 = "Agaricus", taxon2 = "Boletus")
#' }
#' @export


timetree <- function(taxon1 = "Agaricus",
                     taxon2 = "Boletus",
                     verbose = TRUE,
                     screenshot = TRUE,
                     port = 4445L,
                     browserName = "chrome",
                     remoteServerAddr = "localhost",
                     wait = 2) {
  
  
  # test internet conectivity
  if(!url.exists("r-project.org") == TRUE)
    stop( "Not connected to the internet. Please create a stable connection and try again." )
  if(!is.character(getURL("http://timetree.org")))
    stop(" Database is not available : http://timetree.org")
  
  if(missing(taxon1) | missing(taxon1))
    stop("Two species names need to be specified!")
  
  wait <- ifelse(wait<=2, 2, wait)
  
  # Initialize session -----------------------------------------------------
  
  start_docker(verbose = TRUE)
  
  ## Set up remote
  dr <- remoteDriver(remoteServerAddr = "localhost", port = port, browserName = browserName)
  Sys.sleep(wait)
  
  ## Open connection; run server
  out <- capture.output(dr$open(silent = FALSE))
  Sys.sleep(2)
  if(verbose > 1)
    cat(out)
  
  if(dr$getStatus()$ready)
    cat(dr$getStatus()$message[1], "\n")
  if(!dr$getStatus()$ready)
    stop("Remote server is not running \n Please check if Docker is installed!")
  
  
  # Open Website -----------------------------------------------------------
  cat(ifelse(verbose, "Open website\n", ""))
  ## Navigate to website
  
  dr$navigate("http://timetree.org")
  Sys.sleep(wait+1)
  
  ## Enter user query ------------------------------------------------------------------
  cat(ifelse(verbose, "Send user query to website:\n", ""))
  
  ## Fill elements: user defined query input
  
  ## Taxon 1
  webElem <- dr$findElement('id', "taxon-a")
  webElem$sendKeysToElement(list(taxon1))
  
  
  ## Taxon 2
  webElem <- dr$findElement('id', "taxon-b")
  webElem$sendKeysToElement(list(taxon2))
  
  # Press Enter -----------------------------------------------------
  webElem$sendKeysToElement(list(key = "enter"))
  Sys.sleep(wait+2)
  
  if(screenshot)
    dr$screenshot(display = TRUE, useViewer = TRUE)
  
  
  # Results  --------------------------------
  
  x <- dr$getPageSource()[[1]]
  x <- read_html(x)
  x <- html_table(x)[[1]]
  
  res <- list(studies = x, median_time = median(x$Time))
  
  # Close Website and Server ------------------------------------------------
  cat(ifelse(verbose, "Close website and quit server\n", ""))
  
  ## Close Website
  dr$close()
  
  ## Stop docker
  stop_docker()
  
  ## Return downloaded query results as list
  return(res)
}
