#' Conditional Package Installer
#'
#' This function conditionally installs packages when not installed on a compter.
#' @param packages A list of packages needed for script that are checked to see if on machine
#' @keywords packages
#' @export
#' @examples 
#' packages <- c("rgdal","mapview")
#' cond_packages(packages)


cond_packages <- function(packages) {
  
  # generate packages_installed a df representing packages already on machine.
  
  packages_installed <- as.data.frame( installed.packages()[,c(1,3:4)] )
  
  rownames( packages_installed ) <- NULL
  
  # using null value found in $Priority subsets to only packages downloaded
  
  packages_installed <- packages_installed[is.na(
    packages_installed$Priority),
    1:2,drop=FALSE]
  
  # make packages_index, a t/f index of packages on computer. true here is those not installed
  
  packages_index <- packages[!packages %in% as.character(packages_installed$Package)]
  
  # if packages_index length 0, means all are installed and libs can be loaded, else install em!
  
  if (identical( packages_index, character(0) )) { 
    
  } else { 
    
    install.packages( packages_index ) 
    
    }
  
  # install packages in package
  
  for(package in packages) {
    
    library( package, character.only = TRUE)
    
  } 
  
}


