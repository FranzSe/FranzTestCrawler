#Function to load necessary packages only if needed
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

pkgTest("cronR")
pkgTest('miniUI')
pkgTest('shiny')
pkgTest('shinyFiles')