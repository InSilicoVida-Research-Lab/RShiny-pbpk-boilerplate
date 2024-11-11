install_required_packages <- function() {
    
    # List all required packages
    packages <- c(
        "deSolve",
        "coda",
        "rootSolve",
        "purrr",
        "tidyverse",
        "dplyr",
        "shiny"
    )
    
    # Function to install missing packages
    new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
    if(length(new_packages)) {
        install.packages(new_packages, repos='https://cran.rstudio.com/')
    }
    
    # Load all packages
    invisible(lapply(packages, library, character.only = TRUE))
}
