install_required_packages <- function() {
    
    # List all required packages
    packages <- c(
        "shiny",
        "shinydashboard",
        "DT",
        "dplyr",
        "ggplot2",
        "readr",
        "tidyr",
        "plotly",
        "remotes"
    )
    
    # Function to install missing packages
    new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
    if(length(new_packages)) {
        install.packages(new_packages, repos='https://cran.rstudio.com/')
    }
    
    # Load all packages
    invisible(lapply(packages, library, character.only = TRUE))
}
