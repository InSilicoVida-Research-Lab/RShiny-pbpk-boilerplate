source("src/global.R")
source("src/ui.R")
source("src/server_logic.R")

shinyApp(ui = ui, server = server)