ui <-  fluidPage(

  # Application title
  titlePanel(APP_NAME),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h4("Dosing details in ug/Kg BW/day"),
      numericInput("dose", label='Dose', value = 10,min=0, max=100)
    ),
 #############################plotting here
 mainPanel(
  plotOutput("v")
)
  )
)