ui <- dashboardPage(
    dashboardHeader(title = APP_NAME),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Data", tabName = "data", icon = icon("table"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                fluidRow(
                    box(plotOutput("plot1")),
                    box(plotOutput("plot2"))
                )
            ),
            tabItem(tabName = "data",
                fluidRow(
                    box(DTOutput("table"), width = 12)
                )
            )
        )
    )
)