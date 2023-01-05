library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)

dashboardPage(
    dashboardHeader(title="StravaR"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Activities", tabName = "data", icon = icon("dashboard")),
            menuItem("Mileage", tabName = "mileage", icon = icon("th")),
            menuItem("Fitness", tabName = "fitness", icon = icon("th")),
            menuItem("Routes", tabName = "routes", icon = icon("th"))
        )
    ),
    dashboardBody(
        useShinyjs(),
        tabItems(
            tabItem(tabName = "data",
                    h2("Activities"),
                    fluidRow(
                        box(title = "Activities",
                            uiOutput("activity_type_table"),
                            DTOutput("activities"),
                            solidHeader = TRUE,
                            width=12,
                            status="success")
                    )
            ),
            tabItem(tabName = "mileage",
                    h2("Mileage"),
                    uiOutput("activity_type_mileage"),
                    fluidRow(
                        box(title="Yearly mileage",
                            plotOutput("mileage_cumulative"),
                            solidHeader=TRUE,
                            status="success",
                            width=6
                        ),
                        box(title="Rolling mileage",
                            plotOutput("mileage_weekly"),
                            solidHeader=TRUE,
                            status="success",
                            width=6
                        )
                    )
            ),
            tabItem(tabName = "fitness",
                    h2("Fitness"),
                    fluidRow(
                        box(title="Training status",
                            plotOutput("training"),
                            status="success",
                            solidHeader = TRUE,
                            width=12)
                    )
            ),
            tabItem(tabName = "routes",
                    h2("Widgets tab content")
            )
        )
    ),
    skin="yellow"
)