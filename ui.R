library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(leaflet)
library(plotly)

dashboardPage(
    dashboardHeader(title="StravaR"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Activities", tabName = "data", icon = icon("dashboard")),
            menuItem("Mileage", tabName = "mileage", icon = icon("chart-line")),
            menuItem("Training", tabName = "fitness", icon = icon("heart")),
            menuItem("Routes", tabName = "routes", icon = icon("map")),
            uiOutput("activity_type_select_wrapper")
        )
    ),
    dashboardBody(
        useShinyjs(),
        tabItems(
            tabItem(tabName = "data",
                    fluidRow(
                            box(
                                title="Year's history",
                                withSpinner(plotlyOutput("calendar")),
                                solidHeader = TRUE,
                                width=6,
                                status="success"
                            ),
                            box(title = "Activities",
                                withSpinner(DTOutput("activities")),
                                solidHeader = TRUE,
                                width=6,
                                status="success"
                            ),
                    )
            ),
            tabItem(tabName = "mileage",
                    fluidRow(
                        box(title="Yearly mileage",
                            withSpinner(plotlyOutput("mileage_cumulative")),
                            solidHeader=TRUE,
                            status="success",
                            width=6
                        ),
                        box(title="Rolling mileage",
                            withSpinner(plotlyOutput("mileage_weekly")),
                            solidHeader=TRUE,
                            status="success",
                            width=6
                        )
                    )
            ),
            tabItem(tabName = "fitness",
                    fluidRow(
                        box(title="Training history",
                            withSpinner(plotlyOutput("training")),
                            status="success",
                            solidHeader = TRUE,
                            width=12)
                    )
            ),
            tabItem(tabName = "routes",
                    fluidRow(
                        column(
                            box(title="Routes",
                                withSpinner(leafletOutput("routes")),
                                status="success",
                                solidHeader = TRUE,
                                width=6),
                            width=12,
                            offset=3
                        )
                    )
            )
        )
    ),
    skin="yellow"
)