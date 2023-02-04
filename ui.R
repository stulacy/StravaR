library(shiny)
library(shinyjs)
library(shinyBS)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(leaflet)
library(plotly)

ui <- dashboardPage(
    dashboardHeader(title="StravaR",
                    tags$li(actionLink("refresh", label = "", icon = icon("arrows-rotate")),
                            class = "dropdown"),
                    tags$li(actionLink("settings", label = "", icon = icon("gear")),
                            class = "dropdown")
    ),
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
        uiOutput("redirect_js"),
        useShinyjs(),
        bsTooltip(id = "refresh", 
                  title = "Sync activities from Strava"),
        bsTooltip(id = "settings", 
                  title = "Update athlete and app settings"),
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