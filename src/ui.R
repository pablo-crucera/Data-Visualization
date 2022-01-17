library(shiny)
library(shinydashboard)
library(chorddiag)
library(mapdeck)
library(leaflet)
library(shinyWidgets)

# TODO: Unify color palettes and justify their use

# Trips flow user interface
flowUI <- function(id) {
  ns <- NS(id)

  # TODO: What to do with Mapbox token?
  # Mapbox token
  key <- "pk.eyJ1IjoiamF2aWVnYWwiLCJhIjoiY2t5ZDU0NGo1MDEyMTMwcXBqOWxuaWQ1aSJ9.R8Jpo0pPpa8Ow46YQry_Wg"
  set_token(key)

  # Get week days
  styles <- c(
    "dark", "light", "outdoors", "streets", "satellite", "satellite-streets"
  )
  lc_time <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "en_US.UTF-8") # Needed to get week days in English
  week_days <- weekdays(seq(Sys.Date(), Sys.Date() + 6, by = "days"))
  Sys.setlocale("LC_TIME", lc_time)

  hours <- sprintf("%02d", 0:23)
  names(hours) <- sprintf("%02d:00-%02d:59", 0:23, 0:23)

  # FIXME: Mapdeck map is not always shown
  tabItem(
    tabName = "trips",
    sidebarLayout(
      sidebarPanel(
        width = 2,
        selectInput(ns("day"), h3("Week day"),
          choices = week_days, selected = "Monday"
        ),
        selectInput(ns("hour"), h3("Time"),
          choices = hours, selected = 17
        )
      ),
      mainPanel(
        width = 10,
        fluidRow(
          column(
            7, box(
              title = "Flow map", status = "primary",
              solidHeader = TRUE, height = "auto", width = NULL,
              mapdeckOutput(outputId = ns("map"), height = 655),
              fluidRow(
                column(
                  2, fluidRow(column(12,
                    align = "center",
                    actionBttn(ns("pitch_up"), size = "xs", style = "material-circle",
                      label = NULL,
                      icon = icon("angle-up"),
                    )
                  ), column(
                    6,
                    align = "right",
                    actionBttn(ns("bearing_left"), size = "xs", style = "material-circle",
                      label = NULL,
                      icon = icon("angle-left")
                    )
                  ), column(6,
                    align = "left",
                    actionBttn(ns("bearing_right"), size = "xs", style = "material-circle",
                      label = NULL,
                      icon = icon("angle-right")
                    )
                  ), column(12,
                    align = "center",
                    actionBttn(ns("pitch_down"), size = "xs", style = "material-circle",
                      label = NULL,
                      icon = icon("angle-down")
                    )
                  ))
                ),
                column(2, selectInput(ns("styles"), "Map style",
                  choices = styles,
                  selected = "streets"
                )),
                column(2, sliderInput(ns("width"),
                  label = "Width:",
                  min = 0, max = 100, value = 50, ticks = FALSE
                )),
                column(2, sliderInput(ns("length"),
                  label = "Length:",
                  min = 0, max = 500, value = 50, ticks = FALSE
                )),
                column(2, sliderInput(ns("speed"),
                  label = "Speed:",
                  min = 0, max = 500, value = 50, ticks = FALSE
                )),
                column(2, radioButtons(
                  ns("flow"), "Flow style",
                  c("Arcs", "Lines")
                ))
              )
            )
          ),
          column(
            5, box(
              title = "Chord diagram", status = "primary", solidHeader = TRUE,
              chorddiagOutput(ns("chord"), height = 800),
              width = NULL, height = "auto"
            )
          )
        )
      )
    )
  )
}

# Tips user interface
tipsUI <- function(id) {
  ns <- NS(id)

  # FIXME: Conditional panel with modules
  tabItem(
    tabName = "tips",
    sidebarLayout(
      sidebarPanel(
        width = 2,
        selectInput(ns("select"), h3("What to visualize?"),
          choices = c("Tips", "Trip length", "Disputes"),
          selected = "Trips"
        ),
        conditionalPanel(
          condition = "input[['{select}']] == 'Tips'",
          radioButtons(
            ns("PU_DO"), "Show tip values by:",
            c("Origin" = "orig", "Destination" = "dest")
          ),
          radioButtons(
            ns("MN_MD"), "Show:",
            c("Mean tip" = "mn", "Median tip" = "md")
          ), ns = ns
        ),
        conditionalPanel(
          condition = "input[['{select}']] == 'Trip length'",
          radioButtons(ns("PU_DO2"), "Show distance values by:",
            c("Origin" = "orig", "Destination" = "dest"),
            selected = "orig"
          ),
          radioButtons(
            ns("MN_MD2"), "Statistic:",
            c("Mean distance" = "mn", "Median distance" = "md")
          ), ns = ns
        ),
        conditionalPanel(
          condition = "input[['{select}']] == 'Disputes'",
          radioButtons(
            ns("PU_DO3"), "Show dispute percentage by:",
            c("Origin" = "orig", "Destination" = "dest")
          ), ns = ns
        )
      ),
      mainPanel(
        width = 10,
        box(
          title = "Choropleth map", status = "primary", height = "auto",
          solidHeader = TRUE, width = NULL,
          leafletOutput(ns("choropleth_map"), height = 800)
        )
      )
    )
  )
}

# Clustering user
clusteringUI <- function(id) {
  ns <- NS(id)

  plot_types <- c("cluster", "heatmap", "pairplot")
  names(plot_types) <- c("Clusters", "Heat map", "Pair plot")

  tabItem(
    tabName = "clustering",
    sidebarLayout(
      sidebarPanel(
        width = 2,
        selectInput(ns("plot_type"), h3("Type of plot"),
          choices = plot_types,
          selected = "cluster"
        ),
        sliderInput(ns("clusters"),
          label = "Number of clusters:",
          min = 1, max = 5, value = 3, ticks = FALSE
        )
      ),
      mainPanel(
        width = 10,
        box(
          title = "Clustering plot", status = "primary", height = "auto",
          solidHeader = TRUE, width = NULL,
          plotOutput(outputId = ns("cluster_plot"), height = 800)
        )
      )
    )
  )
}

shinyUI(dashboardPage(
  skin = "black",
  dashboardHeader(title = "NYC Taxi"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Trips flow", tabName = "trips", icon = icon("taxi")),
      menuItem("Tips", tabName = "tips", icon = icon("coins")),
      menuItem("Clustering",
        tabName = "clustering",
        icon = icon("project-diagram")
      )
    )
  ),
  dashboardBody(
    tabItems(
      flowUI("trips_flow"),
      tipsUI("tips"),
      clusteringUI("clustering")
    )
  )
))
