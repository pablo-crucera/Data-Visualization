library(shiny)
library(shinydashboard)
library(chorddiag)
library(mapdeck)
library(leaflet)

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
  
  tabItem(
    tabName = "trips",
    fluidRow(
      column(
        2, box(
          title = "Time and week day", status = "primary", height = "auto",
          solidHeader = TRUE, width = NULL,
          selectInput(ns("day"), h3("Week day"),
            choices = week_days, selected = "Monday"
          ),
          selectInput(ns("hour"), h3("Time"),
            choices = hours, selected = 17
          )
        )
      ),
      column(
        6, box(
          title = "Flow map", status = "primary",
          solidHeader = TRUE, height = "auto", width = NULL,
          # FIXME: Mapdeck not working with modules
          mapdeckOutput(outputId = ns("map"), height = 650),
          box(fluidRow(
            column(
              2, fluidRow(column(12,
                align = "center",
                actionButton(ns("pitch_up"),
                  label = NULL,
                  icon = icon("angle-up"),
                )
              ), column(
                6,
                align = "right",
                actionButton(ns("bearing_left"),
                  label = NULL,
                  icon = icon("angle-left")
                )
              ), column(6,
                align = "left",
                actionButton(ns("bearing_right"),
                  label = NULL,
                  icon = icon("angle-right")
                )
              ), column(12,
                align = "center",
                actionButton(ns("pitch_down"),
                  label = NULL,
                  icon = icon("angle-down")
                )
              ))
            ),
            column(4, selectInput(ns("styles"), "Map style",
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
            ))
          ), width = NULL)
        )
      ),
      column(
        4, box(
          title = "Chord diagram", status = "primary", solidHeader = TRUE,
          chorddiagOutput(ns("chord"), height = 805),
          width = NULL, height = "auto"
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
    fluidRow(
      column(3, box(
        title = "Options", status = "primary", height = "auto",
        solidHeader = TRUE, width = NULL,
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
      )),
      column(
        9, box(
          title = "Choropleth map", status = "primary", height = "auto",
          solidHeader = TRUE, width = NULL,
          leafletOutput(ns("choropleth_map"))
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
    fluidRow(
      column(3, box(
        title = "Options", status = "primary", height = "auto",
        solidHeader = TRUE, width = NULL,
        selectInput(ns("plot_type"), h3("Type of plot"),
          choices = plot_types,
          selected = "cluster"
        ),
        sliderInput(ns("clusters"),
          label = "Number of clusters:",
          min = 1, max = 5, value = 3, ticks = FALSE
        )
      )),
      column(
        9,
        box(
          title = "Clustering plot", status = "primary", height = "auto",
          solidHeader = TRUE, width = NULL,
          plotOutput(outputId = ns("cluster_plot"))
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
