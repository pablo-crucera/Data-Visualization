library(shiny)
library(shinydashboard)
library(chorddiag)
library(mapdeck)

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

plot_types <- c("cluster", "heatmap", "pairplot")
plot_types <- c("Cluster", "Heat map", "Pair plot")

shinyUI(dashboardPage(
  skin = "black",
  dashboardHeader(title = "NYC Taxi"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Trips flow", tabName = "trips", icon = icon("taxi")),
      menuItem("Tips", tabName = "tips", icon = icon("coins")),
      menuItem("Clustering", tabName = "clustering", icon = icon("project-diagram"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "trips",
        fluidRow(
          column(
            2, box(
              title = "Time and week day", status = "primary", height = "auto",
              solidHeader = TRUE, width = NULL,
              selectInput("day", h3("Week day"),
                choices = week_days, selected = "Monday"
              ),
              selectInput("hour", h3("Time"),
                choices = hours, selected = 17
              )
            )
          ),
          column(
            6, box(
              title = "Flow map", status = "primary",
              solidHeader = TRUE, height = "auto", width = NULL,
              mapdeckOutput("map", height = 650),
              box(fluidRow(
                column(
                  2, fluidRow(column(12,
                    align = "center",
                    actionButton("pitch_up",
                      label = NULL,
                      icon = icon("angle-up"),
                    )
                  ), column(
                    6, align = "right",
                    actionButton("bearing_left",
                      label = NULL,
                      icon = icon("angle-left")
                    )), column(6, align = "left",
                    actionButton("bearing_right",
                      label = NULL,
                      icon = icon("angle-right")
                    )
                  ), column(12,
                    align = "center",
                    actionButton("pitch_down",
                      label = NULL,
                      icon = icon("angle-down")
                    )
                  ))
                ),
                column(4, selectInput("styles", h3("Map style"),
                  choices = styles,
                  selected = "streets"
                )),
                column(2, sliderInput("width",
                  label = "Width:",
                  min = 0, max = 100, value = 50, ticks = FALSE
                )),
                column(2, sliderInput("length",
                  label = "Length:",
                  min = 0, max = 500, value = 50, ticks = FALSE
                )),
                column(2, sliderInput("speed",
                  label = "Speed:",
                  min = 0, max = 500, value = 50, ticks = FALSE
                ))
              ), width = NULL)
            )
          ),
          column(
            4, box(
              title = "Chord diagram", status = "primary", solidHeader = TRUE,
              chorddiagOutput("chord", height = 805),
              width = NULL, height = "auto"
            )
          )
        )
      ),
      tabItem(
        tabName = "clustering",
        fluidRow(
          column(3, box(
            title = "Options", status = "primary", height = "auto",
            solidHeader = TRUE, width = NULL,
            selectInput("plot_type", h3("Type of plot"),
              choices = plot_types,
              selected = "cluster"
            ),
            sliderInput("clusters",
              label = "Number of clusters:",
              min = 1, max = 5, value = 3, ticks = FALSE
            )
          )),
          column(
            9,
            box(
              title = "Clustering plot", status = "primary", height = "auto",
              solidHeader = TRUE, width = NULL,
              plotOutput(outputId = "cluster_plot")
            )
          )
        )
      ),
      tabItem(tabName = "clustering")
    )
  )
))
