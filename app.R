library(shiny)
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)
library(lubridate)
library(readr)
library(janitor)
library(plotly)
library(DT)
library(scales)
library(digest)
library(fs)

source("R/data_access.R")
source("R/transform.R")
source("R/dashboard_module.R")

CACHE_DIR <- "data/cache"
fs::dir_create(CACHE_DIR, recurse = TRUE)

ui <- fluidPage(
  titlePanel("Australian Medicare Statistics Explorer"),
  sidebarLayout(
    sidebarPanel(
      numericInput("months", "Most recent MBS resources", 12, 1, 60),
      radioButtons(
        "mode", "Preferred ingestion",
        choices = c("CKAN DataStore API" = "api",
                    "Published CSV" = "csv")
      ),
      actionButton("refresh", "Refresh now"),
      checkboxInput("auto_refresh", "Poll for new resources", TRUE),
      numericInput("poll_minutes", "Polling interval (minutes)", 30, 5, 1440),
      hr(),
      strong("Status"),
      verbatimTextOutput("status")
    ),
    mainPanel(dashboard_ui("dashboard"))
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    data = NULL,
    signature = NULL,
    status = "Starting…"
  )
  
  load_now <- function(catalogue = NULL, force = FALSE) {
    tryCatch({
      withProgress(message = "Loading Medicare data", value = 0, {
        if (is.null(catalogue)) {
          catalogue <- discover_medicare_resources(
            CACHE_DIR, force_network = TRUE
          )
        }
        incProgress(0.25)
        
        bundle <- load_mbs_item_data(
          catalogue,
          cache_dir = CACHE_DIR,
          months = input$months,
          prefer_api = identical(input$mode, "api"),
          force_refresh = force
        )
        incProgress(0.60)
        
        rv$data <- standardise_mbs(bundle$data)
        rv$signature <- resource_signature(catalogue)
        rv$status <- paste(
          "Rows:", format(nrow(rv$data), big.mark = ","),
          "\nResources:", nrow(bundle$resources),
          "\nCatalogue:", bundle$catalogue_source,
          "\nLoaded:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      })
    }, error = function(e) {
      rv$status <- paste("Refresh failed:", conditionMessage(e),
                         "\nExisting in-memory data retained.")
      showNotification(conditionMessage(e), type = "error", duration = 10)
    })
  }
  
  session$onFlushed(function() load_now(), once = TRUE)
  
  observeEvent(input$refresh, {
    load_now(force = TRUE)
  })
  
  observe({
    invalidateLater(input$poll_minutes * 60 * 1000, session)
    req(input$auto_refresh)
    
    catalogue <- try(
      discover_medicare_resources(CACHE_DIR, force_network = TRUE),
      silent = TRUE
    )
    
    if (!inherits(catalogue, "try-error")) {
      new_signature <- resource_signature(catalogue)
      if (!identical(new_signature, rv$signature)) {
        load_now(catalogue = catalogue, force = FALSE)
      }
    }
  })
  
  output$status <- renderText(rv$status)
  
  app_data <- reactive({
    req(rv$data)
    rv$data
  })
  
  dashboard_server("dashboard", app_data)
}

shinyApp(ui, server)