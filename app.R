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

# ---------------------------------------------------------
# Load app functions
# ---------------------------------------------------------

source("R/data_access.R")
source("R/transform.R")
source("R/dashboard_module.R")
source("R/pbs_dashboard_module.R")


# ---------------------------------------------------------
# Settings
# ---------------------------------------------------------
# Connect Cloud sets R_CONFIG_ACTIVE = "connect_cloud".
# Use the normal project cache locally and a temporary cache in the cloud.

IS_CONNECT_CLOUD <- identical(
  Sys.getenv("R_CONFIG_ACTIVE"),
  "connect_cloud"
)

CACHE_DIR <- if (IS_CONNECT_CLOUD) {
  file.path(tempdir(), "medicare-cache")
} else {
  "data/cache"
}

fs::dir_create(
  CACHE_DIR,
  recurse = TRUE
)


# =========================================================
# USER INTERFACE
# =========================================================

ui <- fluidPage(
  
  titlePanel(
    "Australian Medicare Statistics Explorer"
  ),
  
  sidebarLayout(
    
    # -----------------------------------------------------
    # LEFT SIDEBAR
    # -----------------------------------------------------
    
    sidebarPanel(
      
      numericInput(
        "months",
        "Most recent monthly resources",
        value = 3,
        min = 1,
        max = 60
      ),
      
      radioButtons(
        "mode",
        "Preferred ingestion",
        choices = c(
          "CKAN DataStore API" = "api",
          "Published CSV" = "csv"
        ),
        selected = "api"
      ),
      
      actionButton(
        "refresh",
        "Refresh now"
      ),
      
      checkboxInput(
        "auto_refresh",
        "Poll for new resources",
        value = FALSE
      ),
      
      numericInput(
        "poll_minutes",
        "Polling interval (minutes)",
        value = 30,
        min = 5,
        max = 1440
      ),
      
      hr(),
      
      strong("Status"),
      
      verbatimTextOutput(
        "status"
      )
    ),
    
    
    # -----------------------------------------------------
    # MAIN DASHBOARD
    # -----------------------------------------------------
    
    mainPanel(
      
      tabsetPanel(
        
        id = "dataset_tab",
        
        # -------------------------
        # MBS TAB
        # -------------------------
        
        tabPanel(
          "MBS",
          
          dashboard_ui(
            "mbs_dashboard"
          )
        ),
        
        # -------------------------
        # PBS TAB
        # -------------------------
        
        tabPanel(
          "PBS",
          
          pbs_dashboard_ui(
            "pbs_dashboard"
          )
        )
      )
    )
  )
)


# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session) {
  
  
  # -------------------------------------------------------
  # Store datasets
  # -------------------------------------------------------
  
  rv <- reactiveValues(
    
    mbs = NULL,
    
    pbs = NULL,
    
    signature = NULL,
    
    status = "Starting..."
  )
  
  
  # =======================================================
  # MAIN DATA LOADING FUNCTION
  # =======================================================
  
  load_now <- function(
    catalogue = NULL,
    force = FALSE) {
    
    # -------------------------------------------------------
    # Capture Shiny inputs as ordinary R values
    # -------------------------------------------------------
    
    months_to_load <- isolate(input$months)
    
    prefer_api_to_use <- isolate(input$mode) == "api"
    
    
    tryCatch({
      
      withProgress(
        message = "Loading Medicare data",
        value = 0,
        {
          
          # --------------------------------------------------
          # STEP 1: Get catalogue
          # --------------------------------------------------
          
          if (is.null(catalogue)) {
            
            catalogue <- discover_medicare_resources(
              cache_dir = CACHE_DIR,
              force_network = TRUE
            )
          }
          
          incProgress(
            0.15,
            detail = "Catalogue loaded"
          )
          
          
          # --------------------------------------------------
          # STEP 2: Load MBS
          # --------------------------------------------------
          
          mbs_bundle <- load_mbs_item_data(
            resources = catalogue,
            cache_dir = CACHE_DIR,
            months = months_to_load,
            prefer_api = prefer_api_to_use,
            force_refresh = force
          )
          
          incProgress(
            0.30,
            detail = "MBS data loaded"
          )
          
          
          # --------------------------------------------------
          # STEP 3: Load PBS
          # --------------------------------------------------
          
          pbs_bundle <- load_pbs_item_data(
            resources = catalogue,
            cache_dir = CACHE_DIR,
            months = months_to_load,
            prefer_api = prefer_api_to_use,
            force_refresh = force
          )
          
          incProgress(
            0.30,
            detail = "PBS data loaded"
          )
          
          
          # --------------------------------------------------
          # STEP 4: Standardise
          # --------------------------------------------------
          
          # --------------------------------------------------
          # STEP 4: Standardise datasets
          # --------------------------------------------------
          
          mbs_clean <- standardise_mbs(
            mbs_bundle$data
          )
          
          pbs_clean <- standardise_pbs(
            pbs_bundle$data
          )
          
          incProgress(
            0.15,
            detail = "Data cleaned"
          )
          
          
          # --------------------------------------------------
          # STEP 5: Store datasets in reactiveValues
          # --------------------------------------------------
          
          rv$mbs <- mbs_clean
          rv$pbs <- pbs_clean
          
          
          # --------------------------------------------------
          # STEP 6: Catalogue signature
          # --------------------------------------------------
          
          rv$signature <- resource_signature(
            catalogue
          )
          
          
          # --------------------------------------------------
          # STEP 7: Status
          # --------------------------------------------------
          
          rv$status <- paste(
            "MBS rows:",
            format(
              nrow(mbs_clean),
              big.mark = ","
            ),
            
            "\nPBS rows:",
            format(
              nrow(pbs_clean),
              big.mark = ","
            ),
            
            "\nMBS resources:",
            nrow(mbs_bundle$resources),
            
            "\nPBS resources:",
            nrow(pbs_bundle$resources),
            
            "\nLoaded:",
            format(
              Sys.time(),
              "%Y-%m-%d %H:%M:%S"
            )
          )
          
          incProgress(
            0.10,
            detail = "Complete"
          )
        }
      )
      
    }, error = function(e) {
      
      rv$status <- paste(
        "Refresh failed:",
        conditionMessage(e),
        "\nExisting in-memory data retained."
      )
      
      showNotification(
        conditionMessage(e),
        type = "error",
        duration = 15
      )
      
    })
  }
  
  
  # =======================================================
  # INITIAL LOAD
  # =======================================================
  
  session$onFlushed(
    
    function() {
      
      load_now()
      
    },
    
    once = TRUE
  )
  
  
  # =======================================================
  # MANUAL REFRESH BUTTON
  # =======================================================
  
  observeEvent(
    input$refresh,
    {
      
      load_now(
        force = TRUE
      )
      
    }
  )
  
  
  # =======================================================
  # AUTOMATIC CATALOGUE CHECK
  # =======================================================
  
  observe({
    
    invalidateLater(
      input$poll_minutes *
        60 *
        1000,
      session
    )
    
    
    req(
      input$auto_refresh
    )
    
    
    catalogue <-
      try(
        
        discover_medicare_resources(
          
          cache_dir = CACHE_DIR,
          
          force_network = TRUE
        ),
        
        silent = TRUE
      )
    
    
    if (
      !inherits(
        catalogue,
        "try-error"
      )
    ) {
      
      
      new_signature <-
        resource_signature(
          catalogue
        )
      
      
      if (
        !identical(
          new_signature,
          rv$signature
        )
      ) {
        
        
        load_now(
          
          catalogue = catalogue,
          
          force = FALSE
        )
      }
    }
  })
  
  
  # =======================================================
  # STATUS PANEL
  # =======================================================
  
  output$status <-
    renderText({
      
      rv$status
      
    })
  
  
  # =======================================================
  # MBS REACTIVE DATA
  # =======================================================
  
  mbs_data <-
    reactive({
      
      req(
        rv$mbs
      )
      
      rv$mbs
      
    })
  
  
  # =======================================================
  # PBS REACTIVE DATA
  # =======================================================
  
  pbs_data <-
    reactive({
      
      req(
        rv$pbs
      )
      
      rv$pbs
      
    })
  
  
  # =======================================================
  # MBS DASHBOARD
  # =======================================================
  
  dashboard_server(
    
    "mbs_dashboard",
    
    mbs_data
  )
  
  
  # =======================================================
  # PBS DASHBOARD
  # =======================================================
  
  pbs_dashboard_server(
    
    "pbs_dashboard",
    
    pbs_data
  )
  
}


# =========================================================
# RUN APPLICATION
# =========================================================

shinyApp(
  ui = ui,
  server = server
)
