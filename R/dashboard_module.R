# R/dashboard_module.R

dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput(ns("state"), "State", "All")),
      shiny::column(3, shiny::selectInput(ns("group"), "MBS group", "All")),
      shiny::column(3, shiny::textInput(ns("item"), "Item number contains")),
      shiny::column(3, shiny::dateRangeInput(ns("dates"), "Processing period"))
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::h4(shiny::textOutput(ns("services_kpi")))),
      shiny::column(6, shiny::h4(shiny::textOutput(ns("benefit_kpi"))))
    ),
    shiny::tabsetPanel(
      shiny::tabPanel(
        "Services",
        plotly::plotlyOutput(ns("services_plot"), height = "430px")
      ),
      shiny::tabPanel(
        "Benefits",
        plotly::plotlyOutput(ns("benefit_plot"), height = "430px")
      ),
      shiny::tabPanel(
        "Data",
        DT::DTOutput(ns("table"))
      )
    )
  )
}

dashboard_server <- function(id, data_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(data_reactive(), {
      d <- data_reactive()
      shiny::updateSelectInput(
        session, "state",
        choices = c("All", sort(unique(stats::na.omit(d$state))))
      )
      shiny::updateSelectInput(
        session, "group",
        choices = c("All", sort(unique(stats::na.omit(d$group))))
      )
      shiny::updateDateRangeInput(
        session, "dates",
        start = min(d$month_date), end = max(d$month_date),
        min = min(d$month_date), max = max(d$month_date)
      )
    })
    
    filtered <- shiny::reactive({
      d <- data_reactive()
      shiny::req(d, input$dates)
      
      if (input$state != "All")
        d <- dplyr::filter(d, state == input$state)
      if (input$group != "All")
        d <- dplyr::filter(d, group == input$group)
      if (nzchar(input$item))
        d <- dplyr::filter(
          d, stringr::str_detect(item_number, stringr::fixed(input$item))
        )
      
      d |>
        dplyr::filter(
          month_date >= input$dates[1],
          month_date <= input$dates[2]
        )
    })
    
    trend <- shiny::reactive({
      filtered() |>
        dplyr::group_by(month_date) |>
        dplyr::summarise(
          services = sum(services, na.rm = TRUE),
          benefit = sum(benefit, na.rm = TRUE),
          .groups = "drop"
        )
    })
    
    output$services_kpi <- shiny::renderText(
      paste("Services:", scales::comma(sum(filtered()$services, na.rm = TRUE)))
    )
    output$benefit_kpi <- shiny::renderText(
      paste("Benefits:", scales::dollar(sum(filtered()$benefit, na.rm = TRUE)))
    )
    
    output$services_plot <- plotly::renderPlotly({
      plotly::plot_ly(
        trend(), x = ~month_date, y = ~services,
        type = "scatter", mode = "lines+markers"
      ) |>
        plotly::layout(xaxis = list(title = ""),
                       yaxis = list(title = "Services"))
    })
    
    output$benefit_plot <- plotly::renderPlotly({
      plotly::plot_ly(
        trend(), x = ~month_date, y = ~benefit,
        type = "scatter", mode = "lines+markers"
      ) |>
        plotly::layout(xaxis = list(title = ""),
                       yaxis = list(title = "Benefits ($)"))
    })
    
    output$table <- DT::renderDT({
      filtered() |>
        dplyr::select(
          month_date, state, group, sub_group, item_number,
          services, benefit, source_resource_name
        )
    }, options = list(pageLength = 20, scrollX = TRUE))
  })
}

pbs_dashboard_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    
    shiny::fluidRow(
      
      shiny::column(
        3,
        shiny::selectInput(
          ns("state"),
          "State",
          "All"
        )
      ),
      
      shiny::column(
        3,
        shiny::selectInput(
          ns("scheme"),
          "Scheme",
          "All"
        )
      ),
      
      shiny::column(
        3,
        shiny::selectInput(
          ns("patient_category"),
          "Patient category",
          "All"
        )
      ),
      
      shiny::column(
        3,
        shiny::textInput(
          ns("item"),
          "PBS item number contains"
        )
      )
    ),
    
    shiny::fluidRow(
      
      shiny::column(
        4,
        shiny::dateRangeInput(
          ns("dates"),
          "Processing period"
        )
      ),
      
      shiny::column(
        4,
        shiny::h4(
          shiny::textOutput(
            ns("services_kpi")
          )
        )
      ),
      
      shiny::column(
        4,
        shiny::h4(
          shiny::textOutput(
            ns("benefit_kpi")
          )
        )
      )
    ),
    
    shiny::tabsetPanel(
      
      shiny::tabPanel(
        "Services",
        plotly::plotlyOutput(
          ns("services_plot"),
          height = "430px"
        )
      ),
      
      shiny::tabPanel(
        "Benefits",
        plotly::plotlyOutput(
          ns("benefit_plot"),
          height = "430px"
        )
      ),
      
      shiny::tabPanel(
        "Data",
        DT::DTOutput(
          ns("table")
        )
      )
    )
  )
}

