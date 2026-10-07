pbs_dashboard_server <- function(
    id,
    data_reactive) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      shiny::observeEvent(
        data_reactive(),
        {
          
          d <- data_reactive()
          
          shiny::updateSelectInput(
            session,
            "state",
            choices = c(
              "All",
              sort(
                unique(
                  stats::na.omit(
                    d$state
                  )
                )
              )
            )
          )
          
          shiny::updateSelectInput(
            session,
            "scheme",
            choices = c(
              "All",
              sort(
                unique(
                  stats::na.omit(
                    d$scheme
                  )
                )
              )
            )
          )
          
          shiny::updateSelectInput(
            session,
            "patient_category",
            choices = c(
              "All",
              sort(
                unique(
                  stats::na.omit(
                    d$patient_category
                  )
                )
              )
            )
          )
          
          shiny::updateDateRangeInput(
            session,
            "dates",
            start = min(
              d$month_date,
              na.rm = TRUE
            ),
            end = max(
              d$month_date,
              na.rm = TRUE
            ),
            min = min(
              d$month_date,
              na.rm = TRUE
            ),
            max = max(
              d$month_date,
              na.rm = TRUE
            )
          )
        }
      )
      
      filtered <- shiny::reactive({
        
        d <- data_reactive()
        
        shiny::req(
          d,
          input$dates
        )
        
        if (input$state != "All") {
          
          d <- dplyr::filter(
            d,
            state == input$state
          )
        }
        
        if (input$scheme != "All") {
          
          d <- dplyr::filter(
            d,
            scheme == input$scheme
          )
        }
        
        if (
          input$patient_category != "All"
        ) {
          
          d <- dplyr::filter(
            d,
            patient_category ==
              input$patient_category
          )
        }
        
        if (nzchar(input$item)) {
          
          d <- dplyr::filter(
            d,
            stringr::str_detect(
              item_number,
              stringr::fixed(
                input$item
              )
            )
          )
        }
        
        d |>
          dplyr::filter(
            month_date >=
              input$dates[1],
            month_date <=
              input$dates[2]
          )
      })
      
      trend <- shiny::reactive({
        
        filtered() |>
          dplyr::group_by(
            month_date
          ) |>
          dplyr::summarise(
            services =
              sum(
                services,
                na.rm = TRUE
              ),
            benefit =
              sum(
                benefit,
                na.rm = TRUE
              ),
            .groups = "drop"
          )
      })
      
      output$services_kpi <-
        shiny::renderText({
          
          paste(
            "PBS services:",
            scales::comma(
              sum(
                filtered()$services,
                na.rm = TRUE
              )
            )
          )
        })
      
      output$benefit_kpi <-
        shiny::renderText({
          
          paste(
            "PBS benefits:",
            scales::dollar(
              sum(
                filtered()$benefit,
                na.rm = TRUE
              )
            )
          )
        })
      
      output$services_plot <-
        plotly::renderPlotly({
          
          plotly::plot_ly(
            trend(),
            x = ~month_date,
            y = ~services,
            type = "scatter",
            mode = "lines+markers"
          ) |>
            plotly::layout(
              xaxis =
                list(title = ""),
              yaxis =
                list(
                  title =
                    "PBS services"
                )
            )
        })
      
      output$benefit_plot <-
        plotly::renderPlotly({
          
          plotly::plot_ly(
            trend(),
            x = ~month_date,
            y = ~benefit,
            type = "scatter",
            mode = "lines+markers"
          ) |>
            plotly::layout(
              xaxis =
                list(title = ""),
              yaxis =
                list(
                  title =
                    "PBS benefits ($)"
                )
            )
        })
      
      output$table <-
        DT::renderDT({
          
          filtered() |>
            dplyr::select(
              month_date,
              state,
              scheme,
              patient_category,
              item_number,
              services,
              benefit,
              source_resource_name
            )
          
        },
        options = list(
          pageLength = 20,
          scrollX = TRUE
        ))
    }
  )
}