# R/transform.R

standardise_mbs <- function(x) {
  x <- janitor::clean_names(x)
  
  required <- c(
    "year", "month_of_processing", "group", "sub_group",
    "item_number", "state", "services", "benefit"
  )
  
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Source schema changed. Missing columns: ",
      paste(missing, collapse = ", "),
      ". Available columns: ",
      paste(names(x), collapse = ", ")
    )
  }
  
  x |>
    dplyr::mutate(
      year = readr::parse_integer(as.character(year)),
      month_of_processing = stringr::str_squish(
        as.character(month_of_processing)
      ),
      month_date = as.Date(suppressWarnings(
        lubridate::parse_date_time(
          paste(year, month_of_processing),
          orders = c("Y B", "Y b", "Y m", "Y-m")
        )
      )),
      item_number = as.character(item_number),
      services = readr::parse_number(
        as.character(services),
        locale = readr::locale(grouping_mark = ",")
      ),
      benefit = readr::parse_number(
        as.character(benefit),
        locale = readr::locale(grouping_mark = ",")
      ),
      dplyr::across(
        c(group, sub_group, state),
        ~ dplyr::na_if(stringr::str_squish(as.character(.x)), "")
      )
    ) |>
    dplyr::filter(!is.na(month_date)) |>
    dplyr::distinct(
      source_resource_id, year, month_of_processing, group,
      sub_group, item_number, state, services, benefit,
      .keep_all = TRUE
    )
}

standardise_pbs <- function(x) {
  
  x <- janitor::clean_names(x)
  
  required <- c(
    "year",
    "item_number",
    "state",
    "scheme",
    "month_of_processing",
    "patient_category",
    "services",
    "benefit"
  )
  
  missing <- setdiff(required, names(x))
  
  if (length(missing)) {
    
    stop(
      "PBS source schema changed. Missing columns: ",
      paste(missing, collapse = ", "),
      ". Available columns: ",
      paste(names(x), collapse = ", ")
    )
  }
  
  x |>
    dplyr::mutate(
      
      year = readr::parse_integer(
        as.character(year)
      ),
      
      month_of_processing =
        stringr::str_squish(
          as.character(month_of_processing)
        ),
      
      month_date = as.Date(
        suppressWarnings(
          lubridate::parse_date_time(
            paste(
              year,
              month_of_processing
            ),
            orders = c(
              "Y B",
              "Y b",
              "Y m",
              "Y-m"
            )
          )
        )
      ),
      
      item_number =
        as.character(item_number),
      
      services =
        readr::parse_number(
          as.character(services),
          locale =
            readr::locale(
              grouping_mark = ","
            )
        ),
      
      benefit =
        readr::parse_number(
          as.character(benefit),
          locale =
            readr::locale(
              grouping_mark = ","
            )
        ),
      
      dplyr::across(
        c(
          state,
          scheme,
          patient_category
        ),
        ~ dplyr::na_if(
          stringr::str_squish(
            as.character(.x)
          ),
          ""
        )
      )
    ) |>
    
    dplyr::filter(
      !is.na(month_date)
    ) |>
    
    dplyr::distinct(
      source_resource_id,
      year,
      month_of_processing,
      item_number,
      state,
      scheme,
      patient_category,
      services,
      benefit,
      .keep_all = TRUE
    )
}
