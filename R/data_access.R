# R/data_access.R

CKAN_ACTION <- "https://data.gov.au/data/api/3/action"
MEDICARE_DATASET_ID <- "c8cee0b0-7b3a-4555-9bbb-f2acc0e4182b"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

ckan_request <- function(action, ...) {
  req <- httr2::request(paste0(CKAN_ACTION, "/", action)) |>
    httr2::req_url_query(...) |>
    httr2::req_user_agent("MedicareShinyPrototype/0.1") |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 3)
  
  resp <- httr2::req_perform(req)
  ans <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  
  if (!isTRUE(ans$success)) {
    stop("CKAN request failed: ", jsonlite::toJSON(ans$error, auto_unbox = TRUE))
  }
  ans$result
}

discover_medicare_resources <- function(
    cache_dir = "data/cache",
    force_network = FALSE) {
  
  fs::dir_create(cache_dir, recurse = TRUE)
  catalogue_cache <- file.path(cache_dir, "catalogue.rds")
  
  tryCatch({
    pkg <- ckan_request("package_show", id = MEDICARE_DATASET_ID)
    
    resources <- purrr::map_dfr(pkg$resources, function(x) {
      tibble::tibble(
        id = as.character(x$id %||% NA_character_),
        name = as.character(x$name %||% ""),
        format = toupper(as.character(x$format %||% "")),
        url = as.character(x$url %||% ""),
        hash = as.character(x$hash %||% ""),
        last_modified = as.character(
          x$last_modified %||% x$metadata_modified %||% ""
        ),
        datastore_active = isTRUE(x$datastore_active),
        package_id = as.character(x$package_id %||% MEDICARE_DATASET_ID)
      )
    })
    
    attr(resources, "catalogue_source") <- "live CKAN metadata"
    saveRDS(resources, catalogue_cache)
    resources
  }, error = function(e) {
    if (file.exists(catalogue_cache)) {
      cached <- readRDS(catalogue_cache)
      attr(cached, "catalogue_source") <-
        paste("cached catalogue; live request failed:", conditionMessage(e))
      cached
    } else {
      stop("Catalogue unavailable and no cached catalogue exists: ",
           conditionMessage(e))
    }
  })
}

resource_signature <- function(resources) {
  digest::digest(
    resources |>
      dplyr::arrange(id) |>
      dplyr::select(id, name, hash, last_modified, url)
  )
}

resource_cache_path <- function(meta, cache_dir) {
  hash_tag <- ifelse(nzchar(meta$hash), substr(meta$hash, 1, 12), "nohash")
  file.path(cache_dir, paste0(meta$id, "_", hash_tag, ".rds"))
}

fetch_datastore <- function(resource_id, page_size = 32000L) {
  offset <- 0L
  pages <- list()
  
  repeat {
    req <- httr2::request(paste0(CKAN_ACTION, "/datastore_search")) |>
      httr2::req_url_query(
        resource_id = resource_id,
        limit = page_size,
        offset = offset,
        include_total = "true"
      ) |>
      httr2::req_user_agent("MedicareShinyPrototype/0.1") |>
      httr2::req_timeout(90) |>
      httr2::req_retry(max_tries = 3)
    
    ans <- httr2::req_perform(req) |>
      httr2::resp_body_json(simplifyVector = TRUE)
    
    if (!isTRUE(ans$success)) stop("DataStore query unsuccessful.")
    
    records <- ans$result$records
    if (is.null(records) || length(records) == 0) break
    
    records <- tibble::as_tibble(records)
    pages[[length(pages) + 1L]] <- records
    
    n_received <- nrow(records)
    offset <- offset + n_received
    total <- as.integer(ans$result$total %||% offset)
    
    if (n_received == 0L || offset >= total) break
  }
  
  dplyr::bind_rows(pages)
}

fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  readr::read_csv(
    tmp,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )
}

load_resource <- function(meta, cache_dir, prefer_api = TRUE,
                          force_refresh = FALSE) {
  fs::dir_create(cache_dir, recurse = TRUE)
  current_cache <- resource_cache_path(meta, cache_dir)
  
  if (file.exists(current_cache) && !force_refresh) {
    return(list(
      data = readRDS(current_cache),
      source = "current local cache",
      warning = NULL
    ))
  }
  
  attempt <- tryCatch({
    if (prefer_api && isTRUE(meta$datastore_active)) {
      x <- fetch_datastore(meta$id)
      source <- "CKAN DataStore API"
    } else {
      x <- fetch_csv(meta$url)
      source <- "published CSV"
    }
    
    saveRDS(x, current_cache)
    list(data = x, source = source, warning = NULL)
  }, error = function(primary_error) {
    # Try the alternative acquisition method.
    secondary <- tryCatch({
      if (prefer_api) {
        x <- fetch_csv(meta$url)
        source <- "CSV fallback"
      } else if (isTRUE(meta$datastore_active)) {
        x <- fetch_datastore(meta$id)
        source <- "DataStore fallback"
      } else stop("No alternative endpoint.")
      
      saveRDS(x, current_cache)
      list(
        data = x,
        source = source,
        warning = conditionMessage(primary_error)
      )
    }, error = function(secondary_error) NULL)
    
    if (!is.null(secondary)) return(secondary)
    
    # Last-known-good cache for this resource ID.
    older <- list.files(
      cache_dir,
      pattern = paste0("^", meta$id, "_.*\\.rds$"),
      full.names = TRUE
    )
    
    if (length(older)) {
      older <- older[order(file.info(older)$mtime, decreasing = TRUE)][1]
      return(list(
        data = readRDS(older),
        source = "stale last-known-good cache",
        warning = conditionMessage(primary_error)
      ))
    }
    
    stop("Unable to load ", meta$name, ": ", conditionMessage(primary_error))
  })
  
  attempt
}

load_mbs_item_data <- function(resources, cache_dir = "data/cache",
                               months = 12L, prefer_api = TRUE,
                               force_refresh = FALSE) {
  months <- as.integer(months)
  selected <- resources |>
    dplyr::filter(
      stringr::str_detect(
        stringr::str_to_lower(name),
        "mbs item data"
      ),
      format == "CSV"
    ) |>
    dplyr::mutate(
      resource_month = suppressWarnings(lubridate::my(
        stringr::str_extract(name, "^[A-Za-z]+\\s+[0-9]{4}")
      ))
    ) |>
    dplyr::arrange(dplyr::desc(resource_month)) |>
    dplyr::slice_head(n = months)
  
  if (!nrow(selected)) stop("No MBS Item Data resources were discovered.")
  
  loaded <- purrr::map(seq_len(nrow(selected)), function(i) {
    meta <- selected[i, ]
    tryCatch({
      res <- load_resource(
        meta, cache_dir, prefer_api, force_refresh
      )
      res$data |>
        dplyr::mutate(
          source_resource_id = meta$id,
          source_resource_name = meta$name,
          acquisition_source = res$source,
          acquisition_warning = res$warning %||% NA_character_
        )
    }, error = function(e) {
      message("Skipping ", meta$name, ": ", conditionMessage(e))
      NULL
    })
  })
  
  data <- dplyr::bind_rows(loaded)
  if (!nrow(data)) stop("No MBS resources could be loaded.")
  
  list(
    data = data,
    resources = selected,
    catalogue_source = attr(resources, "catalogue_source") %||% "unknown"
  )
}

load_pbs_item_data <- function(
    resources,
    cache_dir = "data/cache",
    months = 12L,
    prefer_api = TRUE,
    force_refresh = FALSE) {
  
  months <- as.integer(months)
  
  selected <- resources |>
    dplyr::filter(
      stringr::str_detect(
        stringr::str_to_lower(name),
        "pbs item data"
      ),
      format == "CSV"
    ) |>
    dplyr::mutate(
      resource_month = suppressWarnings(
        lubridate::my(
          stringr::str_extract(
            name,
            "^[A-Za-z]+\\s+[0-9]{4}"
          )
        )
      )
    ) |>
    dplyr::arrange(dplyr::desc(resource_month)) |>
    dplyr::slice_head(n = months)
  
  if (!nrow(selected)) {
    stop("No PBS Item Data resources were discovered.")
  }
  
  loaded <- purrr::map(
    seq_len(nrow(selected)),
    function(i) {
      
      meta <- selected[i, ]
      
      tryCatch({
        
        res <- load_resource(
          meta,
          cache_dir,
          prefer_api,
          force_refresh
        )
        
        res$data |>
          dplyr::mutate(
            source_resource_id = meta$id,
            source_resource_name = meta$name,
            acquisition_source = res$source,
            acquisition_warning =
              res$warning %||% NA_character_
          )
        
      }, error = function(e) {
        
        message(
          "Skipping ",
          meta$name,
          ": ",
          conditionMessage(e)
        )
        
        NULL
      })
    }
  )
  
  data <- dplyr::bind_rows(loaded)
  
  if (!nrow(data)) {
    stop("No PBS resources could be loaded.")
  }
  
  list(
    data = data,
    resources = selected,
    catalogue_source =
      attr(resources, "catalogue_source") %||% "unknown"
  )
}
