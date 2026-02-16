#' Pull FRED series
#' 
#' Pulls individual series from FRED's API
#' 
#' Dependencies: dplyr, tidyr, jsonlite, purrr
#' 
#' Note: to pull multiple series, function loops through a series list
#' Be aware your series may have multiple frequencies.
#' 
#' FRED R packages to know about:
#' fredr: https://sboysel.github.io/fredr/
#' tidyquant: https://business-science.github.io/tidyquant/
#' FredR: https://github.com/jcizel/FredR  
#'
#' @param api_key string; FRED API key obtained by creating an account with FRED:
#' https://fred.stlouisfed.org/docs/api/api_key.html
#'
#' @param series_ids string; series or list of series to pull, formatted c(series1, series2)
#'
#' @return data.frame with columns: series, obs_date, obs, updated_at
pull_fred_series <- function(api_key, series_ids) {

  call <- function(series) {
    
    tryCatch({

      message(glue::glue("Pulling data for series {series}..."))
    
      # returns json
      url <- paste0("https://api.stlouisfed.org/fred/series/observations?series_id=",
                      series,
                      "&api_key=", api_key,
                      "&file_type=json")
      
      date_vars <- c("realtime_start", "realtime_end",
                     "observation_start", "observation_end",
                     "observations.realtime_start", "observations.realtime_end",
                     "observations.date")
      
      numeric_vars <- c("observations.value")
    
      df_pull <- as.data.frame(jsonlite::fromJSON(url)) %>%
        mutate(across(all_of(date_vars), ~ as.Date(.x, "%Y-%m-%d"))) %>%
        mutate(across(all_of(numeric_vars), ~ as.numeric(.x)))
      
      last_update <- max(df_pull$observations.realtime_end)
      
      message(paste0("Series ", series, " last updated ", format(last_update, "%B %d, %Y")))
      
      df <- df_pull %>%
        mutate(series = series) %>%
        select(series,
               observations.date,
               observations.value,
               observations.realtime_end)
        
      
      names(df) <- c("series", "obs_date", "obs", "updated_at")
    
      return(df)

    },
    error = function(e) {
      warning(paste0("Failed to pull series ", series, ": ", e$message))
      return(NULL) # or return empty df
    })
  }

  dfs <- purrr::map(series_ids, call) %>%
    purrr::compact() %>%
    bind_rows()

  return(dfs)

}
