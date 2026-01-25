#' Pull FRED series
#' 
#' Pulls individual series from FRED's API
#' 
#' Dependencies: dplyr, tidyr, jsonlite, purrr
#' 
#' Note: to pull multiple series, function loops through a series list
#' 
#' Assumes series are produced at the same frequency (weekly, monthly, annual)
#' and have identical dates and updated dates.
#' If series in your list don't share dates and frequency, pull them 
#' individually.
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
#' @return df
pull_fred_series <- function(api_key, series_ids) {

  call <- function(series) {

    message(paste0("Pulling data for series ", series, "..."))

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
      mutate_at(date_vars, ~ as.Date(.x, "%Y-%m-%d")) %>%
      mutate_at(numeric_vars, ~ as.numeric(.x))
    
    last_update <- df_pull %>%
      select(update = observations.realtime_end) %>%
      summarise(max(update))
    
    message(paste0("Series ", series, " last updated ", format(last_update, "%B %d, %Y")))
    
    df <- df_pull %>%
      mutate(series = series) %>%
      select(series,
             observations.date,
             observations.value,
             observations.realtime_end)
      
    
    names(df) <- c("series", "obs_date", "obs", "updated_at")

    return(df)

  }

  dfs <- purrr::map(series_ids, call) %>%
    bind_rows() %>%
    pivot_wider(names_from = "series", values_from = "obs") %>%
    arrange(obs_date)

  return(dfs)

}
