#' Plotly line interactive
#'
#' Function for single series line charts
#'
#' Dependencies: dplyr, tidyr, ggplot2, stringr, scales, RColorBrewer, plotly
#'
#' Plot colors: https://www.r-bloggers.com/2020/03/how-to-standardize-group-colors-in-data-visualizations-in-r/
#'
#' @param df df; formatted desc, line_item, group, date, current, lag, chg
#' @param series_id string; id of the series you're charting
#' @param series_label string; formal name of the series you're plotting
#' @param source string; source name and credit
#' @param date_start string; earliest date of chart, formatted "yyyymmdd"
#' @param date_end string; latest date of chart, formatted "yyyymmdd", optional
#' @param labelsize numeric; size of chart labels, set in params
#' @param titlesize numeric; size of chart title, set in params
#'
#' @return line chart
plotly_line_interactive <- function(df,
                                    series_id,
                                    series_label,
                                    source = NULL,
                                    date_start = as.Date(params$date_min, "%Y%m%d"),
                                    date_end = as.Date(params$date_max, "%Y%m%d"),
                                    labelsize = params$label_size,
                                    titlesize = params$title_size) {
  
  id_pal <- RColorBrewer::brewer.pal(n = 3, name = "Blues")
  
  id_colors <- c(
    "LNS14024887" = id_pal[2]
  )
  
  id_labs <- c(
    "LNS14024887" = series_label
  )
  
  dt_start <- as.Date(date_start, format = "%Y%m%d")
  dt_end <- if (!missing(date_end)) as.Date(date_end, format = "%Y%m%d") else Sys.Date()
  
  chart_vars <- c("date", paste0(series_id))

  long <- df %>%
    select(all_of(chart_vars)) %>%
    mutate(
      date_chart = as.Date(paste(year(date), month(date), "01", sep = "-"),
                           format = "%Y-%m-%d"),
      series_name = series_id # constant column w series id
    ) %>%
    select(-date) %>%
    rename(date = date_chart) %>%
    filter(date >= dt_start & date <= dt_end) %>%
    relocate(all_of(chart_vars)) %>%
    rename("chart_val" = series_id)

  plot <- ggplot2::ggplot(long,
                          aes(color = series_name,
                              x = date,
                              y = chart_val,
                              group = series_name),
                          lwd = 0.5) +
    geom_line(aes(text = paste0(
      format(date, "%B %Y"), "<br>",
      sprintf("%.1f%%", chart_val)
    ))) +
    scale_color_manual(name = "",
                       labels = id_labs,
                       values = id_colors) +
    scale_y_continuous(breaks = seq(0, 30, 5)) + 
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    theme_bw() +
    labs(x = "",
         y = "Percent",
         fill = "",
         title = paste0(series_label, ", ",
                        format(min(long$date), "%B %Y"),
                        " to ",
                        format(max(long$date), "%B %Y"))) +
    scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = labelsize)) +
    theme(axis.title.x = element_text(size = labelsize),
          axis.title.y = element_text(size = labelsize),
          axis.text.x = element_text(size = labelsize),
          axis.text.y = element_text(size = labelsize)) +
    theme(plot.title = element_text(size = titlesize))
  
  # convert to plotly object; long annotation for source chart caption
  ggplotly(plot, tooltip = "text") %>%
    layout(showlegend = FALSE,
           margin = list(b = 80),
           annotations = list(
             list(
               x = 1,
               y = -0.15,
               text = source,
               showarrow = FALSE,
               xref = "paper", yref = "paper",
               xanchor = "right", yanchor = "auto",
               xshift = 0, yshift = 0,
               font = list(size = 10, color = "gray"))
             )
           )
  
}
