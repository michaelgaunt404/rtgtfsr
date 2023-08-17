#' Plot Batch Query Records
#'
#' This function generates a plot showing the batch query records over time.
#'
#' @param data A data frame containing the batch query records.
#' @param days An optional parameter specifying the number of days to include in the plot. Defaults to NA, which includes all available data.
#' @return A ggplot object displaying the batch query records.
#' @import ggplot2
#' @import scales
#' @examples
#' data <- read.csv("batch_query_data.csv")
#' plot_batch_query_records(data, days = 7)
plot_batch_query_records <- function(data, days = NA) {
  data %>%
    ggplot(aes(time, total_records, color = as.factor(route_id))) +
    geom_point() +
    geom_smooth() +
    scale_x_time(breaks = scales::breaks_width("1 hour")) +
    coord_cartesian(ylim = c(0, NA)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    theme(strip.text.y.left = element_text(angle = 0)) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(y= "Number of Query Records", fill = "Rec. Count", x = "Time of Day"
         ,color = "Route")
}
