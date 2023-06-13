#' Plot Query Batch Data
#'
#' Creates a tile plot to visualize the count of records in query batches over time.
#'
#' @param data A preprocessed data.frame containing vehicle position data.
#' @param days An optional numeric value specifying the number of days to consider for plotting. By default, it is set to 7.
#'
#' @details The `plot_query_batch_data` function takes preprocessed vehicle position data and creates a tile plot to monitor the count of records in query batches over time. The function aggregates the data by minute, counts the records within each minute, and creates a tile plot with time on the x-axis and date on the y-axis. Each tile represents a minute and is colored based on the count of records.
#'
#' By default, the function considers the last 7 days of data. However, the `days` parameter allows the user to specify a different number of days to include in the plot.
#'
#' The resulting plot provides an overview of the distribution and frequency of records in query batches, enabling the monitoring of data collection and processing processes.
#'
#' @import ggplot2
#' @import dplyr
#' @import hms
#' @import scales
#' @import tidyr
#' @import lubridate
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 scale_x_time
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labs
#'
#' @examples
#' # Plot the count of records in query batches over the last 7 days
#' plot_query_batch_data(vehicle_data)
#'
#' @export
plot_query_batch_data =  function(data, days = 7) {
  message("Creating plot of query batch data...")

  data %>%
    count(time = hms::as_hms(floor_date(date_time, "minute")),
          date = date(date_time)) %>%
    {if (!is.na(days)) (.) %>%
        filter(date > max(date) - days) else .} %>%
    ggplot() +
    geom_tile(aes(time, y = 1,  fill = n)) +
    facet_grid(row = vars(date), switch = "y") +
    scale_x_time(breaks = scales::breaks_width("1 hour")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    theme(strip.text.y.left = element_text(angle = 0)) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(y = "", fill = "Rec. Count", x = "Time of Day")

  message("Plot of query batch data created.")
}
