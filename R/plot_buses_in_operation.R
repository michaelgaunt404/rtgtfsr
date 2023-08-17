#' Plot Buses in Operation
#'
#' Creates a line plot to visualize the number of buses in operation over time.
#'
#' @param data A preprocessed data.frame containing vehicle position data.
#' @param days An optional numeric value specifying the number of days to consider for plotting. By default, it is set to 7.
#'
#' @details The `plot_buses_in_operation` function takes preprocessed vehicle position data and creates a line plot to monitor the number of buses in operation over time. The function aggregates the data by minute, counts the number of buses in operation within each minute, and creates a line plot with time on the x-axis and the number of buses on the y-axis.
#'
#' By default, the function considers the last 7 days of data. However, the `days` parameter allows the user to specify a different number of days to include in the plot.
#'
#' The resulting plot provides an overview of the bus operations, allowing for monitoring and analysis of bus activity patterns.
#'
#' @import ggplot2
#' @import dplyr
#' @import hms
#' @import scales
#' @import tidyr
#' @import lubridate
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 scale_x_time
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labs
#'
#' @examples
#' # Plot the number of buses in operation over the last 7 days
#' plot_buses_in_operation(vehicle_data)
#'
#' @export
plot_buses_in_operation =  function(data, days = NA) {
  message("Creating plot of buses in operation...")

  data %>%
    {if (!is.na(days)) (.) %>%
        filter(date > max(date) - days) else .} %>%
    ggplot(aes(time, buses_in_service, group = date)) +
    geom_line(alpha = .5) +
    geom_point(alpha = .5) +
    ggplot2::scale_x_time(breaks = scales::breaks_width("1 hour")) +
    facet_grid(cols = vars(route_id), switch = "y") +
    coord_cartesian(ylim = c(0, NA)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(y = "No. of Buses Operating", fill = "Rec. Count", x = "Time of Day")

  message("Plot of buses in operation created.")
}
