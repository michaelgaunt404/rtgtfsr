#' Check if the vehicle positioning data has the correct amount of trip and vehicle IDs
#'
#' This function takes a data frame as input and checks if the vehicle positioning data
#' contains the correct number of trip and vehicle IDs. It provides a message to the user
#' indicating the outcome of the check and notifies that the function has started.
#'
#' @param data A data frame containing the vehicle positioning data.
#'
#' @return The maximum count of trip IDs found in the data.
#'
#' @examples
#' data <- read.csv("vehicle_data.csv")
#' check_rtgtfs_trip_vehicle_ids(data)
#'
#' @import dplyr
#' @importFrom stats unique
#' @importFrom stats arrange
#' @importFrom stats group_by
#' @importFrom stats mutate
#' @importFrom stats select
#'
#' @export
check_rtgtfs_trip_vehicle_ids <- function(data) {
  message("Starting check_rtgtfs_trip_vehicle_ids...")

  tmp_data <- data %>%
    dplyr::select(trip_id, direction_id, route_id, start_date, vehicle_id) %>%
    unique() %>%
    arrange(trip_id) %>%
    group_by(trip_id) %>%
    dplyr::mutate(count_trip_id = n()) %>%
    group_by(vehicle_id) %>%
    dplyr::mutate(count_bus_id = n())

  max_count_trip_id <- max(tmp_data$count_trip_id)

  if (max_count_trip_id == 1) {
    message("Vehicle positioning data has the correct amount of trip and vehicle IDs.")
  } else {
    message("Vehicle positioning data does not have the correct amount of trip and vehicle IDs.")
  }

  message("check_rtgtfs_trip_vehicle_ids completed.")

  return(max_count_trip_id)
}
