#' Create a crosswalk dataframe between the realtime GTFS vehicle position dataset and the static GTFS input
#'
#' This function takes a realtime GTFS vehicle position dataset and a static GTFS input as input. It creates
#' a crosswalk dataframe that links the trip IDs from the realtime dataset to their corresponding shape IDs
#' from the static GTFS input. The function notifies the user about the start and completion of the operation,
#' and also reports the number of unique shapes and total number of trip IDs found in the crosswalk dataframe.
#' The crosswalk dataframe can be used in another function called `make_speed_profiles()`.
#'
#' @param vp_data A dataframe containing the realtime GTFS vehicle position dataset.
#' @param gtfs A list or dataframe representing the static GTFS input.
#'
#' @return A dataframe representing the crosswalk between trip IDs and shape IDs.
#'
#' @examples
#' vp_data <- read.csv("vehicle_position_data.csv")
#' gtfs <- read_gtfs("static_gtfs.zip")
#' get_gtfs_shape_xwalk(vp_data, gtfs)
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr unique
#' @importFrom dplyr pull
#' @importFrom dplyr %>%
#' @importFrom tidytransit read_gtfs
#'
#' @export
get_gtfs_shape_xwalk <- function(vp_data, gtfs) {
  message("Starting get_gtfs_shape_xwalk...")

  index <- vp_data %>%
    dplyr::pull(trip_id) %>%
    unique()

  index_shape <- gtfs$trips %>%
    dplyr::filter(trip_id %in% index) %>%
    dplyr::select(trip_id, shape_id) %>%
    unique()

  num_shapes <- nrow(index_shape)
  num_trips <- length(index)

  message("get_gtfs_shape_xwalk completed.")
  message("Number of unique shapes: ", num_shapes)
  message("Total number of trip IDs: ", num_trips)

  return(index_shape)
}
