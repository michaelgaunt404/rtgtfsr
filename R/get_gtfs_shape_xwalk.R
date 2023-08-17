#' Create a crosswalk dataframe between the realtime GTFS vehicle position dataset and the static GTFS input
#'
#' This function takes a pre-processed, realtime GTFS vehicle position dataset and a static GTFS list as inputs. It creates
#' a crosswalk dataframe that links the trip IDs from the vehicle poisition dataset to their corresponding shape IDs
#' from the static GTFS input. The function notifies the user about the start and completion of the operation,
#' and also reports the number of unique shapes and total number of trip IDs found in the crosswalk dataframe.
#' The crosswalk dataframe is required by the function called `make_speed_profiles()`.
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

  index = vp_data %>%
    dplyr::pull(trip_id) %>%
    unique()

  index_shape = gtfs$trips %>%
    dplyr::filter(trip_id %in% index) %>%
    dplyr::select(trip_id, shape_id) %>%
    unique()

  message("get_gtfs_shape_xwalk completed.")
  message("Number of unique route shape IDs: ", length(unique(index_shape$shape_id)))
  message("Total number of trip IDs: ", length(unique(index_shape$trip_id)))

  #put something here that aborts if either one of these items are zero

  return(index_shape)
}
