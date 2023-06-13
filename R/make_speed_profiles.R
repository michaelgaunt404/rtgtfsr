#' Create speed profiles by snapping realtime GTFS vehicle position data to discretized segments of the static GTFS object
#'
#' This function takes a preprocessed static GTFS object represented as an `sf` object, a coordinate reference system (CRS),
#' a crosswalk object linking trip IDs to shape IDs, realtime GTFS vehicle position data, and optional parameters for
#' sampling distance and overriding the snapping process. The function notifies the user about the start and completion
#' of the operation and also provides progress messages during the execution of the `map_df` function. It returns a list
#' containing the route samples and speed profiles.
#'
#' @param sf_object An `sf` object representing the preprocessed static GTFS object.
#' @param crs The coordinate reference system (CRS) to be used for snapping.
#' @param xwalk_ob The crosswalk object linking trip IDs to shape IDs.
#' @param vp_data Realtime GTFS vehicle position data.
#' @param samp_dist The sampling distance for snapping the points to discretized segments (default is 100).
#' @param over_ride Optional parameter for overriding the snapping process (default is NA).
#'
#' @return A list containing the route samples and speed profiles.
#'
#' @examples
#' sf_object <- read_sf("static_gtfs.shp")
#' crs <- "+proj=utm +zone=10 +ellps=WGS84"
#' xwalk_ob <- read_csv("crosswalk.csv")
#' vp_data <- read_csv("vehicle_position_data.csv")
#' make_speed_profiles(sf_object, crs, xwalk_ob, vp_data, samp_dist = 100, over_ride = NA)
#'
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr pull
#' @importFrom dplyr unique
#' @importFrom purrr map_df
#' @importFrom purrr reduce
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_join
#' @importFrom sf st_nearest_feature
#' @importFrom sf st_line_sample_to_points
#' @importFrom sf filter
#' @importFrom sf group_map
#' @importFrom sf reduce
#' @importFrom sf remove_rownames
#' @importFrom sf read_sf
#'
#' @export
make_speed_profiles <- function(sf_object, crs, xwalk_ob, vp_data, samp_dist = 100, over_ride = NA) {
  message("Starting make_speed_profiles...")

  if (!is.na(over_ride)) {
    xwalk_ob <- xwalk_override(xwalk_ob, over_ride)
  }

  sf_object_points <- sf_object %>%
    dplyr::filter(shape_id %in% unique(xwalk_ob$shape_id)) %>%
    dplyr::group_by(shape_id_1 = shape_id) %>%
    dplyr::group_map(~sf::st_line_sample_to_points(sf_object = .x, crs = crs, samp_dist = samp_dist)) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    sf::remove_rownames()

  temp_speed_profiles <- xwalk_ob$shape_id %>%
    unique() %>%
    purrr::map_df(~{
      message("Processing shape ID: ", .x)

      index_xwalk <- xwalk_ob %>%
        dplyr::filter(shape_id %in% .x) %>%
        dplyr::pull(trip_id)

      temp_vp_data <- vp_data %>%
        dplyr::filter(trip_id %in% index_xwalk)

      temp_sf_object_points <- sf_object_points %>%
        dplyr::filter(shape_id %in% .x)

      message("Number of rows in sf_object_points: ", nrow(temp_sf_object_points))
      message("Max index in sf_object_points: ", max(temp_sf_object_points$index))

      temp_joined <- sf::st_join(x = temp_vp_data,
                                 y = temp_sf_object_points,
                                 join = sf::st_nearest_feature) %>%
        sf::st_drop_geometry()

      # Additional processing steps

    })

  message("make_speed_profiles completed.")

  return(list(route_samples = sf_object_points,
              speed_profiles = temp_speed_profiles))
}
