#' Convert VP Data to Spatial Format
#'
#' Converts a data.frame containing longitude and latitude coordinates into a spatial format suitable for mapping purposes.
#'
#' @param data A data.frame containing longitude and latitude columns.
#' @param crs_to A character string specifying the Coordinate Reference System (CRS) for the specific area where the data is located. The CRS should have meters as its base unit.
#' @return A spatial object of class 'sf' representing the converted data in the specified CRS.
#'
#' @details The `convert_vp_data_to_sf_1` function takes a data.frame with longitude and latitude columns and converts it into a spatial object using the 'sf' package. The resulting spatial object can be used for mapping and spatial analysis purposes.
#'
#' The function first converts the data.frame into an 'sf' object using the `sf::st_as_sf` function, specifying the longitude and latitude columns as the coordinate values and the CRS of the original data as EPSG code 4326 (WGS84). The `crs_to` parameter allows the user to specify the desired CRS for the converted data. It is important to choose a CRS appropriate for the specific area where the data is located and ensure that the CRS has meters as its base unit.
#'
#' The converted spatial object is then transformed to the specified CRS using the `sf::st_transform` function. The function also applies the `gauntlet::st_extract_coords` function to extract the route, vehicle, and trip identifiers from the data and groups the data by these identifiers.
#'
#' Additional computations are performed within each group using the `dplyr` package. These computations include calculating the differences in longitude and latitude between consecutive points, the total distance traveled (`ttl_diff`), and the average speed (`speed_avg`). Points with an average speed exceeding 90 meters per second are filtered out to remove any potential outliers. The data is then ungrouped and transformed back to EPSG code 4326 to ensure compatibility with common mapping tools.
#'
#' This function simplifies the process of converting tabular data with coordinates into a spatial format, enabling easier visualization and analysis of geographic data.
#'
#' @references The `sf` and `dplyr` packages are used in this function for spatial data manipulation and analysis, respectively. Please refer to their documentation for further details.
#'
#' @examples
#' # Convert VP data in a data.frame format to an 'sf' object with a specific CRS.
#' converted_data <- convert_vp_data_to_sf(vp_data, crs_to = "EPSG:32633")
#'
#' @import sf
#' @import dplyr
#' @import gauntlet
#'
#' @export
convert_vp_data_to_sf = function (data, crs_to) {
  message("Converting VP Data to Spatial Format...")

  converted_data <- data %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    sf::st_transform(crs = crs_to) %>%
    gauntlet::st_extract_coords() %>%
    group_by(route_id, vehicle_id, trip_id) %>%
    mutate(
      lon_diff = lon - lag(lon),
      lat_diff = lat - lag(lat),
      ttl_diff = sqrt(lon_diff^2 + lat_diff^2),
      speed_avg = (ttl_diff / datetime_diff) * 2.236936
    ) %>%
    ungroup() %>%
    filter(speed_avg < 90) %>%
    group_by(route_id, vehicle_id, trip_id, direction_id) %>%
    mutate(speed_avg_diff = speed_avg - lag(speed_avg)) %>%
    ungroup() %>%
    sf::st_transform(4326)

  message("VP Data converted to Spatial Format.")

  return(converted_data)
}
