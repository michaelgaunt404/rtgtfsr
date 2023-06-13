#' Process VP List RDS Files
#'
#' Processes a list of RDS objects containing real-time GTFS feed queries saved throughout the day, extracting relevant information and performing data transformations.
#'
#' @param rds_list_objects A list of RDS objects representing the real-time GTFS feed queries saved throughout the day.
#' @param timezone The timezone used for processing the query batch timestamp (default is "US/Pacific").
#' @param am_peak A numeric vector specifying the start and end hours of the AM peak period (default is c(6, 9)).
#' @param pm_peak A numeric vector specifying the start and end hours of the PM peak period (default is c(16, 18)).
#' @param midday_peak A numeric vector specifying the start and end hours of the midday peak period (default is c(10, 14)).
#' @return A data.frame containing the processed data with relevant information extracted and data transformations applied.
#'
#' @details The `process_vp_list_rds_1` function takes a list of RDS objects, each representing a real-time GTFS feed query saved at different points throughout the day. The function processes these objects to extract relevant information and apply necessary data transformations.
#'
#' The function starts by combining the RDS objects with their corresponding names using the `pmap` function from the `purrr` package. It extracts the query batch timestamp by removing the "data_vp_" prefix and the ".rds" extension from the names and converting the resulting character strings to datetime objects with the specified `timezone`.
#'
#' The processed data is then combined using the `reduce` function from the `purrr` package and renamed using the `rename_with` function. The renaming step replaces "vehicle.id" with "vehicle_id" in the column names and retains only the portion after the last dot in the remaining column names.
#'
#' The data is arranged by the route ID, vehicle ID, trip ID, and query batch timestamp using the `arrange` function from the `dplyr` package. It is then grouped by the route ID, vehicle ID, and trip ID using the `group_by` function.
#'
#' Additional computations are performed within each group using the `mutate` function. The `date_time` column is derived from the `timestamp` column by converting it to numeric format, then to datetime format using the `lubridate::as_datetime` function, and finally applying the specified `timezone` using the `lubridate::with_tz` function.
#'
#' The `datetime_diff` column is calculated as the difference in seconds between consecutive `date_time` values using the `lag` function. Finally, the data is ungrouped and converted back to a data.frame using the `data.frame` function. Rows with a `datetime_diff` of zero are filtered out to remove redundant or duplicate entries.
#'
#' This function facilitates the processing and transformation of real-time GTFS feed query data, enabling further analysis and visualization of the queried information.
#'
#' @references The `purrr` and `dplyr` packages are used in this function for list manipulation and data transformation, respectively. Please refer to their documentation for further details.
#'
#' @examples
#' # Process a list of RDS objects containing real-time GTFS feed queries.
#' processed_data <- process_vp_list_rds(vp_rds_list)
#'
#' @import purrr
#' @import dplyr
#' @import lubridate
#' @importFrom stringr str_remove_all, str_replace, gsub
#' @importFrom tidyr rename_with
#'
#' @export
process_vp_list_rds = function(rds_list_objects, timezone = "US/Pacific",
                               am_peak = c(6, 9), pm_peak = c(16, 18), midday_peak = c(10, 14)) {
  message("Processing VP List RDS Files...")

  processed_data <- list(rds_list_objects, names(rds_list_objects)) %>%
    pmap(~ {
      message("Processing ", .y, "...")
      .x %>% mutate(query_batch = .y %>% str_remove_all("data_vp_|\\.rds") %>%
                      as_datetime(tz = timezone))
    }) %>%
    reduce(bind_rows) %>%
    rename_with(~ .x %>% str_replace(., "vehicle.id", "vehicle_id") %>% gsub(".*\\.", "\\1", .)) %>%
    arrange(route_id, vehicle_id, trip_id, query_batch) %>%
    group_by(route_id, vehicle_id, trip_id) %>%
    mutate(date_time = timestamp %>% as.numeric() %>% lubridate::as_datetime() %>% lubridate::with_tz(timezone)) %>%
    arrange(route_id, vehicle_id, trip_id, query_batch, date_time) %>%
    mutate(datetime_diff = as.numeric(date_time - lag(date_time))) %>%
    ungroup() %>%
    data.frame() %>%
    filter(datetime_diff != 0) %>%
    mutate(flag_peak_time = case_when(
      hour(date_time) >= am_peak[1] & hour(date_time) < am_peak[2] ~ "AM Peak",
      hour(date_time) >= pm_peak[1] & hour(date_time) < pm_peak[2] ~ "PM Peak",
      hour(date_time) >= midday_peak[1] & hour(date_time) < midday_peak[2] ~ "Midday",
      TRUE ~ "Untracked"
    ))

  message("VP List RDS Files processed.")

  return(processed_data)
}
