#' Make Vehicle Position Data Summary Statistics
#'
#' Computes summary statistics for vehicle position data.
#'
#' @param data A data.frame containing vehicle position data.
#'
#' @details The `make_vp_data_smmry_stats` function takes vehicle position data and computes summary statistics. It counts the number of records for each query batch, route ID, and vehicle ID. It then groups the data by query batch and route ID and calculates the number of buses in service (`buses_in_service`) and the total number of records (`total_records`) for each group.
#'
#' The function also adds a time column (`time`) by converting the `query_batch` column to the nearest minute using the `hms::as_hms` function, and a date column (`date`) by extracting the date from the `query_batch` column.
#'
#' The resulting data.frame contains the summary statistics for each query batch and route ID, providing insights into the number of buses in service and the total number of records for each group.
#'
#' @import dplyr
#' @import hms
#' @import lubridate
#' @importFrom dplyr count
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom hms as_hms
#' @importFrom lubridate floor_date
#'
#' @examples
#' # Compute summary statistics for vehicle position data
#' summary_stats <- make_vp_data_smmry_stats(vehicle_data)
#'
#' @export
make_vp_data_smmry_stats = function(data) {
  data %>%
    count(query_batch, route_id, vehicle_id) %>%
    group_by(query_batch, route_id) %>%
    summarise(buses_in_service = n(),
              total_records = sum(n)) %>%
    ungroup() %>%
    mutate(time = hms::as_hms(floor_date(query_batch, "minute")),
           date = lubridate::as_date(query_batch))
}
