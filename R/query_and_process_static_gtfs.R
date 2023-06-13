#' Query and process GTFS data from a user-supplied URL
#'
#' This function takes a user-supplied static GTFS URL, provided by municipal transit operators,
#' as input. It attempts to read and process the GTFS data using the `tidytransit` package. It
#' notifies the user about the outcome of the operation, including whether it was successful
#' or if an error occurred during the reading process.
#'
#' @param url_gtfs The URL of the static GTFS data supplied by the municipal transit operator.
#'
#' @return A spatial data frame representing the processed GTFS data.
#'
#' @examples
#' url_gtfs <- "https://example.com/gtfs.zip"
#' query_and_process_gtfs(url_gtfs)
#'
#' @import tidytransit
#' @importFrom tidytransit gtfs_as_sf
#' @importFrom tidytransit read_gtfs
#'
#' @export
query_and_process_static_gtfs <- function(url_gtfs) {
  message("Starting query_and_process_gtfs...")

  tryCatch({
    ct_gtfs <- tidytransit::read_gtfs(url_gtfs)
    ct_gtfs_sf <- tidytransit::gtfs_as_sf(ct_gtfs)

    message("GTFS data successfully read and processed.")

    return(ct_gtfs_sf)
  }, error = function(e) {
    message("An error occurred while reading the GTFS data.")
    message("Error message:", e$message)
  })

  message("query_and_process_gtfs completed.")
}
