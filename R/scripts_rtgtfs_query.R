#' Check if all data files exist and have non-zero size
#'
#' This function checks if all data files specified by the given \code{names} and \code{sub_query_start_time} exist
#' and have non-zero size. It returns the result as a list with two elements: a logical vector indicating if all files
#' exist, and a numeric vector with the size of each file.
#'
#' This code is not ran by the user and simply checks if the files are made during function \code{query_rtgtfs_json}
#'
#' @param names a character vector of file names
#' @param time a character string representing the sub-query start time in the format "YYYY-MM-DD"
#' @return a list containing two elements: a logical vector indicating if all files exist, and a numeric vector with the size of each file
#' @examples
#' \dontrun{
#' # Check if files "file1" and "file2" exist for the sub-query starting on January 1st, 2022
#' check_data_files(c("file1", "file2"), "2022-01-01")
#' }
#' @export
check_data_files <- function(names, time) {
  # Check if all data files exist
  all_files_exist <- names %>%
    purrr::map_lgl(~ {
      file.exists(here::here("data", stringr::str_glue("{.x}_{time}.rds")))
    }) %>%
    all()

  # Check if all data files have non-zero size
  all_files_nonzero <- names %>%
    purrr::map_dbl(~ {
      file.size(here::here("data", stringr::str_glue("{.x}_{time}.rds")))
    })

  # Return the result as a logical vector
  list(all_files_exist, all_files_nonzero)
}

#' Query RTGTFS JSON data
#'
#' This function queries real-time data for specified routes from RTGTFS feeds, and saves the obtained data as RDS files.
#'
#' @param rtgtfs_feeds A list of RTGTFS feed URLs (containing URLs for vehicle positions, updates, and alerts).
#' @param ttl_query_duration The duration of the entire query process (in hours).
#' @param cache_interval The interval between subsequent queries for the same data (in minutes).
#' @param query_interval The interval between queries for new data (in seconds).
#' @param routes A character vector containing the route IDs to be queried.
#' @param log_file A character string representing the path to the log file.
#'
#' @return Nothing is returned by this function, but the obtained data is saved as RDS files.
#'
#' @import jsonlite dplyr purrr here readr
#'
#' @examples
#' \dontrun{
#' query_rtgtfs_json(rtgtfs_feeds = list(url_vp = "http://url_vp", url_updates = "http://url_updates", url_alerts = "http://url_alerts"),
#'                   ttl_query_duration = 12, cache_interval = 5, query_interval = 10,
#'                   routes = c("route_1", "route_2"), log_file = "query.log")
#' }
#' @export
query_rtgtfs_json = function(rtgtfs_feeds, ttl_query_duration, cache_interval, query_interval, routes, log_file) {
  logger = logger("DEBUG", appenders = file_appender(here::here(log_file)))
  num_queries = (ttl_query_duration * 60 * 60) / query_interval
  query_duration = cache_interval * num_queries
  start_time = gauntlet::strg_clean_datetime()

  info(logger, stringr::str_glue("Query start time: {start_time}"))
  message(stringr::str_glue("Starting query as {start_time}\nThis will run for approximately {ttl_query_duration} hours ({ttl_query_duration*60} min)\nThe query process will be ran a total of {ttl_query_duration*60/cache_interval} times\n{num_queries} queries will be made in total"))

  number_of_query_pro = ttl_query_duration*60/cache_interval
  query_amount = cache_interval*60/query_interval

  for (i in 1:number_of_query_pro){
    tryCatch({
      sub_query_start_time = gauntlet::strg_clean_datetime()

      info(logger, "Subquery start")
      message(stringr::str_glue("{gauntlet::strg_make_space_2()}{gauntlet::strg_make_space_2()}Sub query start time: {start_time}"))

      temp_file_vp = tempfile(fileext = ".csv")
      json_list_object_updates = list()
      json_list_object_alerts = list()
      num_errors = 0

      for (i in 1:query_amount) {

        tryCatch({
          message(str_glue("% Complete: {100*round(i/query_amount, 2)}%"))

          {
            json_data_vp = jsonlite::fromJSON(rtgtfs_feeds$url_vp, simplifyVector = T) %>%
              .[['entity']] %>%
              jsonlite::flatten() %>%
              data.frame() %>%
              filter(vehicle.trip.route_id %in% routes)

            json_data_names = names(json_data_vp)

            readr::write_csv(json_data_vp, temp_file_vp, append = T)
          }

          {
            json_data_updates = jsonlite::fromJSON(rtgtfs_feeds$url_updates, simplifyVector = F) %>%
              .[['entity']]

            index_updates = json_data_updates %>%
              purrr::map_lgl(~{
                (.x$trip_update$trip$route_id %in% routes)
              })

            json_list_object_updates[[i]] = json_data_updates[index_updates]
          }

          {
            json_data_alerts = jsonlite::fromJSON(rtgtfs_feeds$url_alerts, simplifyVector = F) %>%
              .[['entity']]

            index_alerts = json_data_alerts %>%
              purrr::map_lgl(~{
                (.x$alert$informed_entity[[1]]$route_id %in% routes)
              })

            json_list_object_alerts[[i]] = json_data_alerts[index_alerts]
          }

          Sys.sleep(query_interval)

        }, error = function(e) {
          cat("Error:", conditionMessage(e), "\n")
          cat("Skipping iteration", i, "\n")
          num_errors <<- num_errors + 1
        })
      }

      full_queired_data = read.csv(temp_file_vp) %>%
        set_names(json_data_names)

      gauntlet::log_and_info(stringr::str_glue("{nrow(full_queired_data)} records were obtained for this query."), logger)
      gauntlet::log_and_warn(stringr::str_glue("There were {num_errors} failed RTGTFS queries.... ~{100*round(num_errors/query_amount, 2)}% of total"), logger)

      saveRDS(full_queired_data, here::here("data", stringr::str_glue('data_vp_{sub_query_start_time}.rds')))
      saveRDS(json_list_object_updates, here::here("data", stringr::str_glue('data_updates_{sub_query_start_time}.rds')))
      saveRDS(json_list_object_alerts, here::here("data", stringr::str_glue('data_alerts_{sub_query_start_time}.rds')))

      file_checks = check_data_files(c("data_vp", 'data_updates', 'data_alerts'), sub_query_start_time)

      message(stringr::str_glue("All files {ifelse(file_checks[[1]], 'successfully', 'unsuccessfully')} created"))
      message(stringr::str_glue("Files sizes are {paste0(file_checks[[2]], collapse = ', ')}"))

      # delete the temporary file
      file.remove(temp_file_vp)

      message("Sub Query END")

    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      cat("Skipping iteration", i, "\n")
    })

  }
}


