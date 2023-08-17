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

  number_of_query_pro = round(ttl_query_duration*60/cache_interval, 0)
  query_amount = round(cache_interval*60/query_interval, 0)

    for (j in 1:number_of_query_pro){
      tryCatch({
        sub_query_start_time = gauntlet::strg_clean_datetime()

        info(logger, "Subquery start")
        message(stringr::str_glue("{gauntlet::strg_make_space_2()}{gauntlet::strg_make_space_2()}Sub query start time: {start_time}\nMaking {query_amount} for this batch"))

        temp_file_vp = tempfile(fileext = ".csv")
        json_list_object_updates = list()
        json_list_object_alerts = list()
        num_errors = 0

        for (i in 1:query_amount) {
          tryCatch({
            time = Sys.time()
            message(str_glue("% Complete: {100*round(i/query_amount, 2)}%"))

            {
              json_data_vp = jsonlite::fromJSON(rtgtfs_feeds$url_vp, simplifyVector = T) %>%
                .[['entity']] %>%
                jsonlite::flatten() %>%
                data.frame() %>%
                filter(vehicle.trip.route_id %in% routes) %>%
                mutate(time_queired = time
                       ,query_block = j
                       ,query_num = i)

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

            remaining_time = (query_interval - as.numeric(Sys.time() - time) %% query_interval)

            Sys.sleep(remaining_time)
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
        cat("Skipping iteration", j, "\n")
      })

    }
}
