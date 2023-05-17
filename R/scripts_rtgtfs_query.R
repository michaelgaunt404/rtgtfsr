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

#' Cache Daily Files
#'
#' Combines individual RDS files generated throughout a day into a consolidated file for efficient data storage and retrieval.
#'
#' @param folder A character string specifying the folder path where the RDS files are located. By default, it is set to "data".
#' @param folder_save_to A character string specifying the folder path where the condensed file will be saved. By default, it is set to "data".
#' @return This function does not explicitly return a value, but it caches the condensed file(s) in the specified folder.
#'
#' @details The `cache_daily_files` function scans the specified folder for RDS files that contain daily updates, alerts, or variable parameters. It extracts the relevant data from these files and combines them into a single consolidated file. The consolidated file is then saved in the designated folder using a standardized naming convention.
#'
#' The process involves parsing and filtering the filenames to identify the relevant files, reading each file using the `gauntlet::read_rds_allFiles` function, and merging the data into a unified dataset using the `bind_rows` function from the `dplyr` package. The resulting combined dataset is then saved as an RDS file using the `readr::write_rds` function.
#'
#' Additionally, the function performs a validation check by reading the saved RDS file using `readr::read_rds` and comparing the number of rows and the content with the original combined dataset. The `check_row` and `check_identical` variables indicate whether the validation is successful.
#'
#' This function aids in the organization and optimization of data storage, allowing for efficient retrieval and processing of consolidated daily files.
#'
#' @references The `gauntlet` and `dplyr` packages are utilized in this function for reading and manipulating data, respectively. Please refer to their documentation for further details.
#'
#' @examples
#' # Cache daily files in the "data" folder and save the consolidated file in the same folder.
#' cache_daily_files()
#'
#' # Cache daily files in a custom folder and save the consolidated file in a different folder.
#' cache_daily_files(folder = "custom_data_folder", folder_save_to = "consolidated_data_folder")
#'
#' @import gauntlet
#' @import dplyr
#' @import readr
#' @import here
#' @importFrom glue str_glue
#' @importFrom lubridate as_date
#' @importFrom purrr map, reduce
#' @importFrom stringr gsub
#' @importFrom utils write.table
#'
#' @export
cache_daily_files = function(folder = "data", folder_save_to = "data"){
  files = list.files(here::here(folder)) %>%
    gsub(".*data_updates_|data_alerts_|data_vp_", "\\1", .) %>%
    gsub("_.*", "\\1", .) %>%
    unique() %>%
    .[!is.na(as.numeric(.))] %>%
    .[as_date(.)!=Sys.Date()]

  files %>%
    map(~{
      data_vp = gauntlet::read_rds_allFiles(specifically = str_glue("vp_{.x}"))

      rds_path = here::here(folder_save_to, str_glue("daily_cache_vp_{.x}.rds"))

      data_vp_comb = data_vp %>%
        reduce(bind_rows)

      readr::write_rds(data_vp_comb, rds_path)

      rds_save_ob = readr::read_rds(rds_path)

      check_row = nrow(rds_save_ob) == nrow(data_vp_comb)
      check_identical = identical(rds_save_ob, data_vp_comb)
    })
}

#' Convert VP Data to Spatial Format
#'
#' Converts a data.frame containing longitude and latitude coordinates into a spatial format suitable for mapping purposes.
#'
#' @param data A data.frame containing longitude and latitude columns.
#' @param crs_to A character string specifying the Coordinate Reference System (CRS) for the specific area where the data is located. The CRS should have meters as its base unit.
#' @return A spatial object of class 'sf' representing the converted data in the specified CRS.
#'
#' @details The `convert_vp_data_to_sf` function takes a data.frame with longitude and latitude columns and converts it into a spatial object using the 'sf' package. The resulting spatial object can be used for mapping and spatial analysis purposes.
#'
#' The function first converts the data.frame into an 'sf' object using the `sf::st_as_sf` function, specifying the longitude and latitude columns as the coordinate values and the CRS of the original data as EPSG code 4326 (WGS84). The `crs_to` parameter allows the user to specify the desired CRS for the converted data. It is important to choose a CRS appropriate for the specific area where the data is located and ensure that the CRS has meters as its base unit.
#'
#' The converted spatial object is then transformed to the specified CRS using the `sf::st_transform` function. The function also applies the `gauntlet::st_extract_coords` function to extract the route, vehicle, and trip identifiers from the data and groups the data by these identifiers.
#'
#' Additional computations are performed within each group using the `dplyr` package. These computations include calculating the differences in longitude and latitude between consecutive points, the total distance traveled (`ttl_diff`), and the average speed (`speed_avg`). Points with an average speed exceeding 100 meters per second are filtered out to remove any potential outliers. Finally, the data is transformed back to EPSG code 4326 to ensure compatibility with common mapping tools.
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
convert_vp_data_to_sf = function(data, crs_to){
  data %>%
    sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>%
    sf::st_transform(crs = crs_to) %>%
    gauntlet::st_extract_coords() %>%
    group_by(route_id, vehicle_id, trip_id) %>%
    mutate(lon_diff = lon-lag(lon),
           lat_diff = lat-lag(lat),
           ttl_diff = sqrt(lon_diff^2 + lat_diff^2),
           speed_avg = (ttl_diff/datetime_diff)*2.236936) %>%
    ungroup() %>%
    filter(speed_avg < 100) %>%
    sf::st_transform(4326)
}

#' Process VP List RDS Files
#'
#' Processes a list of RDS objects containing real-time GTFS feed queries saved throughout the day, extracting relevant information and performing data transformations.
#'
#' @param rds_list_objects A list of RDS objects representing the real-time GTFS feed queries saved throughout the day.
#' @return A data.frame containing the processed data with relevant information extracted and data transformations applied.
#'
#' @details The `process_vp_list_rds` function takes a list of RDS objects, each representing a real-time GTFS feed query saved at different points throughout the day. The function processes these objects to extract relevant information and apply necessary data transformations.
#'
#' The function starts by combining the RDS objects with their corresponding names using the `pmap` function from the `purrr` package. It extracts the query batch timestamp by removing the "data_vp_" prefix and the ".rds" extension from the names and converting the resulting character strings to datetime objects with the "US/Pacific" timezone.
#'
#' The processed data is then combined using the `reduce` function from the `purrr` package and renamed using the `rename_with` function. The renaming step replaces "vehicle.id" with "vehicle_id" in the column names and retains only the portion after the last dot in the remaining column names.
#'
#' The data is arranged by the query batch timestamp, route ID, vehicle ID, and trip ID using the `arrange` function from the `dplyr` package. It is then grouped by the route ID, vehicle ID, and trip ID using the `group_by` function.
#'
#' Additional computations are performed within each group using the `mutate` function. The `date_time` column is derived from the `timestamp` column by converting it to numeric format, then to datetime format using the `lubridate::as_datetime` function, and finally applying the "US/Pacific" timezone using the `lubridate::with_tz` function.
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
process_vp_list_rds = function(rds_list_objects){
  list(
    rds_list_objects
    ,names(rds_list_objects)
  ) %>%
    pmap(~{
      .x %>%
        mutate(query_batch = .y %>%
                 str_remove_all("data_vp_|\\.rds") %>%
                 as_datetime(tz = "US/Pacific"))
    }) %>%
    reduce(bind_rows) %>%
    rename_with(~.x %>%
                  str_replace(., "vehicle.id", "vehicle_id") %>%
                  gsub(".*\\.", "\\1", .)) %>%
    arrange(query_batch, route_id, vehicle_id, trip_id) %>%
    group_by(route_id, vehicle_id, trip_id) %>%
    mutate(date_time = timestamp %>%
             as.numeric() %>%
             lubridate::as_datetime() %>%
             lubridate::with_tz("US/Pacific")) %>%
    mutate(datetime_diff = as.numeric(date_time-lag(date_time))) %>%
    ungroup() %>%
    data.frame() %>%
    filter(datetime_diff != 0)
}


