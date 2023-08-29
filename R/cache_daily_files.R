#' Cache Daily Vehicle Position Files
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
#' cache_daily_vp_files(folder = "data", folder_save_to = "cache", timezone = "US/Pacific")
#'
#' @import dplyr
#' @import lubridate
#' @import purrr
#' @import stringr
#' @import here
#' @import gauntlet
#' @import readr
#'
#' @export
cache_daily_vp_files = function(folder = "data"
                                ,folder_save_to = "data"
                                ,timezone = "US/Pacific"){

  daily_vp_dates = list.files(here::here(folder)) %>%
    .[str_detect(., "data_vp")] %>%
    gsub(".*data_vp_", "\\1", .) %>%
    gsub("_.*", "\\1", .) %>%
    unique() %>%
    .[as_date(.)!=Sys.Date()]

  daily_vp_dates %>%
    map(~{
      data_vp = gauntlet::read_rds_allFiles(
        data_location = folder, specifically = str_glue("vp_{.x}"))
      rds_path = here::here(folder_save_to, str_glue("daily_cache_vp_{.x}.rds"))

      data_vp_comb = list(data_vp, names(data_vp)) %>%
        pmap(~{
          .x %>%
            mutate(query_batch = .y %>%
                     str_remove_all("\\.rds") %>%
                     gsub(".*vp_", "\\1", .) %>%
                     lubridate::as_datetime(tz = timezone))
        }) %>%
        reduce(bind_rows)

      readr::write_rds(data_vp_comb, rds_path)
      rds_save_ob = readr::read_rds(rds_path)
      check_row = nrow(rds_save_ob) == nrow(data_vp_comb)
      check_identical = identical(rds_save_ob, data_vp_comb)
    })
}
