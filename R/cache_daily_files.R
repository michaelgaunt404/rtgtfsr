' Cache Daily Files
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
    gsub(".*data_updates_|data_alerts_|data_vp_"
         , "\\1", .) %>%
    gsub("_.*", "\\1", .) %>%
    unique() %>%
    .[!is.na(as.numeric(.))] %>%
    .[as_date(.)!=Sys.Date()]

  files %>%
    map(~{
      data_vp = gauntlet::read_rds_allFiles(specifically = str_glue("vp_{.x}"))
      rds_path = here::here(folder_save_to, str_glue("daily_cache_vp_{.x}.rds"))

      data_vp_comb = data_vp %>% reduce(bind_rows)
      readr::write_rds(data_vp_comb, rds_path)

      rds_save_ob = readr::read_rds(rds_path)
      check_row = nrow(rds_save_ob) == nrow(data_vp_comb)
      check_identical = identical(rds_save_ob, data_vp_comb)
    })
}
