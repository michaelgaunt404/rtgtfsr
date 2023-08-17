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









