#' Crosswalk Override
#'
#' Overrides the shape_id values in a crosswalk object with specified overrides.
#'
#' @param xwalk_ob A crosswalk object (data.frame) containing the shape_id column.
#' @param over_ride A character vector or single string specifying the shape_id override value(s).
#' @return A modified crosswalk object with the shape_id values overridden.
#'
#' @details The `xwalk_override` function takes a crosswalk object (`xwalk_ob`) and overrides the shape_id values with the specified override values (`over_ride`). If `over_ride` contains a single string value, all rows in the crosswalk object will be updated with that shape_id value. If `over_ride` is a character vector with multiple values, the shape_id values will be selectively overridden based on matching patterns.
#'
#' Within the function, if the length of `over_ride` is 1, the `xwalk_ob` object is mutated to replace all shape_id values with the single override value. If the length of `over_ride` is greater than 1, the `xwalk_ob` object is mutated using the `case_when` function from the `dplyr` package. The `shape_id` column is updated based on matching patterns with the first and second elements of `over_ride`.
#'
#' This function provides a convenient way to override shape_id values in a crosswalk object, allowing for customized mapping and analysis based on specific needs.
#'
#' @references The `dplyr` package is used in this function for data manipulation. Please refer to its documentation for further details.
#'
#' @examples
#' # Override shape_id values in a crosswalk object
#' overridden_xwalk <- xwalk_override(xwalk_ob, over_ride = "new_shape_id")
#'
#' @import dplyr
#'
#' @export
xwalk_override = function(xwalk_ob, over_ride) {
  message("Overriding shape_id values in crosswalk object...")

  if (length(over_ride) == 1) {
    modified_xwalk <- xwalk_ob %>%
      mutate(shape_id = over_ride)
  } else {
    modified_xwalk <- xwalk_ob %>%
      mutate(shape_id = case_when(
        gsub(":.*", "\\1", shape_id) == gsub(":.*", "\\1", over_ride[1]) ~ over_ride[1],
        gsub(":.*", "\\1", shape_id) == gsub(":.*", "\\1", over_ride[2]) ~ over_ride[2]
      ))
  }

  message("Shape_id values overridden.")

  return(modified_xwalk)
}
