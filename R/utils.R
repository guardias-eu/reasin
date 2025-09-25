#' Clean up names
#'
#' This script removes leading and trailing single quotes from some columns
#' in the input dataframe.
#' @param df A dataframe containing a `Name` column.
#' @param cols A character vector specifying the columns to clean.
#' @return The modified dataframe with cleaned `Name` column.
#' @noRd
#' @examples
#' df <- data.frame(Name = c("'Species A'", "'Species B", "Species C'"))
#' clean_up_names(df)
clean_up_names <- function(df, cols) {
  # Check that all cols are present
  if (!all(cols %in% colnames(df))) {
    missing_cols <- cols[!cols %in% colnames(df)]
    cli::cli_abort(
      "The following columns are missing in the dataframe: {paste(missing_cols, collapse = ', ')}",
      class = "reasin_error_assignment_invalid"
    )
  }
  df <- df %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(cols),
      ~ gsub("^'|'$", "", .x)
    )
  )
  return(df)
}
