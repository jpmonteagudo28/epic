#' @description
#' Make n-way contingency tables of categorical or factor variables
#'
#' @details
#' A wrapper around `table()` that allows the user to specify
#' a data frame and select columns to display as a table.
#'
#' @param data a non-empty data frame with either categorical or factor variables
#' @param ... additional arguments to be passed to function
#'
#' @examples
#' df <- data.frame(
#'  Gender = rep(c("Male", "Female", "Male", "Female", "Male"),each = 1000),
#'  Smoker = rep(c("Yes", "Yes", "No", "No", "No"),each = 1000),
#'  Age_group = rep(c("Adult", "Child", "Adult", "Adult", "Child"), each = 1000)
#' )
#' make_nway_table(df,"Gender","Smoker")

make_nway_table <- function(data, ...){

  UseMethod("make_nway_table")

}
make_nway_table.data.frame <- function(data, ...) {
  # Extract column names passed in `...`
  variables <- unlist(list(...))

  # Check that the specified variables exist in the data frame
  if (!all(variables %in% names(data))) {
    stop("Some variables are not present in the data frame.")
  }

  # Generate the table
  subset_data <- data[, variables, drop = FALSE]
  tab <- table(subset_data)

  return(tab)
}

# Method for matrices
make_nway_table.matrix <- function(data,
                                   ...,
                                   as_data_frame = FALSE) {

  if (as_data_frame) {
    # Convert the matrix to a data frame
    data <- as.data.frame(data)
  }

  # If no variables are passed, apply `table` directly on the matrix
  if (missing(...)) {
    tab <- table(data)
    return(tab)
  }

  # Otherwise, extract column indices or names passed in `...`
  variables <- unlist(list(...))

  # Check if variables are numeric and valid column indices
  if (!all(is.numeric(variables)) || any(variables < 1) || any(variables > ncol(data))) {
    stop("Some column indices are out of bounds.")
  }

  # Generate the table (assuming columns)
  subset_data <- data[, as.numeric(variables), drop = FALSE]
  tab <- table(subset_data)
  return(tab)
}

make_nway_table.list <- function(data, ...) {
  # Extract elements to use for tabulation
  variables <- unlist(list(...))

  # Check if each variable is present in the list
  if (!all(variables %in% names(data))) {
    stop("Some variables are not present in the list.")
  }

  # Create a data frame from the list
  combined_df <- data[variables]

  # Generate the table
  tab <- table(do.call(data.frame, combined_df))

  return(tab)
}

# Default method for unsupported data types
make_nway_table.default <- function(data, ...) {
  stop("Unsupported data type. Please provide a data frame, matrix, or list.")
}
