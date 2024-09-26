#' @title
#' Randomize a vector of values
#'
#' @description
#' This function randomizes the ordering and placement of values in a given vector,
#' useful for creating synthetic data. It serves as a wrapper around the `base::sample`
#' function.
#'
#' @param x A numeric vector. The values to be randomized.
#' @param seed An integer. The seed for reproducibility of the randomization. Defaults to NULL.
#' @param replace A logical. Should sampling be done with replacement? Defaults to FALSE.
#' @param prob A numeric vector of probability weights for sampling. Should be the same length as `x`. Defaults to NULL.
#'
#' @return A vector of the same length as `x` with values randomized.
#'
#' @details
#' This function leverages `base::sample` to shuffle or randomly reorder the elements in `x`.
#' Setting the seed ensures that the randomization is reproducible. The `replace` parameter controls
#' whether sampling is done with or without replacement, and `prob` allows for weighted random sampling.
#'
#' @examples
#' # Basic usage
#' set.seed(123)
#' randomize(c(1, 2, 3, 4), seed = 123)
#'
#' # Sampling with replacement
#' randomize(c(1, 2, 3, 4), seed = 456, replace = TRUE)
#'
#' # Weighted sampling
#' randomize(c(1, 2, 3, 4), seed = 789, prob = c(0.1, 0.2, 0.3, 0.4))
#'
#' @seealso [base::sample()]
#' @keywords internal
#'
randomize <- function(x,
                      seed = NULL,
                      replace = FALSE,
                      prob = NULL){

  stopifnot(is.numeric(seed))

  set.seed(seed)
  len <- length(x)
  resampled_object <- sample(x,len,replace = replace,prob = prob)

  return(resampled_object)
}

