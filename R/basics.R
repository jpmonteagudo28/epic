#> Proportion of participants with a risk factor or disease at
#> a particular point in time
prevalence <- function(exposed, total) {

  stopifnot(is.numeric(exposed),
            is.numeric(total))

  if(any(total <= 0)){
    stop("Total must be greater than zero.")
  }

  if(any(exposed > total)){
    stop("Exposed count cannot be greater than the total population.")
  }

  prevalence <- exposed / total

  return(prevalence)
}

#' @title
#' Calculate Person-Time Based on Exposure Status
#'
#' @description
#' This function computes the total person-time for exposed and unexposed individuals based on
#' the time until disease onset or follow-up time if no onset occurs. It categorizes individuals
#' according to their exposure status and sums their person-time accordingly.
#'
#' @param data A data frame or matrix containing patient/individual data. Must include columns
#'              specified by `exposure_col` and `onset_col`.
#' @param follow_up A numeric value representing the study follow-up time.
#' @param exposure_col A character string indicating the name of the column that specifies
#'                     exposure status ("Yes" or "No" for character type, or 1 for exposed
#'                     in numeric type).
#' @param onset_col A character string indicating the name of the column that contains the time
#'                   until disease onset for each individual.
#' @param onset_class A character string indicating the data type of the `onset_col`. Should
#'                    be either "char" for character strings or "numeric" for numeric values.
#'                    Defaults to "char".
#' @param use_na use NA values to compute person time. NA values treated as right-censored and
#'               assign total follow-up time.
#'
#' @return A tibble with total person-time for both exposed and unexposed individuals.
#'         The names are "Exposed" and "Unexposed".
#' @export
#'
#' @examples
#' # Sample data
#' flopatitis_data <- data.frame(
#'   Deadline_Pressure = c("Yes", "No", "Yes", "No"),
#'   Disease_Onset = c(3, 12, 5, 8)
#' )
#'
#' # Calculate person-time with follow-up of 12
#' result <- person_time(flopatitis_data, 12, "Deadline_Pressure", "Disease_Onset",
#'                       onset_class = "char")
#' print(result)  # Should show total person-time for Exposed and Unexposed
#'
person_time <- function(data,
                        follow_up,
                        exposure_col,
                        onset_col,
                        onset_class = "char",
                        use_na = FALSE) {

  # Validate the onset_class argument
  onset_class <- match.arg(onset_class, c("char", "numeric"))

  # Ensure the input is a data frame or matrix
  stopifnot(is.data.frame(data) || is.matrix(data),
            is.numeric(follow_up),
            is.character(exposure_col),
            is.character(onset_col))

  # Validate column names
  col_names <- colnames(data)
  if (!(exposure_col %in% col_names && onset_col %in% col_names)) {
    stop("The exposure or onset column name does not exist in the data frame")
  }

  # Calculate person time using vectorized operations
  if (onset_class == "char") {
    is_exposed <- data[[exposure_col]] == "Yes"
  } else {
    is_exposed <- data[[exposure_col]] == 1
  }

  # Calculate person time for exposed and unexposed
  onset_time <- data[[onset_col]]

  # Treat NA in onset_time as having full follow-up time
  if(use_na){
    onset_time[is.na(onset_time)] <- follow_up
  }

  person_time_exposed <- sum(pmin(follow_up, onset_time[is_exposed]), na.rm = TRUE)
  person_time_unexposed <- sum(pmin(follow_up, onset_time[!is_exposed]), na.rm = TRUE)

  person_time <- dplyr::tibble(Exposed = person_time_exposed,
                               Unexposed = person_time_unexposed)

  # Return the result as a tibble
  return(person_time)
}


#> Likelihood of developing the disease over a specified time period.
incidence <- function(exposed,person_time){

  if(is.vector(person_time)){
    person_time <- sum(person_time)
  }

  stopifnot(is.numeric(exposed),
            is.numeric(person_time))

  if(any(person_time <= 0)){
    stop("Person-time must be greater than zero.")
  }

  if(any(exposed > person_time)){
    stop("Exposed count cannot be greater than the total person-time.")
  }

  incidence_rate <- exposed/person_time

  return(incidence_rate)
}

#>the ratio of the number of new cases to the total number
#>of participants free of the disease at the outset.
cum_incidence <- function(exposed, total_base){

  stopifnot(is.numeric(exposed),
            is.numeric(total_base))

  if(any(total_base <= 0)){
    stop("Total must be greater than zero.")
  }

  if(any(exposed > total_base)){
    stop("Exposed count cannot be greater than the total population at baseline.")
  }

  cum_incidence <- exposed/total_base

  return(cum_incidence)
}


#> Population Attributable risk quantifies the association
#> between a risk factor and the prevalence or incidence of disease
pop_attributable_risk <- function(total,unexposed){

  par <- (total - unexposed)/total

  return(par)
}

## Risk ratio - a measure to compare prevalence or incidence
#> of disease between two groups
relative_risk <- function(exposed,unexposed, total){

  PP_exposed <- prevalence(exposed,total)
  PP_unexposed <- 1 - PP_exposed
  risk <- PP_exposed/PP_unexposed

  return(risk)
}


#> Case-control studies use OR instead of RR
#> OR are invariant, unlike RR
odds_ratio <- function(exposed,unexposed,total_base){

  ci_exposed <- cum_incidence(exposed,total_base)
  ci_unexposed <- cum_incidence(unexposed,total_base)

  or <- (ci_exposed/(1 - ci_exposed))/(ci_unexposed/(1 - ci_unexposed))

  return(or)
}

#> Risk difference is also called excess risk, measures the absolute effect
#> of the exposure or risk factor on prevalence or incidence.
risk_diff <- function(exposed, unexposed, total){

  PP_exposed <- prevalence(exposed,total)
  PP_unexposed <- 1 - PP_exposed

  risk_diff <- PP_exposed - PP_unexposed

  return(risk_diff)
}
