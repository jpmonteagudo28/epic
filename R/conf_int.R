#' @title
#' Confidence interval for proportion $\pi$
#'
#' @description
#' Confidence intervals for the population proportion $\pi$ for a large *n* and
#' the population mean $\mu$ under certain conditions.
#'
#' @details
#' When *n* is sufficiently large and *p* is not too small, the sample proportion *p* has
#' an approximately Gaussian distribution. In the long run, if repeated samples were taken,
#' the sample proportion would vary around the proportion $\pi$ in the target population.
#'
#' There are three available methods for computing the binomial proportion confidence interval.
#' - **wald**: Relies on approximating the distribution of error about a binomially-distributed
#'             observation. Typically used when *n* is sufficiently large and *p* is not too small.
#' - **exact**: This method uses the binomial distribution to calculate the coverage probability, which
#' is never less than $1- \alpha$. See [here](https://stats.stackexchange.com/questions/87775/clopper-pearson-for-non-mathematicians) for a simple explanation
#' - **wilson**: Asymetric interval with continuity correction typically employed with small samples and skewed observations.
#'              The observed coverage probability is closer to $1- \alpha$ than the Wald method.
#'
#' @param p: sample proportion
#' @param n: sample size
#' @param conf.level: confidence level between 0 and 1. Defaults to .95
#' @param method: the method chosen for computing the confidence interval
#'                for proportions. Defaults to `wald`
#' @param bounds option to choose two-sided confidence intervals, lower or upper bounds. Defaults to `two-sided`
#'
#'
#' @examples
#' ci_prop(.45,100)
#'

diagnostic_metrics <- function(true_positives,
                               true_negatives,
                               false_positives,
                               false_negatives,
                               conf_level = 0.95,
                               digits = 2) {

  # Input validation
  stopifnot(is.numeric(true_positives),
            is.numeric(true_negatives),
            is.numeric(false_positives),
            is.numeric(false_negatives),
            is.numeric(digits),
            true_positives >= 0,
            true_negatives >= 0,
            false_positives >= 0,
            false_negatives >= 0)

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package \"tibble\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # Total counts
  total <- true_positives + true_negatives + false_positives + false_negatives
  total_positives <- true_positives + false_negatives
  total_negatives <- true_negatives + false_positives

  # Basic metrics
  sensitivity <- true_positives / total_positives
  specificity <- true_negatives / total_negatives

  ppv <- true_positives / (true_positives + false_positives)
  npv <- true_negatives / (true_negatives + false_negatives)

  accuracy <- (true_positives + true_negatives) / total
  prevalence <- total_positives / total

  # Likelihood ratios
  positive_lr <- sensitivity / (1 - specificity)
  negative_lr <- (1 - sensitivity) / specificity

  # F1 Score
  precision <- ppv  # Precision is the same as PPV
  recall <- sensitivity
  f1_score <- 2 * (precision * recall) / (precision + recall)

  # Youden's J Statistic
  youdens_j <- sensitivity + specificity - 1

  # Diagnostic Odds Ratio
  dor <- (true_positives * true_negatives) / (false_positives * false_negatives)

  # Confidence Intervals (using Wilson score interval for proportions)
  wilson_ci <- function(x, n, conf_level) {
    z <- qnorm((1 + conf_level) / 2)
    p <- x / n
    lower <- (p + z^2/(2*n) - z*sqrt((p*(1-p) + z^2/(4*n))/n)) / (1 + z^2/n)
    upper <- (p + z^2/(2*n) + z*sqrt((p*(1-p) + z^2/(4*n))/n)) / (1 + z^2/n)
    paste0("[", round(lower, digits = digits), ", ", round(upper, digits = digits), "]")
  }

  sensitivity_ci <- wilson_ci(true_positives, total_positives, conf_level)
  specificity_ci <- wilson_ci(true_negatives, total_negatives, conf_level)
  ppv_ci <- wilson_ci(true_positives, true_positives + false_positives, conf_level)
  npv_ci <- wilson_ci(true_negatives, true_negatives + false_negatives, conf_level)
  accuracy_ci <- wilson_ci(true_positives + true_negatives, total, conf_level)
  prevalence_ci <- wilson_ci(total_positives, total, conf_level)

  tibble::tibble(
    Metric = c("Sensitivity", "Specificity",
               "PPV", "NPV", "Accuracy",
               "Prevalence", "Positive LR",
               "Negative LR", "F1 Score",
               "Youden's J", "Diagnostic Odds Ratio"),
    Value = round(
      c(sensitivity, specificity, ppv, npv, accuracy,
        prevalence, positive_lr, negative_lr, f1_score,
        youdens_j, dor),
      digits = digits),
    CI = c(sensitivity_ci, specificity_ci, ppv_ci, npv_ci,
           accuracy_ci, prevalence_ci,
           rep(NA_character_, 5))
  ) |>
    tibble::column_to_rownames("Metric")
}

ci_mean_diff <- function(mean1, mean2,
                         sd1, sd2,
                         n1, n2,
                         conf.level = 0.95) {

  # Calculate variances
  var1 <- sd1^2
  var2 <- sd2^2

  # Calculate standard error
  se <- sqrt((var1 / n1) + (var2 / n2))

  # Calculate degrees of freedom using Welch–Satterthwaite equation
  df <- (var1 / n1 + var2 / n2)^2 /
    ((var1 / n1)^2 / (n1 - 1) + (var2 / n2)^2 / (n2 - 1))


  t_value <- qt(
    (1 + conf.level) / 2, df = df
  )

  mean_diff <- mean1 - mean2
  lower <- mean_diff - t_value * se
  upper <- mean_diff + t_value * se

  return(
    dplyr::tibble(
      statistic = mean_diff,
      lower = lower,
      upper = upper)
  )
}

##--------------------------------------------##
ci_prop_diff <- function(p1,
                         p2,
                         n1,
                         n2,
                         conf.level = 0.95,
                         method = "wald",
                         bounds = "two-sided") {

  # Ensure proportions are valid
  if (p1 > 1 || p1 < 0 || p2 > 1 || p2 < 0) {
    stop("Proportions 'p1' and 'p2' must be between 0 and 1.")
  }

  method <- match.arg(method, c("wald", "wilson","exact"),
                      several.ok = FALSE)
  bounds <- match.arg(bounds, c("two-sided", "less", "greater"),
                      several.ok = FALSE)

  alpha <- 1 - conf.level
  z <- qnorm((1 + conf.level) / 2)


  prop_diff <- p1 - p2


  if (method == "wald") {

    # Pooled standard error
    pooled_se <- sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))

    lower <- prop_diff - z * pooled_se
    upper <- prop_diff + z * pooled_se
  }

  # Wilson method
  if (method == "wilson") {

    z <- qnorm(1 - alpha / 2)

    # Wilson score correction for each proportion
    denominator1 <- 1 + z^2 / n1
    denominator2 <- 1 + z^2 / n2

    center1 <- (p1 + z^2 / (2 * n1)) / denominator1
    center2 <- (p2 + z^2 / (2 * n2)) / denominator2

    margin1 <- (z / denominator1) * sqrt(p1 * (1 - p1) / n1 + z^2 / (4 * n1^2))
    margin2 <- (z / denominator2) * sqrt(p2 * (1 - p2) / n2 + z^2 / (4 * n2^2))

    lower1 <- center1 - margin1
    upper1 <- center1 + margin1

    lower2 <- center2 - margin2
    upper2 <- center2 + margin2

    lower <- lower1 - upper2
    upper <- upper1 - lower2
  }

  # Ensure the interval bounds are within [-1, 1] (difference in proportions range)
  lower <- max(-1, lower)
  upper <- min(1, upper)

  # Return based on the 'bounds' argument
  if (bounds == "less") {

    return(dplyr::tibble(
      statistic = prop_diff,
      lower = lower))

  } else if (bounds == "greater") {

    return(dplyr::tibble(
      statistic = prop_diff,
      upper = upper))

  } else {

    return(dplyr::tibble(
      statistic = prop_diff,
      lower = lower,
      upper = upper))
  }
}

##------------------------------------------------##
ci_relative_risk <- function(cases_exposed,
                             total_exposed,
                             cases_unexposed,
                             total_unexposed,
                             conf_level = 0.95,
                             digits = 2) {

  # Calculate relative risk (RR)
  rr <- relative_risk(cases_exposed,
                      total_exposed,
                      cases_unexposed,
                      total_unexposed)

  # Standard error of log(RR)
  se_log_rr <- sqrt((1 / cases_exposed) - (1 / total_exposed) +
                      (1 / cases_unexposed) - (1 / total_unexposed))

  # Z-value for the given confidence level
  z <- qnorm((1 + conf_level) / 2)

  # Confidence interval on the log scale
  log_rr <- log(rr)
  ci_lower_log <- log_rr - z * se_log_rr
  ci_upper_log <- log_rr + z * se_log_rr

  # Convert back to the original scale
  ci_lower <- round(exp(ci_lower_log), digits = digits)
  ci_upper <- round(exp(ci_upper_log), digits = digits)

  # Return a tibble with the RR and the confidence interval
  tibble::tibble(
    statistic = round(rr,digits = digits),
    lower =  ci_lower,
    upper =  ci_upper
  )
}
##--------------------------------------------------##
ci_odds_ratio <- function(exposed,
                          exposed_noncases,
                          unexposed,
                          unexposed_noncases,
                          conf_level = 0.95,
                          digits = 3) {

  or <- odds_ratio(exposed,
                   exposed_noncases,
                   unexposed,
                   unexposed_noncases)

  # Log of the odds ratio
  log_or <- log(or)

  # Standard error for log(OR)
  se_log_or <- sqrt(1 / exposed + 1 / exposed_noncases + 1 / unexposed + 1 / unexposed_noncases)

  # Z-score for the confidence level
  z <- qnorm((1 + conf_level) / 2)

  # Confidence interval on the log scale
  lower_log_ci <- log_or - z * se_log_or
  upper_log_ci <- log_or + z * se_log_or

  # Convert back from log scale to get the confidence interval for OR
  lower_ci <- exp(lower_log_ci)
  upper_ci <- exp(upper_log_ci)

  # Return a tibble with rounded confidence interval
  tibble::tibble(
    statistic = or,
    lower = round(lower_ci, digits),
    upper = round(upper_ci, digits)
  )
}

##----------------------------------------------------#
ci_prop <- function(p,
                    n,
                    conf.level = 0.95,
                    method = "wald",
                    bounds = "two-sided") {

  if (p > 1 || p < 0) {
    stop("Number of successes or proportion 'p' must be between 0 and 1.")
  }

  method <- match.arg(method,
                      c("wald","exact","wilson"),
                      several.ok = FALSE)

  bounds <- match.arg(bounds,
                      c("two-sided", "lower", "upper"),
                      several.ok = FALSE)

  alpha <- 1 - conf.level

  if(method == "wald"){

    z <- qnorm((1 + conf.level) / 2)
    se <- sqrt(p * (1 - p) / n)

    lower <- p - z * se
    upper <- p + z * se
  }

  if(method == "exact"){

    # Calculate number of successes
    x <- round(p * n)

    # Use the Clopper-Pearson method for binomial proportions
    if(x == 0){

      # Special case when x = 0
      lower <- 0
      upper <- 1 - (alpha / 2)^(1 / n)

    } else if(x == n) {

      # Special case when x = n
      lower <- (alpha / 2)^(1 / n)
      upper <- 1
    } else {

      # Regular case when 0 < x < n
      lower <- qbeta(alpha / 2, x, n - x + 1)
      upper <- qbeta(1 - alpha / 2, x + 1, n - x)
    }
  }

  if(method == "wilson"){

    z <- qnorm(1 - alpha / 2)


    # Wilson score correction components
    denominator <- 1 + z^2 / n
    center <- (p + z^2 / (2 * n)) / denominator
    margin <- (z / denominator) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))

    # Lower and upper bounds
    lower <- center - margin
    upper <- center + margin
  }

  # Ensure the interval bounds are within [0, 1]
  lower <- max(0, lower)
  upper <- min(1, upper)

  if (bounds == "lower") {

    return(dplyr::tibble(
      statistic = p,
      lower = lower)
    )

  } else if (bounds == "upper") {

    return(dplyr::tibble(
      statistic = p,
      upper = upper)
    )

  } else {

    return(dplyr::tibble(
      statistic = p,
      lower = lower,
      upper = upper)
    )
  }
}

#----------------------------------------#
ci_mean <- function(mean,
                    sd,
                    n,
                    conf.level = 0.95) {

  t_value <- qt((1 + conf.level) / 2, df = n - 1)
  se <- sd / sqrt(n)

  lower <- mean - t_value * se
  upper <- mean + t_value * se

  return(
    dplyr::tibble(
      statistic = mean,
      lower = lower,
      upper = upper)
  )
}

#------------------------------------------------------#
ci_boot <- function(data,
                    statistic_func,
                    R = 1000,
                    conf.level = 0.95) {

  boot_res <- boot::boot(data, statistic_func, R = R)
  ci <- boot::boot.ci(boot_res,
                      type = "perc",
                      conf = conf.level)

  lower <- ci$percent[4]
  upper <- ci$percent[5]

  # compute statistic on original data
  stat <- statistic_func(data, 1:length(data))

  return(
    dplyr::tibble(
    statistic = stat,
                lower = lower,
                upper = upper)
         )
}

# Example usage: Bootstrapping confidence interval for the mean
data <- rnorm(100, mean = 5, sd = 2)
statistic_func <- function(data, indices) {
  return(mean(data[indices]))  # bootstrapping the mean
}

ci_boot(data, statistic_func)


