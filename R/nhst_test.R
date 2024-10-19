# Assumptions:
# Independence, normality and homogeneity of variance of residuals
# Continuous response

anova <- function(formula,
                  data,
                  ...,
                  type = c("one_way",
                           "two_way",
                           "n_way",
                           "factorial",
                           "repeated")
) {


  stopifnot(is.data.frame(data) || is.matrix(data))

  if (!inherits(formula, "formula")) {
    stop("The input must be a valid formula of the form y ~ x + a")
  }

  type <- match.arg(type)

  response <- all.vars(formula)[1]
  predictors <- all.vars(formula)[-1]

  # Check if response and factors exist in the data
  if (!response %in% names(data)) {
    stop(paste("Response variable", response, "not found in the data"))
  }

  for (pred in predictors) {
    if (!pred %in% names(data)) {
      stop(paste("Factor", pred, "not found in the data"))
    }
  }
  out <- switch(type,
                one_way = stats::oneway.test(formula, data, ...),
                two_way = stats::aov(formula, data, ...),
                n_way = stats::aov(formula,data, ...),
                factorial = stats::aov(formula, data, ...,
                                       contrasts = c('contr.sum', 'contr.poly')
                ),
                repeated = nlme::lme(formula,...,data)
  )

  if (inherits(out, "aov")) {
    aov_summary <- summary.aov(out)
  } else if (inherits(out, "htest")) {
    aov_summary <- out
  } else if (inherits(out, "lme")) {
    aov_summary <- summary(out)
  } else {
    stop("Unsupported model type")
  }

  return(aov_summary)

}

chi_square_test <- function(x,y = NULL,
                            prob = "uniform",
                            boot_p_value = FALSE,
                            n_samples = 1000,
                            adjust_p = TRUE){

  stopifnot(is.logical(boot_p_value))

  n <- length(x)


  if (prob == "uniform") {

    prob <- rep(1/n, n)
  } else {

    # If p doesn't sum to 1, you'll get an error
    stopifnot(length(prob) == n,
              is.numeric(prob),
              sum(prob) == 1)
  }

  out <- chisq.test(x,y,args,
                    p = prob,
                    simulate.p.value = boot_p_value,
                    B = n_samples, # 1000 resamples will set min. p at 0.001
                    correct = adjust_p
  )

  out <- data.frame(
    "X-squared" = out$statistic,
    "Df" = out$parameter,
    "p-value" = out$p.value
  )

  return(out)
}

proportions_test <- function(successes,totals,
                             bounds = c("two.sided","less","greater"),
                             conf.level = 0.95){

  bounds <- match.arg(bounds)

  stopifnot(length(successes) == length(totals),
            length(successes) >= 2,
            is.numeric(successes),
            is.numeric(totals),
            all(successes <= totals))

  result <- prop.test(x = successes,
                      n = totals,
                      alternative = bounds,
                      conf.level = conf.level)

  result <- data.frame(
    X_squared = result$statistic,
    Df = result$parameter,
    Proportions = paste(result$estimate, collapse = ", "),
    CI = paste0(
      "(", round(result$conf.int[1],digits = 3),
      ", ", round(result$conf.int[2], digits = 3), ")"
    ),
    p_value = result$p.value,
    row.names = NULL
  )

  return(result)
}





