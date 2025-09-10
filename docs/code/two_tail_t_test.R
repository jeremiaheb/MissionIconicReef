#' @title Perform a Two-Sample T-Test Using Confidence Intervals
#'
#' @description This function compares the means of two groups based on pre-calculated
#' statistics (mean, variance, sample size). It determines if the difference is
#' statistically significant by checking for overlap in their confidence intervals.
#'
#' @param dataframe A data frame containing summary statistics for two groups (two rows).
#' @param metric A character string specifying the column name for the sample means.
#' @param alpha The significance level (e.g., 0.05 for a 95% confidence level).
#' @param return_dataframe A logical value. If TRUE, the function returns the
#'   input dataframe with CI calculations appended. If FALSE, it returns a
#'   character string with the test result.
#'
#' @return Either a data frame or a character string, depending on `return_dataframe`.
#'
#' @examples
#' # Create sample data for two groups
#' sample_data <- data.frame(
#'   group = c("A", "B"),
#'   density = c(10.5, 12.8), # The metric (mean)
#'   var = c(0.8, 1.1),     # Variance of the sample mean
#'   n = c(50, 55),         # Sample size
#'   nm = c(105, 105),        # Total size (assuming this is part of the df calc)
#'   strat_num = c(5, 5),   # Number of strata (for df calc)
#'   STAGE_LEVEL = c(1, 2)  # Stage level (for df calc)
#' )
#'
#' # Run the test and print the result message
#' two_tail_t_test(sample_data, "density", 0.05)
#'
#' # Run the test and get the dataframe with CI columns
#' result_df <- two_tail_t_test(sample_data, "density", 0.05, return_dataframe = TRUE)
#' print(result_df)

two_tail_t_test <- function(dataframe, metric, alpha, return_dataframe = FALSE) {

  # Define the columns that MUST be in the dataframe.
  must_have <- c(metric, "var", "n", "nm", "strat_num", "STAGE_LEVEL")

  if (!all(must_have %in% colnames(dataframe))) {
    stop("One or more required columns are missing from the dataframe.")
  }

  if (nrow(dataframe) != 2) {
    stop("This function requires a dataframe with exactly two rows.")
  }

  # Calculate degrees of freedom (df)
  dataframe$df <- ifelse(dataframe$STAGE_LEVEL == 1,
                         dataframe$n - dataframe$strat_num,
                         dataframe$nm - dataframe$n - dataframe$strat_num)

  # Calculate the critical t-value.
  dataframe$t_value <- abs(qt(alpha / 2, dataframe$df))

  std_error <- sqrt(dataframe$var)
  margin_of_error <- dataframe$t_value * std_error

  dataframe$LCI <- dataframe[[metric]] - margin_of_error
  dataframe$UCI <- dataframe[[metric]] + margin_of_error

  # Check if the mean of one group falls within the confidence interval of the other.
  mean1_in_interval2 <- (dataframe[[metric]][1] >= dataframe$LCI[2]) && (dataframe[[metric]][1] <= dataframe$UCI[2])
  mean2_in_interval1 <- (dataframe[[metric]][2] >= dataframe$LCI[1]) && (dataframe[[metric]][2] <= dataframe$UCI[1])

  # If either condition is true, they are not significantly different.
  is_not_significant <- mean1_in_interval2 || mean2_in_interval1

  if (return_dataframe) {
    return(dataframe)
  } else {
    if (is_not_significant) {
      result_message <- paste(metric, "is NOT statistically significant at alpha =", alpha)
    } else {
      result_message <- paste(metric, "is statistically significant (p <", alpha, ")")
    }
    return(result_message)
  }
}

#' @title Convenience wrapper for the t-test
#'
#' @description A wrapper to call two_tail_t_test with a fixed metric and alpha.
#' Useful for applying the test over a list of dataframes.
#' @param df A data frame formatted for two_tail_t_test.
#' @param ... Additional arguments passed on to two_tail_t_test (e.g., return_dataframe)
#' @return The output from two_tail_t_test.
apply_ttest_dens <- function(df, ...) {
  two_tail_t_test(df, "density", 0.05, ...)
}

apply_ttest_occ <- function(df, ...) {
  two_tail_t_test(df, "occurrence", 0.05, ...)
}
