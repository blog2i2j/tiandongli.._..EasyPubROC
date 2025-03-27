#'
#' Compare ROC Curves Between Two Datasets for Multiple Predictors
#'
#' This function compares the ROC curves of multiple predictors between two datasets by performing a statistical test
#' (default is the DeLong test). It calculates the ROC curves for each predictor in both data sets and returns a data frame
#' with the corresponding p-values, indicating whether there is a significant difference between the ROC curves.
#'
#' @param data_one A data frame representing the first dataset (e.g., training set) containing the binary response variable
#' @param data_two A data frame representing the second dataset (e.g., validation set) with the same structure as \code{data_one}.
#' @param response_col A character string specifying the name of the binary response column in both datasets (e.g., "Group").
#' @param method A character string specifying the method for the ROC comparison test. Options are \code{"delong"}, \code{"bootstrap"},
#'   or \code{"venkatraman"}. The first letter is sufficient. Default is \code{"delong"}.
#'
#' @returns A data frame containing two columns: \code{Model} (the predictor name) and \code{P_Value} (the p-value of the ROC test).
#'
#' @details
#' The function first checks that both datasets are data frames and contain the specified response column. It then ensures that
#' the column names of the two datasets match. For each predictor (i.e., each column in \code{data_one} except the response column),
#' the function calculates ROC curves for both datasets using the \code{pROC::roc()} function and performs a ROC comparison test
#' using \code{pROC::roc.test()}. The test method can be specified via the \code{method} parameter.
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("data_one")
#' data("data_two")
#' result <- multiroc_test(data_one = data_one,
#'    data_two = data_two,
#'    response_col = "Group")
#' View(result)

multiroc_test <- function(data_one, data_two, response_col, method = "delong") {
  # Ensure inputs are data frames
  if (!is.data.frame(data_one) | !is.data.frame(data_two)) {
    stop("Input data must be data frames.")
  }

  # Check if the response column exists in both datasets
  if (!(response_col %in% colnames(data_one)) | !(response_col %in% colnames(data_two))) {
    stop("The specified response column does not exist in the datasets.")
  }

  # Check if both datasets have the same column names
  if (!identical(sort(colnames(data_one)), sort(colnames(data_two)))) {
    stop("Column names in both datasets do not match. Please check the data.")
  }

  # Extract model names (excluding the response column)
  predictors <- setdiff(colnames(data_one), response_col)

  # Print model names to be tested
  cat("Performing", method, "test for the following models:\n")
  cat(predictors)

  # Compute p-values using the selected method
  p_values <- lapply(predictors, function(predictor) {
    roc_one <- pROC::roc(data_one[[response_col]], data_one[[predictor]])
    roc_two <- pROC::roc(data_two[[response_col]], data_two[[predictor]])
    roc_test_result <- pROC::roc.test(roc_one, roc_two, method = method)
    return(roc_test_result$p.value)
  })

  # Compile results into a data frame
  results <- data.frame(Predictor = predictors, PValue = unlist(p_values))
  return(results)
}
