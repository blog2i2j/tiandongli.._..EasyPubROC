#' Perform ROC Analysis on Wider-Format Data
#'
#' This function calculates Receiver Operating Characteristic (ROC) curves, Area Under the Curve (AUC),
#' sensitivity, specificity, and other related metrics for wider-format data. In wider-format data,
#' each predictor is stored in a separate column, and the function evaluates each predictor independently.
#'
#' @param data A data frame containing the response variable and predictor variables.
#' @param response_col The name of the column in `data` containing the binary response variable (e.g., 0/1 or "Case"/"Control").
#' @param youdex Logical. If `TRUE`, the Youden index is calculated and included in the output. Default is `TRUE`.
#' @param threshold A numeric value specifying a fixed threshold for sensitivity and specificity calculations. Default is `NULL`.
#' @param sensitivity_threshold A numeric value specifying the minimum sensitivity threshold. Default is `NULL`.
#' @param specificity_threshold A numeric value specifying the minimum specificity threshold. Default is `NULL`.
#' @param digits An integer specifying the number of decimal places to round the results. Default is `3`.
#'
#' @returns A data frame with evaluation metrics, such as AUC, sensitivity, and specificity, calculated for each predictor:
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("example_data_wide")
#' # Perform ROC analysis
#' # 1.calculate the evaluation metrics using the Youdex index
#' result <- calc_auc_wider(data = example_data_wide, response_col = "Group")
#' View(result)
#'
#' # 2.calculate the evaluation metrics using the cutoff of the threshold
#' result <- calc_auc_wider(data = example_data_wide, response_col = "Group",threshold = 0.3)
#' View(result)
#'
#' # 3.calculate the evaluation metrics using the cutoff of the sensitivity(%)
#' result <- calc_auc_wider(data = example_data_wide, response_col = "Group", sensitivity_threshold = 60)
#' View(result)
#'
#' # 4.calculate the evaluation metrics using the cutoff of  the specificity(%)
#' result <- calc_auc_wider(data = example_data_wide, response_col = "Group", specificity_threshold = 90)
#' View(result)

calc_auc_wider <- function(data, response_col = NULL, youdex = TRUE, threshold = NULL,
                           sensitivity_threshold = NULL, specificity_threshold = NULL, digits = 3) {
  # Load packages
  if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("verification", quietly = TRUE)) install.packages("verification")
  if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")

  library(pROC)
  library(dplyr)
  library(tibble)
  library(verification)

  ## Set response column
  if (is.null(response_col)) {
    response_col <- readline("Please enter the colnames of the <<response>>: ")
  }

  # 提取响应列并检查是否为二分类
  response <- data[[response_col]]

  # 检查是否为数值型或因子
  if (!is.numeric(response)) {
    # 尝试转换为因子
    response_factor <- as.factor(response)
    unique_levels <- levels(response_factor)

    # 确保只有两个类别
    if (length(unique_levels) != 2) {
      stop("Response variable must have exactly 2 categories. Found: ", length(unique_levels))
    }

    # 转换为数值型 (0/1)
    data[[response_col]] <- as.numeric(response_factor) - 1  # 例如 "A"/"B" -> 0/1
    message("Converted response variable to 0/1: ",
            paste(unique_levels, collapse = " -> 0, "), " -> 1")
  } else {
    # 检查数值型是否为0/1
    unique_values <- unique(response)
    if (!all(unique_values %in% c(0, 1))) {
      stop("Numeric response variable must be 0/1. Found values: ",
           paste(unique_values, collapse = ", "))
    }
  }


  ## Helper function to calculate sensitivity and specificity
  calculate_senspe <- function(rocobj) {
    if (!is.null(threshold)) {
      senspe_table <- pROC::coords(rocobj, x = threshold, ret = "all", transpose = FALSE) %>%
        dplyr::filter(precision == max(precision) | precision == "NaN")
    } else if (!is.null(sensitivity_threshold)) {
      senspe_table <- pROC::coords(rocobj, "all", ret = "all", transpose = FALSE) %>%
        dplyr::filter(sensitivity >= sensitivity_threshold) %>%
        dplyr::filter(youden == max(youden)) %>%
        dplyr::filter(precision == max(precision) | precision == "NaN")
    } else if (!is.null(specificity_threshold)) {
      senspe_table <- pROC::coords(rocobj, "all", ret = "all", transpose = FALSE) %>%
        dplyr::filter(specificity >= specificity_threshold) %>%
        dplyr::filter(youden == max(youden)) %>%
        dplyr::filter(precision == max(precision) | precision == "NaN")
    } else {
      senspe_table <- pROC::coords(rocobj, "best", ret = "all", transpose = FALSE, best.method = "youden") %>%
        dplyr::filter(precision == max(precision) | precision == "NaN")
    }

    senspe_table$youden <- (senspe_table$youden - 100) / 100
    return(senspe_table)
  }

  ## Helper function: determine significance level based on p-value
  get_significance <- function(p_value, digits = 3) {
    if (p_value > 0.05) {
      "P > 0.05"
    } else if (p_value <= 0.05 & p_value > 0.01) {
      "P < 0.05"
    } else if (p_value <= 0.01 & p_value > 0.001) {
      "P < 0.01"
    } else if (p_value <= 0.001 & p_value > 0.0001) {
      "P < 0.001"
    } else {
      "P < 0.0001"
    }
  }

  ## Main processing
  if ((ncol(data) - 1) == 1) {  # Single predictor case
    rocobj <- pROC::roc(as.formula(paste(response_col, "~ .")), data = data, percent = TRUE, levels = c(0, 1), direction = '<')
    senspe_table <- calculate_senspe(rocobj)
    rownames(senspe_table) <- setdiff(names(data), response_col)

    ## Calculate AUC and p-value
    auc_value <- format(round(pROC::auc(rocobj) / 100, digits), nsmall = digits)
    ci_lower <- format(round(pROC::ci.auc(rocobj)[1] / 100, digits), nsmall = digits)
    ci_upper <- format(round(pROC::ci.auc(rocobj)[3] / 100, digits), nsmall = digits)
    ci_value <- paste0("(", ci_lower, "-", ci_upper, ")")
    p_value <- format(verification::roc.area(data[[response_col]], rocobj$predictor)$p.value, scientific = FALSE, digits = 5)

    significance <- get_significance(as.numeric(p_value), digits)
    auc_table <- setNames(data.frame(auc_value, paste0(auc_value, ci_value), p_value, significance),
                          c("AUC", "AUC(95% CI)", "pvalue", "Significance"))
    rownames(auc_table) <- setdiff(names(data), response_col)

    # Merge tables
    if (identical(rownames(senspe_table), rownames(auc_table))) {
      aucsenspe_table <- merge(auc_table, senspe_table, by = "row.names") %>%
        tibble::column_to_rownames("Row.names")
      return(aucsenspe_table)
    } else {
      stop("Error: Row names of auc_table and senspe_table are not identical")
    }

  } else {
    # Multiple predictors case
    roc.list <- pROC::roc(as.formula(paste(response_col, "~ .")), data = data, percent = TRUE, levels = c(0, 1), direction = '<')
    senspe_table <- lapply(roc.list, calculate_senspe) %>% do.call(rbind, .)

    auc_table <- lapply(roc.list, function(rocobj) {
      auc_value <- format(round(pROC::auc(rocobj) / 100, digits), nsmall = digits)
      ci_lower <- format(round(pROC::ci.auc(rocobj)[1] / 100, digits), nsmall = digits)
      ci_upper <- format(round(pROC::ci.auc(rocobj)[3] / 100, digits), nsmall = digits)
      ci_value <- paste0("(", ci_lower, "-", ci_upper, ")")
      p_value <- format(verification::roc.area(data[[response_col]], rocobj$predictor)$p.value, scientific = FALSE, digits = 5)

      significance <- get_significance(as.numeric(p_value), digits)
      return(setNames(data.frame(auc_value, paste0(auc_value, ci_value), p_value, significance),
                      c("AUC", "AUC(95% CI)", "pvalue", "Significance")))
    }) %>% do.call(rbind, .)

    if (identical(rownames(senspe_table), rownames(auc_table))) {
      aucsenspe_table <- merge(auc_table, senspe_table, by = "row.names") %>%
        tibble::column_to_rownames("Row.names")
      return(aucsenspe_table)
    } else {
      stop("Error: Row names of auc_table and senspe_table are not identical")
    }
  }
}
