#' Perform ROC Analysis on Long-Format Data
#'
#' This function calculates Receiver Operating Characteristic (ROC) curves, Area Under the Curve (AUC),
#' sensitivity, specificity, and other related metrics for long-format data. It is particularly useful
#' for datasets where multiple predictors are stored in a single column, and their corresponding values
#' are stored in another column.
#'
#' @param data A data frame containing the response variable, predictor variable, and value column.
#' @param response_col  The name of the column in `data` containing the binary response variable (e.g., 0/1 or "Case"/"Control").
#' @param predictor_col The name of the column in `data` containing the predictor variable names (e.g., gene names or biomarker names).
#' @param value_col The name of the column in `data` containing the predictor values (e.g., expression levels or scores).
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
#' data("example_data_long")
#' table(example_data_long$ID,example_data_long$Group)
#'
#' # Perform ROC analysis
#' # 1.calculate the evaluation metrics using the Youdex index
#' result <- calc_auc_longer(data = example_data_long,
#'    response_col = "Group",
#'    predictor_col = "ID",
#'    value_col = "Value")
#' View(result)
#'
#' # 2.calculate the evaluation metrics using the cutoff of the threshold
#' result <- calc_auc_longer(data = example_data_long,
#'    response_col = "Group",
#'    predictor_col = "ID",
#'    value_col = "Value",
#'    threshold = 0.3,)
#' View(result)
#'
#' ## 3.calculate the evaluation metrics using the cutoff of the sensitivity(%)
#' result <- calc_auc_longer(data = example_data_long,
#'    response_col = "Group",
#'    predictor_col = "ID",
#'    value_col = "Value",
#'    sensitivity_threshold = 60)
#' View(result)
#'
#' ## 4.uisng the cutoff of  the specificity(%)
#' result <- calc_auc_longer(data = example_data_long,
#'    response_col = "Group",
#'    predictor_col = "ID",
#'    value_col = "Value",
#'    specificity_threshold = 90)
#' View(result)

calc_auc_longer <- function(data, response_col, predictor_col, value_col, youdex = TRUE, threshold = NULL,
                            sensitivity_threshold = NULL, specificity_threshold = NULL, digits = 3) {
  # 加载必要包
  if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("verification", quietly = TRUE)) install.packages("verification")
  if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")

  library(pROC)
  library(dplyr)
  library(verification)
  library(tibble)

  # 将响应变量转换为因子，并检查水平数
  data[[response_col]] <- as.factor(data[[response_col]])
  if (length(levels(data[[response_col]])) != 2) {
    stop("Response column must have exactly 2 levels.")
  }

  ## 辅助函数：计算敏感度、特异度、最佳阈值、Youden 等指标
  calculate_senspe <- function(rocobj) {
    if (!is.null(threshold)) {
      tab <- pROC::coords(rocobj, x = threshold, ret = "all", transpose = FALSE)
    } else if (!is.null(sensitivity_threshold)) {
      tab <- pROC::coords(rocobj, "all", ret = "all", transpose = FALSE) %>%
        filter(sensitivity >= sensitivity_threshold) %>%
        filter(youden == max(youden))
    } else if (!is.null(specificity_threshold)) {
      tab <- pROC::coords(rocobj, "all", ret = "all", transpose = FALSE) %>%
        filter(specificity >= specificity_threshold) %>%
        filter(youden == max(youden))
    } else {
      tab <- pROC::coords(rocobj, "best", ret = "all", transpose = FALSE, best.method = "youden") %>%
        filter(precision == max(precision) | precision == "NaN")
    }
    if (youdex && "youden" %in% names(tab)) {
      tab$youden <- (tab$youden - 100) / 100
    }
    return(tab)
  }

  ## 辅助函数：格式化数值
  format_val <- function(x, digits) {
    formatC(round(x, digits), format = "f", digits = digits)
  }

  ## 辅助函数：根据 p 值返回显著性描述
  get_significance <- function(p_value) {
    case_when(
      p_value > 0.05 ~ "P > 0.05",
      p_value <= 0.05 & p_value > 0.01 ~ "P < 0.05",
      p_value <= 0.01 & p_value > 0.001 ~ "P < 0.01",
      p_value <= 0.001 & p_value > 0.0001 ~ "P < 0.001",
      TRUE ~ "P < 0.0001"
    )
  }

  ## 主体：按 predictor_col 分组计算各指标的 ROC 及统计指标
  results_list <- data %>%
    group_by(!!sym(predictor_col)) %>%
    group_modify(~ {
      sub_data <- .

      # 检查当前组是否有两个类别
      if (length(unique(sub_data[[response_col]])) != 2) {
        stop(paste("Data for", unique(sub_data[[predictor_col]]), "does not have two classes."))
      }

      # 构造 ROC 对象（percent = TRUE，计算的 AUC 为百分比值）
      roc_obj <- pROC::roc(
        response  = sub_data[[response_col]],
        predictor = sub_data[[value_col]],
        percent = TRUE,
        levels    = levels(sub_data[[response_col]]),
        direction = "<"
      )

      # 计算敏感度、特异度等指标，取最佳行（基于 Youden 指数）
      senspe_tab <- calculate_senspe(roc_obj)
      best_idx <- which.max(senspe_tab$youden)
      best_row <- senspe_tab[best_idx, , drop = FALSE]

      # 计算 AUC
      auc_val <- pROC::auc(roc_obj)

      # 计算 AUC 95% CI（严格保留两位小数），注意 auc 与 ci.auc 均为百分比值
      ci_val <- pROC::ci.auc(roc_obj)
      ci_text <- paste0("(", formatC(ci_val[1] / 100, format = "f", digits = digits), "-",
                        formatC(ci_val[3] / 100, format = "f", digits = digits), ")")

      # 计算 p 值（转换响应变量为数值）
      p_val <- verification::roc.area(as.numeric(as.character(sub_data[[response_col]])), roc_obj$predictor)$p.value

      significance <- get_significance(p_val)

      # 构造 AUC 表，并保留 predictor_col 值
      auc_table <- tibble::tibble(
        !!predictor_col := unique(sub_data[[predictor_col]]),
        AUC = format_val(auc_val / 100, digits),
        `AUC(95% CI)` = paste0(format_val(auc_val / 100, digits), " ", ci_text),
        pvalue = format(p_val, scientific = FALSE, digits = 5),
        Significance = significance
      )

      # 合并 AUC 表和 best_row（senspe指标）
      final_table <- dplyr::bind_cols(auc_table, best_row) %>% as.data.frame()

      # 将 predictor_col 列设置为行名，然后删除该列
      rownames(final_table) <- final_table[[predictor_col]]
      final_table[[predictor_col]] <- NULL
      final_table
    }) %>% ungroup()

  # 将 results_list 转换为 data.frame，并设置行名为 predictor_col 值
  final_results <- as.data.frame(results_list)
  if (predictor_col %in% colnames(final_results)) {
    rownames(final_results) <- final_results[[predictor_col]]
    final_results[[predictor_col]] <- NULL
  }

  return(final_results)
}
