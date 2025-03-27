#' Plot Individual ROC Curves for Multiple Predictors
#'
#' This function generates Receiver Operating Characteristic (ROC) curves for one or more predictors
#' in a dataset. It is designed to handle wider-format data, where each predictor is stored in a
#' separate column. The function can plot individual ROC curves for each predictor and optionally
#' display confidence intervals (CI) for the curves.
#'
#' @param data A data frame containing the response variable and predictor variables.
#' @param response_col The name of the column in `data` containing the binary response variable
#'   (e.g., 0/1 or "Case"/"Control"). If `NULL`, the user will be prompted to enter the column name.
#' @param themes A ggplot2 theme object to customize the appearance of the plot. Default is
#'   `ggthemes::theme_clean()`.
#' @param titles A named list of custom titles for each predictor. If `NULL`, the predictor names
#'   will be used as titles.
#' @param predictor A character vector specifying the names of the predictor columns to plot.
#'   If `NULL`, all columns except `response_col` will be used.
#' @param show_ci Logical. If `TRUE`, confidence intervals (CI) for the ROC curves will be displayed.
#'   Default is `FALSE`.
#' @param color The color of the ROC curve. Default is `"#0072B5"`.
#' @param color_text The color of the text annotations (AUC, CI, and p-value). Default is `"#0072B5"`.
#' @param line_size The thickness of the ROC curve line. Default is `0.66`.
#' @param title_size The font size of the plot title. Default is `14`.
#' @param digits The number of decimal places to round the AUC and p-value. Default is `3`.
#' @param alpha The transparency level of the confidence interval ribbon. Default is `0.15`.
#'
#' @returns
#' - If a single predictor is specified, a `ggplot` object representing the ROC curve is returned.
#' - If multiple predictors are specified, a list of `ggplot` objects is returned, where each element
#'   corresponds to the ROC curve of a predictor.
#'
#' @details
#' The function uses the `pROC` package to calculate ROC curves and AUC values. It also supports
#' adding confidence intervals to the ROC curves using `pROC::ci.se()`. The resulting plots are
#' customized using ggplot2 and can be further modified using the `themes` parameter.
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("example_data_wide")

#' # 1.Plot ROC curves for multiple predictors without confidence intervals
#' result <- plot_roc(example_data_wide, response_col = "Group")
#' patchwork::wrap_plots(result, nrow = 2)

#' # 2.Plot ROC curves for multiple predictors with confidence intervals
#' result <- plot_roc(example_data_wide,
#'    response_col = "Group",
#'    show_ci = TRUE)
#' patchwork::wrap_plots(result, nrow = 2)

#' # 3.Plot ROC curve for the selected predictors
#' result <- plot_roc(example_data_wide,
#'    response_col = "Group",
#'    predictor = c("Predictor1","Predictor2"))
#' patchwork::wrap_plots(result, nrow = 1)

#' # 4.Plot ROC curve for the selected predictors using customized theme
#' result <- plot_roc(example_data_wide,
#'    response_col = "Group",
#'    predictor = c("Predictor1","Predictor2"),
#'    themes = theme_base())
#' patchwork::wrap_plots(result, nrow = 1)
#'
plot_roc <- function(data, response_col = NULL, themes = ggthemes::theme_clean(), titles = NULL,
                     predictor = NULL, show_ci = FALSE, color = "#0072B5", color_text = "#0072B5", line_size = 0.68,
                     title_size = 14, digits = 3, alpha = 0.15) {

  # 如果未指定 response_col，则提示输入
  if (is.null(response_col)) {
    response_col <- readline("Please enter the name of the 'response' column: ")
  }

  # 选择预测变量
  if (is.null(predictor)) {
    predictor_cols <- setdiff(names(data), response_col)
  } else {
    predictor_cols <- predictor
  }

  # 显示正在分析的预测变量
  message("ROC curves are being plotted for the following predictors:\n")
  print(predictor_cols)

  # 统一定义图形主题
  theme_common <- themes +
    theme(
      axis.line.x = ggplot2::element_line(linewidth = 0.3),
      axis.line.y = ggplot2::element_line(linewidth = 0.3),
      plot.title = ggplot2::element_text(hjust = 0.5, vjust = 0.5, size = title_size, face = "plain"),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(colour = NA)
    )

  process_single_roc <- function(predictor_name) {
    # 使用手动加反引号来保护变量名
    formula_used <- as.formula(paste(response_col, "~", paste0("`", predictor_name, "`")))
    roc_obj <- pROC::roc(formula_used, levels = c(0, 1), direction = '<',  data = data)

    # 计算 CI 曲线数据
    data_ci <- NULL
    if (show_ci) {
      ciroc <- pROC::ci.se(roc_obj, specificities = seq(0, 1, 0.01))  # 计算特定灵敏度的 CI
      data_ci <- data.frame(
        x = 1 - as.numeric(rownames(ciroc)),  # 计算特异度（1 - 特异度）
        ymin = ciroc[, "2.5%"],                # 置信区间下界
        ymax = ciroc[, "97.5%"]                # 置信区间上界
      )
    }

    # 计算 AUC
    auc_value <- pROC::auc(roc_obj)

    # 计算 AUC 95% CI（仅用于文本）
    auc_ci <- pROC::ci.auc(roc_obj)
    ci_text <- paste0(
      "95% CI (",
      formatC(auc_ci[1], format = "f", digits = 2), "-",
      formatC(auc_ci[3], format = "f", digits = 2), ")"
    )

    # 计算 P 值
    p_value <- verification::roc.area(data[[response_col]], roc_obj$predictor)$p.value
    p_lab <- ifelse(p_value > 0.05,
                    paste0("italic(P) == ", format(round(p_value, digits), nsmall = digits)),
                    "italic(P) < 0.05")

    title_text <- ifelse(is.null(titles), predictor_name, titles[[predictor_name]])

    p <- pROC::ggroc(roc_obj, col = color, size = line_size, legacy.axes = TRUE) +
      theme_common +
      annotate("segment", x = 0, xend = 1, y = 0, yend = 1,
               col = "darkgrey", linetype = "dashed", linewidth = 0.4) +
      ggtitle(title_text) +
      annotate("text", x = 0.28, y = 0.21, color = color_text, hjust = 0,
               label = paste0("AUC = ", format(round(auc_value, digits), nsmall = digits))) +
      annotate("text", x = 0.28, y = 0.12, color = color_text, hjust = 0,
               label = ci_text) +
      annotate("text", x = 0.28, y = 0.03, color = color_text, hjust = 0,
               label = p_lab, parse = TRUE) +
      coord_fixed(ratio = 1.03)

    if (show_ci && !is.null(data_ci)) {
      p <- p + ggplot2::geom_ribbon(data = data_ci, aes(x = x, ymin = ymin, ymax = ymax), fill = color, alpha = alpha)
    }

    return(p)
  }

  if (length(predictor_cols) == 1) {
    return(process_single_roc(predictor_cols))
  } else {
    g.list <- lapply(predictor_cols, process_single_roc)
    names(g.list) <- predictor_cols
    return(g.list)
  }
}
