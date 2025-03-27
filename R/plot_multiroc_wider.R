#' Plot Multiple ROC Curves in a Single Plot for Wider-Format Data
#'
#' This function generates multiple Receiver Operating Characteristic (ROC) curves for a set of predictors
#' and plots them in a single graph, with each curve distinguished by a unique color. It is particularly
#' useful for comparing the performance of multiple predictors in a unified visualization.
#'
#' @param data A data frame containing the response variable and predictor variables.
#' @param response_col The name of the column in `data` containing the binary response variable
#'   (e.g., 0/1 or "Case"/"Control"). Default is `"Group"`.
#' @param predictors A character vector specifying the names of the predictor columns to plot.
#'   If `NULL`, all columns except `response_col` will be used.
#' @param auc_show Logical. If `TRUE`, the AUC value for each predictor will be displayed in the legend.
#'   Default is `FALSE`.
#' @param legend_pos The position of the legend. Can be `"right"` (default) or `"inside"` (bottom-right corner).
#' @param digits The number of decimal places to round the AUC value. Default is `3`.
#' @param line_size The thickness of the ROC curve lines. Default is `0.72`.
#' @param plot_theme A ggplot2 theme object to customize the appearance of the plot. Default is
#'   `ggthemes::theme_clean()`.
#' @param palette The color palette to use for the ROC curves. Options are `"jama"` (default) or `"nejm"`.
#'   If `NULL`, ggplot2's default colors will be used.
#' @param custom_colors A character vector of custom colors for the ROC curves. If provided, this will
#'   override the `palette` parameter.
#' @param smooth Logical. If `TRUE`, the ROC curves will be smoothed. Default is `FALSE`.
#'
#' @returns A `ggplot` object containing multiple ROC curves plotted in a single graph. The plot includes:
#' \itemize{
#'   \item ROC curves for each predictor, distinguished by unique colors.
#'   \item A diagonal reference line (random classifier).
#'   \item Customizable legend with optional AUC values.
#'   \item Customizable colors and themes.
#' }
#'
#' @details
#' Unlike `plot_roc`, which generates separate ROC curves for each predictor, this function plots all ROC
#' curves in a single graph, making it easier to compare the performance of multiple predictors. The function
#' uses the `pROC` package to calculate ROC curves and AUC values, and the resulting plot is generated using
#' `ggplot2`. Users can customize the appearance of the plot using the `plot_theme` and `custom_colors` parameters.
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("example_data_wide")
#'
#' # 1.Plot ROC curves for all predictors in a single graph
#' plot_multiroc_wider(example_data_wide, response_col = "Group")
#'
#' # 2.Plot ROC curves for all predictors in a single graph with AUC values displayed
#' plot_multiroc_wider(example_data_wide, response_col = "Group", auc_show = TRUE)
#'
#' # 3.Use custom colors and place the legend inside the plot
#' plot_multiroc_wider(example_data_wide,
#'    response_col = "Group",
#'    auc_show = TRUE,
#'    legend_pos = "inside")
#'
#' # 4.Plot ROC curves for the selected predictors with AUC displayed
#' plot_multiroc_wider(example_data_wide,
#'    response_col = "Group",
#'    predictors = c("Predictor1", "Predictor6"),
#'    auc_show = TRUE,
#'    legend_pos = "inside")

plot_multiroc_wider <- function(data, response_col = "Group", predictors = NULL, auc_show = FALSE, legend_pos = "right", digits = 3,
                                line_size = 0.72, plot_theme = ggthemes::theme_clean(), palette = "jama", custom_colors = NULL, smooth = FALSE) {

  if (!require(pROC)) { install.packages("pROC"); library(pROC) }
  if (!require(ggplot2)) { install.packages("ggplot2"); library(ggplot2) }
  if (!require(ggthemes)) { install.packages("ggthemes"); library(ggthemes) }

  # 选择特征列
  if (is.null(predictors)) {
    predictor_cols <- setdiff(names(data), response_col)
  } else {
    predictor_cols <- predictors
  }

  # 计算 ROC 和 AUC
  roc_list <- lapply(predictor_cols, function(model) {
    roc_obj <- pROC::roc(data[[response_col]], data[[model]], levels = c(0,1),
                         direction='<', smooth = smooth)
    list(model = model, roc = roc_obj, auc = pROC::auc(roc_obj))
  })

  # 构造命名的 ROC 对象列表，名称为模型名
  roc_obj_list <- lapply(roc_list, function(x) x$roc)
  names(roc_obj_list) <- sapply(roc_list, function(x) x$model)

  # 生成 AUC 标签：动态计算最长模型名称长度，保证左对齐
  max_model_length <- max(nchar(predictor_cols))
  if (auc_show) {
    model_labels <- sapply(roc_list, function(x) {
      sprintf(paste0("%-", max_model_length + 0, "s (%8s)"),
              x$model, paste0("AUC=", format(round(x$auc, digits), nsmall = digits)))
    })
  } else {
    model_labels <- sapply(roc_list, function(x) x$model)
  }
  names(model_labels) <- sapply(roc_list, function(x) x$model)

  # 颜色选择逻辑
  if (!is.null(custom_colors)) {
    color_vector <- rep(custom_colors, length.out = length(predictor_cols))
  } else if (palette == "jama") {
    color_vector <- c("#0072B2", "#E69F00", "#009E73", "#F0E442",
                      "#D55E00", "#CC79A7", "#56B4E9", "#999999",
                      "#C00000", "#FF8000", "#00B050", "#7030A0")
  } else if (palette == "nejm") {
    color_vector <- c("#3E4A89", "#B71C1C", "#1B5E20", "#F57F17",
                      "#4E342E", "#0D47A1", "#F44336", "#43A047",
                      "#8E24AA", "#3949AB", "#00ACC1", "#FF7043")
  } else {
    color_vector <- NULL  # 让 ggplot 自动选择颜色
  }

  # 使用 ggroc 绘制 ROC 曲线，传递命名列表，保证图例显示正确名称
  p <- pROC::ggroc(roc_obj_list, size = line_size, legacy.axes = TRUE) +
    ggplot2::annotate("segment", x = 0, xend = 1, y = 0, yend = 1, col = "darkgrey", linetype = "dashed", linewidth = 0.4) +
    plot_theme +
    theme(
      legend.position = if (legend_pos == "inside") c(0.95, 0.02) else "right",
      legend.justification = if (legend_pos == "inside") c(1, 0) else NULL,
      legend.background = ggplot2::element_rect(fill = alpha("white", 0.8), colour = "black"),
      legend.text = ggplot2::element_text(family = "mono"), #mono
      #axis.line.x = element_line(linewidth = 0.3),
      #axis.line.y = element_line(linewidth = 0.3),
      plot.title = ggplot2::element_text(hjust = 0.5, vjust = 0.5, size = 14, face = "plain"),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(colour = NA)
    ) +
    ggplot2::labs(title = "ROC Curves", x = "1 - Specificity", y = "Sensitivity", color = "Predictors") +
    ggplot2::coord_fixed(ratio = 1.04)

  # 应用颜色和 AUC 显示逻辑
  if (!is.null(color_vector)) {
    p <- p + ggplot2::scale_color_manual(values = color_vector,
                                         labels = if (auc_show) model_labels else names(roc_obj_list),
                                         breaks = predictor_cols)
  } else {
    p <- p + ggplot2::scale_color_discrete(labels = if (auc_show) model_labels else names(roc_obj_list),
                                           breaks = predictor_cols)
  }

  return(p)
}
