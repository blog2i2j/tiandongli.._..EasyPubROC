#' Plot Combined ROC Curves from a List of ROC Objects
#'
#' This function accepts a named list of ROC objects (obtained using the \code{pROC} package)
#' and generates a single \code{ggplot} object displaying all ROC curves. Each ROC curve is
#' distinguished by a unique color, and the legend labels can optionally include the AUC values.
#'
#' @param roc_list A named list of ROC objects (as returned by \code{pROC::roc()}). The names of the list
#'   elements will be used as predictor labels in the legend.
#' @param auc_show Logical. If \code{TRUE}, the legend labels will include the AUC value for each ROC curve,
#'   formatted to the specified number of digits. Default is \code{FALSE}.
#' @param legend_pos A character string specifying the position of the legend. Options are \code{"right"} (default)
#'   or \code{"inside"} (legend placed inside the plot).
#' @param digits An integer indicating the number of decimal places to round the AUC values. Default is \code{3}.
#' @param line_size A numeric value specifying the thickness of the ROC curve lines. Default is \code{0.72}.
#' @param plot_theme A \code{ggplot2} theme object used to customize the appearance of the plot. Default is
#'   \code{ggthemes::theme_clean()}.
#' @param palette A character string specifying the color palette to use for the ROC curves. Options are
#'   \code{"jama"} (default) or \code{"nejm"}. If \code{NULL}, the default \code{ggplot2} colors are used.
#' @param custom_colors A character vector of custom colors to override the \code{palette} parameter. If provided,
#'   these colors will be used for the ROC curves.
#' @param smooth Logical. If \code{TRUE}, the ROC curves will be smoothed. Default is \code{FALSE}.
#'
#' @returns A \code{ggplot} object containing the combined ROC curves.
#'
#' @details
#' This function is useful for comparing the performance of multiple predictors in a single plot.
#' It uses the \code{ggroc()} function from the \code{pROC} package to generate the ROC curves and
#' customizes the plot appearance using \code{ggplot2}. The legend labels can include the AUC values if
#' \code{auc_show = TRUE}.
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("example_data_wide")
#' # Assume roc1 and roc2 are ROC objects computed using pROC::roc()
#' roc1 <- pROC::roc(example_data_wide$Group, example_data_wide$Predictor1)
#' roc2 <- pROC::roc(example_data_wide$Group, example_data_wide$Predictor4)
#' roc_list <- list("SVM" = roc1, "RF" = roc2)
#' plot_multiroc_combined(roc_list,
#'    auc_show = TRUE,
#'    legend_pos = "inside",
#'    digits = 2)

plot_multiroc_combined <- function(roc_list, auc_show = FALSE, legend_pos  = "right", digits = 3, line_size = 0.72,
                                   plot_theme = ggthemes::theme_clean(), palette = "jama", custom_colors = NULL) {

  # 1) 检查 roc_list 是否是一个已命名列表
  if (!is.list(roc_list) || is.null(names(roc_list))) {
    stop("`roc_list` It must be a list with a name,shuh as list(\"model1\"=roc1, \"model2\"=roc2).")
  }

  # 2) 计算每条曲线的 AUC 值
  #    并根据 auc_show 决定图例中是否附带 AUC
  auc_vals <- sapply(roc_list, pROC::auc)
  model_names <- names(roc_list)

  if (auc_show) {
    # 生成带 AUC 的标签
    max_name_length <- max(nchar(model_names))
    model_labels <- sprintf(
      paste0("%-", max_name_length, "s (AUC=%.", digits, "f)"),
      model_names, auc_vals
    )
  } else {
    # 仅显示模型名称
    model_labels <- model_names
  }

  names(model_labels) <- model_names

  # 3) 选择颜色方案
  if (!is.null(custom_colors)) {
    color_vector <- rep(custom_colors, length.out = length(model_names))
  } else if (palette == "jama") {
    color_vector <- c("#0072B2", "#E69F00", "#009E73", "#F0E442",
                      "#D55E00", "#CC79A7", "#56B4E9", "#999999",
                      "#C00000", "#FF8000", "#00B050", "#7030A0")
  } else if (palette == "nejm") {
    color_vector <- c("#3E4A89", "#B71C1C", "#1B5E20", "#F57F17",
                      "#4E342E", "#0D47A1", "#F44336", "#43A047",
                      "#8E24AA", "#3949AB", "#00ACC1", "#FF7043")
  } else {
    color_vector <- NULL
  }

  # 4) 使用 ggroc() 绘制多条 ROC 曲线
  p <- pROC::ggroc(roc_list, size = line_size, legacy.axes = TRUE) +
    ggplot2::annotate("segment", x = 0, xend = 1, y = 0, yend = 1, col = "darkgrey", linetype = "dashed", linewidth = 0.4) +
    plot_theme +
    ggplot2::theme(
      legend.position = if (legend_pos == "inside") c(0.95, 0.02) else "right",
      legend.justification = if (legend_pos == "inside") c(1, 0) else NULL,
      legend.background = ggplot2::element_rect(fill = ggplot2::alpha("white", 0.8), colour = "black"),
      legend.text = ggplot2::element_text(family = "mono"),
      plot.title = ggplot2::element_text(hjust = 0.5, vjust = 0.5, size = 14, face = "plain"),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(colour = NA)
    ) +
    ggplot2::labs(title = "ROC Curves", x = "1 - Specificity", y = "Sensitivity", color = "Predictors") +
    ggplot2::coord_fixed(ratio = 1.04)

  # 5) 应用颜色方案和图例标签
  if (!is.null(color_vector)) {
    p <- p + ggplot2::scale_color_manual(values = color_vector, labels = model_labels, breaks = model_names)
  } else {
    p <- p + ggplot2::scale_color_discrete(labels = model_labels, breaks = model_names)
  }

  return(p)
}
